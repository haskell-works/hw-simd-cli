{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.Cut
  ( cmdCut
  ) where

import App.Char
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Char                                 (ord)
import Data.Generics.Product.Any
import Data.List                                 (intersperse)
import HaskellWorks.Data.RankSelect.Base.Rank1
import HaskellWorks.Data.RankSelect.Base.Select1
import HaskellWorks.Data.Vector.AsVector64s
import Options.Applicative                       hiding (columns)

import qualified App.Commands.Options.Type               as Z
import qualified App.IO                                  as IO
import qualified Data.ByteString.Builder                 as B
import qualified Data.ByteString.Lazy                    as LBS
import qualified Data.Vector                             as DV
import qualified HaskellWorks.Data.Simd.Comparison.Avx2  as AVX2
import qualified HaskellWorks.Data.Simd.Comparison.Stock as STOCK
import qualified HaskellWorks.Simd.Cli.Comparison        as SERIAL
import qualified HaskellWorks.Simd.Cli.CutCursor         as Z
import qualified System.Exit                             as IO
import qualified System.IO                               as IO

{- HLINT ignore "Redundant do" -}

runCut :: Z.CutOptions -> IO ()
runCut opts = do
  let !delimiter  = opts ^. the @"delimiter"

  bs <- IO.readInputFile (opts ^. the @"inputFile")
  let wNewline = fromIntegral $ ord '\n'
  let !outDelimiterBuilder = B.word8 (opts ^. the @"outDelimiter")

  cursorResult <- case opts ^. the @"method" of
    "stock" -> do
      let v = asVector64s 64 bs
      return $ Right Z.CutCursor
        { Z.text      = bs
        , Z.markers   = STOCK.cmpEqWord8s delimiter v
        , Z.newlines  = STOCK.cmpEqWord8s wNewline  v
        , Z.position  = 1
        }
    "avx2" -> do
      let v = asVector64s 64 bs
      return $ Right Z.CutCursor
        { Z.text      = bs
        , Z.markers   = AVX2.cmpEqWord8s delimiter v
        , Z.newlines  = AVX2.cmpEqWord8s wNewline  v
        , Z.position  = 1
        }
    "serial" -> do
      IO.writeOutputFile (opts ^. the @"outputFile")
        $ SERIAL.cmpEqWord8s delimiter bs
      return $ Right Z.CutCursor
        { Z.text      = bs
        , Z.markers   = asVector64s 64 $ SERIAL.cmpEqWord8s delimiter bs
        , Z.newlines  = asVector64s 64 $ SERIAL.cmpEqWord8s wNewline  bs
        , Z.position  = 1
        }
    m -> do
      return $ Left $ "Unsupported method: " <> m

  case cursorResult of
    Right cursor -> do
      let rows = toListVector cursor

      runResourceT $ do
        (_, hOut) <- IO.openOutputFile (opts ^. the @"outputFile") Nothing
        forM_ rows $ \row -> do
          let fieldStrings = columnToFieldString row <$> (opts ^. the @"fields")

          liftIO $ B.hPutBuilder hOut $ mconcat (intersperse outDelimiterBuilder fieldStrings) <> B.word8 10

          return ()
      return ()

    Left msg -> do
      IO.hPutStrLn IO.stderr msg
      IO.exitFailure

  return ()
  where columnToFieldString :: DV.Vector LBS.ByteString -> Int -> B.Builder
        columnToFieldString fields i = if i >= 0 && i < DV.length fields
          then B.lazyByteString (DV.unsafeIndex fields i)
          else B.lazyByteString LBS.empty

optsCut :: Parser Z.CutOptions
optsCut = Z.CutOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input Text file"
        <>  metavar "STRING"
        )
  <*> option readWord8
        (   long "input-delimiter"
        <>  short 'd'
        <>  help "Text delimiter"
        <>  metavar "CHAR"
        )
  <*> strOption
        (   long "output"
        <>  short 'o'
        <>  help "Output Text file"
        <>  metavar "STRING"
        )
  <*> option readWord8
        (   long "output-delimiter"
        <>  short 'e'
        <>  help "Text delimiter"
        <>  metavar "CHAR"
        )
  <*> many
      ( option auto
        (   long "fields"
        <>  short 'f'
        <>  help "Fields"
        <>  metavar "INT"
        )
      )
  <*> strOption
        (   long "method"
        <>  short 'm'
        <>  help "Comparison method"
        <>  metavar "STRING"
        )

cmdCut :: Mod CommandFields (IO ())
cmdCut = command "cut"  $ flip info idm $ runCut <$> optsCut

atEnd :: Z.CutCursor -> Bool
atEnd c = LBS.null (LBS.drop (fromIntegral (Z.position c)) (Z.text c))
{-# INLINE atEnd #-}

toListVector :: Z.CutCursor -> [DV.Vector LBS.ByteString]
toListVector c = if Z.position d > Z.position c && not (atEnd c)
  then getRowBetween c d dEnd:toListVector (trim d)
  else []
  where nr = nextRow c
        d = nextPosition nr
        dEnd = atEnd nr
{-# INLINE toListVector #-}

getRowBetween :: Z.CutCursor -> Z.CutCursor -> Bool -> DV.Vector LBS.ByteString
getRowBetween c d dEnd = DV.unfoldrN fields go c
  where cr  = rank1 (Z.markers c) (Z.position c)
        dr  = rank1 (Z.markers d) (Z.position d)
        c2d = fromIntegral (dr - cr)
        fields = if dEnd then c2d +1 else c2d
        go :: Z.CutCursor -> Maybe (LBS.ByteString, Z.CutCursor)
        go e = case nextField e of
          f -> case nextPosition f of
            g -> case snippet e of
              s -> Just (s, g)
        {-# INLINE go #-}
{-# INLINE getRowBetween #-}

snippet :: Z.CutCursor -> LBS.ByteString
snippet c = LBS.take (len `max` 0) $ LBS.drop posC $ Z.text c
  where d = nextField c
        posC = fromIntegral $ Z.position c
        posD = fromIntegral $ Z.position d
        len  = posD - posC
{-# INLINE snippet #-}

nextField :: Z.CutCursor -> Z.CutCursor
nextField cursor = cursor
  { Z.position = newPos
  }
  where currentRank = rank1   (Z.markers cursor) (Z.position cursor)
        newPos      = select1 (Z.markers cursor) (currentRank + 1) - 1
{-# INLINE nextField #-}

trim :: Z.CutCursor -> Z.CutCursor
trim c = if Z.position c >= 512
  then trim c
    { Z.text      = LBS.drop 512 (Z.text c)
    , Z.markers   = drop 1 (Z.markers c)
    , Z.newlines  = drop 1 (Z.newlines c)
    , Z.position  = Z.position c - 512
    }
  else c
{-# INLINE trim #-}

nextPosition :: Z.CutCursor -> Z.CutCursor
nextPosition cursor = cursor
    { Z.position = if LBS.null (LBS.drop (fromIntegral newPos) (Z.text cursor))
        then fromIntegral (LBS.length (Z.text cursor))
        else newPos
    }
  where newPos  = Z.position cursor + 1
{-# INLINE nextPosition #-}

nextRow :: Z.CutCursor -> Z.CutCursor
nextRow cursor = cursor
  { Z.position = if newPos > Z.position cursor
                          then newPos
                          else fromIntegral (LBS.length (Z.text cursor))

  }
  where currentRank = rank1   (Z.newlines cursor) (Z.position cursor)
        newPos      = select1 (Z.newlines cursor) (currentRank + 1) - 1
{-# INLINE nextRow #-}
