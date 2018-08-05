{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.Cut
  ( cmdCut
  ) where

import App.Char
import Control.Lens
import Data.Char                            (ord)
import Data.Generics.Product.Any
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.Vector.AsVector64s
import Options.Applicative                  hiding (columns)

import qualified App.Commands.Options.Type               as Z
import qualified App.IO                                  as IO
import qualified Data.ByteString.Lazy                    as LBS
import qualified Data.Vector                             as DV
import qualified HaskellWorks.Data.Simd.Comparison.Avx2  as AVX2
import qualified HaskellWorks.Data.Simd.Comparison.Stock as STOCK
import qualified HaskellWorks.Simd.Cli.Comparison        as SERIAL
import qualified HaskellWorks.Simd.Cli.CutCursor         as Z
import qualified System.Exit                             as IO
import qualified System.IO                               as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runCut :: Z.CutOptions -> IO ()
runCut opts = do
  let !delimiter  = opts ^. the @"delimiter"

  bs <- IO.readInputFile (opts ^. the @"inputFile")
  let wNewline = fromIntegral $ ord '\n'

  cursorResult <- case opts ^. the @"method" of
    "stock" -> do
      return $ Right Z.CutCursor
        { Z.text        = bs
        , Z.delimiters  = STOCK.cmpEqWord8s delimiter $ asVector64s 64 bs
        , Z.newlines    = STOCK.cmpEqWord8s wNewline  $ asVector64s 64 bs
        }
    "avx2" -> do
      return $ Right Z.CutCursor
        { Z.text        = bs
        , Z.delimiters  = AVX2.cmpEqWord8s delimiter $ asVector64s 64 bs
        , Z.newlines    = AVX2.cmpEqWord8s wNewline  $ asVector64s 64 bs
        }
    "serial" -> do
      IO.writeOutputFile (opts ^. the @"outputFile")
        $ SERIAL.cmpEqWord8s delimiter bs
      return $ Right Z.CutCursor
        { Z.text        = bs
        , Z.delimiters  = asVector64s 64 $ SERIAL.cmpEqWord8s delimiter bs
        , Z.newlines    = asVector64s 64 $ SERIAL.cmpEqWord8s wNewline  bs
        }
    m -> do
      return $ Left $ "Unsupported method: " <> m

  case cursorResult of
    Right cursor -> do
      return ()

    Left msg -> do
      IO.hPutStrLn IO.stderr msg
      IO.exitFailure

  return ()

toRows :: Z.CutCursor -> [DV.Vector LBS.ByteString]
toRows c = []


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
cmdCut = command "cmpeqword8s"  $ flip info idm $ runCut <$> optsCut
