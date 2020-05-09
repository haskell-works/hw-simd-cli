{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.Wc
  ( cmdWc
  ) where

import Control.Lens
import Data.Char                                 (ord)
import Data.Generics.Product.Any
import HaskellWorks.Data.Bits.PopCount.PopCount1
import HaskellWorks.Data.Vector.AsVector64s
import Options.Applicative                       hiding (columns)

import qualified App.Commands.Options.Type               as Z
import qualified App.IO                                  as IO
import qualified Data.ByteString.Lazy                    as LBS
import qualified HaskellWorks.Data.Simd.Comparison.Avx2  as AVX2
import qualified HaskellWorks.Data.Simd.Comparison.Stock as STOCK
import qualified HaskellWorks.Simd.Cli.Comparison        as SERIAL
import qualified System.Exit                             as IO
import qualified System.IO                               as IO

{- HLINT ignore "Redundant do" -}

runWc :: Z.WcOptions -> IO ()
runWc opts = do
  bs <- IO.readInputFile (opts ^. the @"inputFile")
  let wNewline = fromIntegral $ ord '\n'

  numRowsResult <- case opts ^. the @"method" of
    "stock"  -> return $ Right $ popCount1 $ STOCK.cmpEqWord8s wNewline $ asVector64s 64 bs
    "avx2"   -> return $ Right $ popCount1 $ AVX2.cmpEqWord8s wNewline  $ asVector64s 64 bs
    "serial" -> return $ Right $ popCount1 $ asVector64s 64 $ SERIAL.cmpEqWord8s wNewline bs
    "simple" -> return $ Right $ LBS.foldl (\wc b -> if b == 10 then wc + 1 else wc) 0 bs
    m        -> return $ Left  $ "Unsupported method: " <> m

  case numRowsResult of
    Right numRows -> IO.print numRows

    Left msg -> do
      IO.hPutStrLn IO.stderr msg
      IO.exitFailure

  return ()

optsWc :: Parser Z.WcOptions
optsWc = Z.WcOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input Text file"
        <>  metavar "STRING"
        )
  <*> strOption
        (   long "output"
        <>  short 'o'
        <>  help "Output Text file"
        <>  metavar "STRING"
        )
  <*> strOption
        (   long "method"
        <>  short 'm'
        <>  help "Comparison method"
        <>  metavar "STRING"
        )

cmdWc :: Mod CommandFields (IO ())
cmdWc = command "wc"  $ flip info idm $ runWc <$> optsWc
