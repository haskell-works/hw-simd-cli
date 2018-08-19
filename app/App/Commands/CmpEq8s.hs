{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.CmpEq8s
  ( cmdCmpEq8s
  ) where

import App.Char
import Control.Lens
import Data.Generics.Product.Any
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.Vector.AsVector64s
import Options.Applicative                  hiding (columns)

import qualified App.Commands.Options.Type               as Z
import qualified App.IO                                  as IO
import qualified HaskellWorks.Data.ByteString.Lazy       as LBS
import qualified HaskellWorks.Data.Simd.ChunkString      as CS
import qualified HaskellWorks.Data.Simd.Comparison.Avx2  as AVX2
import qualified HaskellWorks.Data.Simd.Comparison.Stock as STOCK
import qualified HaskellWorks.Simd.Cli.Comparison        as SERIAL
import qualified System.Exit                             as IO
import qualified System.IO                               as IO

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

runCmpEq8s :: Z.CmpEq8sOptions -> IO ()
runCmpEq8s opts = do
  let !delimiter  = opts ^. the @"delimiter"

  case opts ^. the @"cmpMethod" of
    "new-stock" -> do
      cs <- IO.openInputFile (opts ^. the @"inputFile") >>= CS.hGetContents

      IO.writeOutputFile (opts ^. the @"outputFile")
        $ LBS.toLazyByteString
        $ STOCK.cmpEqWord8s delimiter cs
    "new-avx2" -> do
      cs <- IO.openInputFile (opts ^. the @"inputFile") >>= CS.hGetContents

      IO.writeOutputFile (opts ^. the @"outputFile")
        $ LBS.toLazyByteString
        $ AVX2.cmpEqWord8s delimiter cs
    "stock" -> do
      bs <- IO.readInputFile (opts ^. the @"inputFile")
      let v = asVector64s 64 bs

      IO.writeOutputFile (opts ^. the @"outputFile")
        $ LBS.toLazyByteString
        $ STOCK.cmpEqWord8s delimiter v
    "avx2" -> do
      bs <- IO.readInputFile (opts ^. the @"inputFile")
      let v = asVector64s 64 bs

      IO.writeOutputFile (opts ^. the @"outputFile")
        $ LBS.toLazyByteString
        $ AVX2.cmpEqWord8s delimiter v
    "serial" -> do
      bs <- IO.readInputFile (opts ^. the @"inputFile")

      IO.writeOutputFile (opts ^. the @"outputFile")
        $ SERIAL.cmpEqWord8s delimiter bs
    m -> do
      IO.hPutStrLn IO.stderr $ "Unsupported method: " <> m
      IO.exitFailure

optsCmpEq8s :: Parser Z.CmpEq8sOptions
optsCmpEq8s = Z.CmpEq8sOptions
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
  <*> strOption
        (   long "cmp-method"
        <>  short 'm'
        <>  help "Comparison method"
        <>  metavar "STRING"
        )

cmdCmpEq8s :: Mod CommandFields (IO ())
cmdCmpEq8s = command "cmpeqword8s"  $ flip info idm $ runCmpEq8s <$> optsCmpEq8s
