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
import qualified Data.ByteString.Lazy                    as LBS
import qualified HaskellWorks.Data.ByteString.Lazy       as LBS
import qualified HaskellWorks.Data.Simd.Comparison.Stock as STOCK

runCmpEq8s :: Z.CmpEq8sOptions -> IO ()
runCmpEq8s opts = do
  let !delimiter  = opts ^. the @"delimiter"

  bs <- IO.readInputFile (opts ^. the @"inputFile")

  LBS.writeFile (opts ^. the @"outputFile")
    $ LBS.toLazyByteString
    $ STOCK.cmpEqWord8s delimiter
    $ asVector64s 64 bs

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

cmdCmpEq8s :: Mod CommandFields (IO ())
cmdCmpEq8s = command "cmpeqword8s"  $ flip info idm $ runCmpEq8s <$> optsCmpEq8s
