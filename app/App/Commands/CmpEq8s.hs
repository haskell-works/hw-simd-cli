{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.CmpEq8s
  ( cmdCmpEq8s
  ) where

import App.Char
import App.Commands.Options.Type
import Control.Lens
import Control.Monad
import Data.Generics.Product.Any
import Data.Semigroup                       ((<>))
import HaskellWorks.Data.Vector.AsVector64s
import Options.Applicative                  hiding (columns)

import qualified App.IO                  as IO
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Vector.Storable    as DVS
import qualified System.IO               as IO

runCmpEq8s :: CmpEq8sOptions -> IO ()
runCmpEq8s opts = do
  let !delimiter  = opts ^. the @"delimiter"

  bs <- IO.readInputFile (opts ^. the @"inputFile")

  let x = asVector64s 64 bs

  return ()

optsCmpEq8s :: Parser CmpEq8sOptions
optsCmpEq8s = CmpEq8sOptions
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
cmdCmpEq8s = command "create-index"  $ flip info idm $ runCmpEq8s <$> optsCmpEq8s
