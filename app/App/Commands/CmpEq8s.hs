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
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as LBS
import qualified Data.Vector.Storable    as DVS
import qualified System.IO               as IO

runCmpEq8s :: CmpEq8sOptions -> IO ()
runCmpEq8s opts = do
  let !filePath   = opts ^. the @"filePath"
  let !delimiter  = opts ^. the @"delimiter"

  return ()

optsCmpEq8s :: Parser CmpEq8sOptions
optsCmpEq8s = CmpEq8sOptions
  <$> strOption
        (   long "input"
        <>  short 'i'
        <>  help "Input DSV file"
        <>  metavar "STRING"
        )
  <*> option readWord8
        (   long "input-delimiter"
        <>  short 'd'
        <>  help "DSV delimiter"
        <>  metavar "CHAR"
        )

cmdCmpEq8s :: Mod CommandFields (IO ())
cmdCmpEq8s = command "create-index"  $ flip info idm $ runCmpEq8s <$> optsCmpEq8s
