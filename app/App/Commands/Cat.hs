{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module App.Commands.Cat
  ( cmdCat
  ) where

import App.Commands.Options.Type
import Control.Lens
import Data.Generics.Product.Any
import Data.Semigroup            ((<>))
import Options.Applicative       hiding (columns)

import qualified App.IO               as IO
import qualified Data.ByteString.Lazy as LBS

runCat :: CatOptions -> IO ()
runCat opts = do
  bs <- IO.readInputFile (opts ^. the @"inputFile")

  LBS.writeFile (opts ^. the @"outputFile") bs

optsCat :: Parser CatOptions
optsCat = CatOptions
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

cmdCat :: Mod CommandFields (IO ())
cmdCat = command "create-index"  $ flip info idm $ runCat <$> optsCat
