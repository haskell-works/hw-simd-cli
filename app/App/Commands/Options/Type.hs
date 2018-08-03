{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Type where

import GHC.Generics
import GHC.Word     (Word8)

data CmpEq8sOptions = CmpEq8sOptions
  { inputFile  :: FilePath
  , delimiter  :: Word8
  , outputFile :: FilePath
  } deriving (Eq, Show, Generic)

data CatOptions = CatOptions
  { inputFile  :: FilePath
  , outputFile :: FilePath
  } deriving (Eq, Show, Generic)
