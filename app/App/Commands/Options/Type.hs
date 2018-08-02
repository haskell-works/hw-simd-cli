{-# LANGUAGE DeriveGeneric #-}

module App.Commands.Options.Type where

import GHC.Generics
import GHC.Word     (Word8)

data CmpEq8sOptions = CmpEq8sOptions
  { filePath  :: FilePath
  , delimiter :: Word8
  } deriving (Eq, Show, Generic)
