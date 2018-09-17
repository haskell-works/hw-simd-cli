{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module App.Commands.Options.Type where

import GHC.Generics
import GHC.Word     (Word8)

data CmpEq8sOptions = CmpEq8sOptions
  { inputFile  :: FilePath
  , delimiter  :: Word8
  , outputFile :: FilePath
  , cmpMethod  :: String
  } deriving (Eq, Show, Generic)

data CatOptions = CatOptions
  { inputFile  :: FilePath
  , outputFile :: FilePath
  , method     :: String
  , chunkSize  :: Int
  } deriving (Eq, Show, Generic)

data ChunksOptions = ChunksOptions
  { inputFile   :: FilePath
  , chunkMethod :: String
  , readMethod  :: String
  } deriving (Eq, Show, Generic)

data CutOptions = CutOptions
  { inputFile    :: FilePath
  , delimiter    :: Word8
  , outputFile   :: FilePath
  , outDelimiter :: Word8
  , fields       :: [Int]
  , method       :: String
  } deriving (Eq, Show, Generic)

data WcOptions = WcOptions
  { inputFile  :: FilePath
  , outputFile :: FilePath
  , method     :: String
  } deriving (Eq, Show, Generic)
