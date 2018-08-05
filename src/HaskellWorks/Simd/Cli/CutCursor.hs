{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Simd.Cli.CutCursor
  ( CutCursor(..)
  ) where

import Data.Word
import GHC.Generics

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector.Storable as DVS

data CutCursor = CutCursor
  { text       :: LBS.ByteString
  , delimiters :: [DVS.Vector Word64]
  , newlines   :: [DVS.Vector Word64]
  } deriving Generic
