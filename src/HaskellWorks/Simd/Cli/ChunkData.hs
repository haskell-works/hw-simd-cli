{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TypeApplications #-}

module HaskellWorks.Simd.Cli.ChunkData where

import Control.Lens
import Data.Generics.Product.Any
import Data.Semigroup            (Semigroup (..))
import GHC.Generics

import qualified Data.ByteString as BS
import qualified Data.Map        as M

data ChunkData = ChunkData
  { count :: Int
  , sizes :: M.Map Int Int
  } deriving (Show, Generic)

instance Semigroup ChunkData where
  a <> b = ChunkData
    { count = a ^. the @"count" + b ^. the @"count"
    , sizes = M.unionWith (+) (a ^. the @"sizes") (b ^. the @"sizes")
    }

instance Monoid ChunkData where
  mempty = ChunkData 0 M.empty

chunkDataOf :: BS.ByteString -> ChunkData
chunkDataOf bs = ChunkData
  { count = 1
  , sizes = M.singleton (BS.length bs) 1
  }
