{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Simd.Cli.Comparison
  ( CmpEqWord8s(..)
  ) where

import Data.ByteString                as BS
import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning

import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Vector.Storable         as DVS
import qualified HaskellWorks.Data.ByteString as BS
import qualified HaskellWorks.Data.Length     as HW

class CmpEqWord8s a where
  cmpEqWord8s :: Word8 -> a -> a

instance CmpEqWord8s BS.ByteString where
  cmpEqWord8s w8 bs = BS.toByteString $ DVS.constructN ((BS.length bs + 7) `div` 8) (go 0 0)
    where go :: Word8 -> Count -> DVS.Vector Word8 -> Word8
          go w n u = case HW.length u of
            ui -> case ui * 8 of
              bsi -> if bsi + 8 <= HW.length bs
                then goFast w n u
                else goSafe w n u
          goFast :: Word8 -> Count -> DVS.Vector Word8 -> Word8
          goFast w n u = if n < 8
            then case HW.length u of
              ui -> case ui * 8 of
                bsi -> case bsi + n of
                  wi -> if w8 == bs !!! fromIntegral wi
                    then goFast (w .|. (1 .<. n)) (n + 1) u
                    else goFast  w                (n + 1) u
            else w
          goSafe :: Word8 -> Count -> DVS.Vector Word8 -> Word8
          goSafe w n u = if n < 8
            then case HW.length u of
              ui -> case ui * 8 of
                bsi -> case bsi + n of
                  wi -> if wi < bsLen
                    then if w8 == bs !!! fromIntegral wi
                      then goSafe (w .|. (1 .<. n)) (n + 1) u
                      else goSafe  w                (n + 1) u
                    else w
            else w
          bsLen = HW.length bs
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s [BS.ByteString] where
  cmpEqWord8s w8 vs = cmpEqWord8s w8 <$> vs
  {-# INLINE cmpEqWord8s #-}

instance CmpEqWord8s LBS.ByteString where
  cmpEqWord8s w8 = LBS.fromChunks . cmpEqWord8s w8 . LBS.toChunks
  {-# INLINE cmpEqWord8s #-}
