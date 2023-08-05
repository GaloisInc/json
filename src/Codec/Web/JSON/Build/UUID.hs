module Codec.Web.JSON.Build.UUID
  ( uuidB
  ) where

import           Data.Bits
import           Data.ByteString.Builder (Builder)
import           Data.ByteString.Builder.Prim
import           Data.UUID.Types



uuidB :: UUID -> Builder
uuidB uuid =
  let (w0, w1) = toWords64 uuid

      hex c | c < 10    = c + 0x30
            | otherwise = c + 0x57

      tl0 = fromIntegral . hex $ unsafeShiftR w0 60 .&. 0x0F
      tl1 = fromIntegral . hex $ unsafeShiftR w0 56 .&. 0x0F
      tl2 = fromIntegral . hex $ unsafeShiftR w0 52 .&. 0x0F
      tl3 = fromIntegral . hex $ unsafeShiftR w0 48 .&. 0x0F
      tl4 = fromIntegral . hex $ unsafeShiftR w0 44 .&. 0x0F
      tl5 = fromIntegral . hex $ unsafeShiftR w0 40 .&. 0x0F
      tl6 = fromIntegral . hex $ unsafeShiftR w0 36 .&. 0x0F
      tl7 = fromIntegral . hex $ unsafeShiftR w0 32 .&. 0x0F
      tm0 = fromIntegral . hex $ unsafeShiftR w0 28 .&. 0x0F
      tm1 = fromIntegral . hex $ unsafeShiftR w0 24 .&. 0x0F
      tm2 = fromIntegral . hex $ unsafeShiftR w0 20 .&. 0x0F
      tm3 = fromIntegral . hex $ unsafeShiftR w0 16 .&. 0x0F
      th0 = fromIntegral . hex $ unsafeShiftR w0 12 .&. 0x0F
      th1 = fromIntegral . hex $ unsafeShiftR w0  8 .&. 0x0F
      th2 = fromIntegral . hex $ unsafeShiftR w0  4 .&. 0x0F
      th3 = fromIntegral . hex $              w0    .&. 0x0F

      ch0 = fromIntegral . hex $ unsafeShiftR w1 60 .&. 0x0F
      ch1 = fromIntegral . hex $ unsafeShiftR w1 56 .&. 0x0F
      cl0 = fromIntegral . hex $ unsafeShiftR w1 52 .&. 0x0F
      cl1 = fromIntegral . hex $ unsafeShiftR w1 48 .&. 0x0F
      n00 = fromIntegral . hex $ unsafeShiftR w1 44 .&. 0x0F
      n01 = fromIntegral . hex $ unsafeShiftR w1 40 .&. 0x0F
      n02 = fromIntegral . hex $ unsafeShiftR w1 36 .&. 0x0F
      n03 = fromIntegral . hex $ unsafeShiftR w1 32 .&. 0x0F
      n04 = fromIntegral . hex $ unsafeShiftR w1 28 .&. 0x0F
      n05 = fromIntegral . hex $ unsafeShiftR w1 24 .&. 0x0F
      n06 = fromIntegral . hex $ unsafeShiftR w1 20 .&. 0x0F
      n07 = fromIntegral . hex $ unsafeShiftR w1 16 .&. 0x0F
      n08 = fromIntegral . hex $ unsafeShiftR w1 12 .&. 0x0F
      n09 = fromIntegral . hex $ unsafeShiftR w1  8 .&. 0x0F
      n10 = fromIntegral . hex $ unsafeShiftR w1  4 .&. 0x0F
      n11 = fromIntegral . hex $              w1    .&. 0x0F

  in primMapListFixed word8
       [ tl0, tl1, tl2, tl3, tl4, tl5, tl6, tl7, 0x2d {- - -}
       , tm0, tm1, tm2, tm3, 0x2d {- - -}
       , th0, th1, th2, th3, 0x2d {- - -}
       , ch0, ch1, cl0, cl1, 0x2d {- - -}
       , n00, n01, n02, n03, n04, n05, n06, n07, n08, n09, n10, n11
       ]
