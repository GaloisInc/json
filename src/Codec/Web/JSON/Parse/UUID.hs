module Codec.Web.JSON.Parse.UUID
  ( uuidP
  ) where

import           Data.Attoparsec.Error

import           Data.Attoparsec.ByteString
import           Data.Bits
import           Data.UUID.Types
import           Data.Word
import           Numeric



{-# INLINE toHex #-}
toHex :: Word8 -> Maybe Word8
toHex w =
  if w <= 0x66 {- f -}
    then if w >= 0x61 {- a -}
           then Just $ w - 0x57
           else if w <= 0x46 {- F -}
                  then if w >= 0x41 {- A -}
                         then Just $ w - 0x37
                         else if w <= 0x39 {- 9 -} && w >= 0x30 {- 0 -}
                                then Just $ w - 0x30
                                else Nothing
                  else Nothing
    else Nothing



uuidP :: Parser UUID
uuidP = do
  let unex x = showString "Unexpected byte 0x" $ showHex x " in UUID"

      {-# INLINE dash #-}
      dash = do
        x <- peekWord8'
        if x == 0x2d {- - -}
          then () <$ anyWord8
          else err $ unex x

      {-# INLINE hex #-}
      hex = do
        x <- peekWord8'
        case toHex x of
          Just h  -> h <$ anyWord8
          Nothing -> err $ unex x

  tl0 <- hex
  tl1 <- hex
  tl2 <- hex
  tl3 <- hex
  tl4 <- hex
  tl5 <- hex
  tl6 <- hex
  tl7 <- hex
  dash
  tm0 <- hex
  tm1 <- hex
  tm2 <- hex
  tm3 <- hex
  dash
  th0 <- hex
  th1 <- hex
  th2 <- hex
  th3 <- hex
  dash
  ch0 <- hex
  ch1 <- hex
  cl0 <- hex
  cl1 <- hex
  dash
  n00 <- hex
  n01 <- hex
  n02 <- hex
  n03 <- hex
  n04 <- hex
  n05 <- hex
  n06 <- hex
  n07 <- hex
  n08 <- hex
  n09 <- hex
  n10 <- hex
  n11 <- hex

  q <- anyWord8
  if q /= 0x22
    then err $ unex q
    else pure $ let w0 = unsafeShiftL (fromIntegral tl0) 60
                       + unsafeShiftL (fromIntegral tl1) 56
                       + unsafeShiftL (fromIntegral tl2) 52
                       + unsafeShiftL (fromIntegral tl3) 48
                       + unsafeShiftL (fromIntegral tl4) 44
                       + unsafeShiftL (fromIntegral tl5) 40
                       + unsafeShiftL (fromIntegral tl6) 36
                       + unsafeShiftL (fromIntegral tl7) 32
                       + unsafeShiftL (fromIntegral tm0) 28
                       + unsafeShiftL (fromIntegral tm1) 24
                       + unsafeShiftL (fromIntegral tm2) 20
                       + unsafeShiftL (fromIntegral tm3) 16
                       + unsafeShiftL (fromIntegral th0) 12
                       + unsafeShiftL (fromIntegral th1)  8
                       + unsafeShiftL (fromIntegral th2)  4
                       +               fromIntegral th3

                    w1 = unsafeShiftL (fromIntegral ch0) 60
                       + unsafeShiftL (fromIntegral ch1) 56
                       + unsafeShiftL (fromIntegral cl0) 52
                       + unsafeShiftL (fromIntegral cl1) 48
                       + unsafeShiftL (fromIntegral n00) 44
                       + unsafeShiftL (fromIntegral n01) 40
                       + unsafeShiftL (fromIntegral n02) 36
                       + unsafeShiftL (fromIntegral n03) 32
                       + unsafeShiftL (fromIntegral n04) 28
                       + unsafeShiftL (fromIntegral n05) 24
                       + unsafeShiftL (fromIntegral n06) 20
                       + unsafeShiftL (fromIntegral n07) 16
                       + unsafeShiftL (fromIntegral n08) 12
                       + unsafeShiftL (fromIntegral n09)  8
                       + unsafeShiftL (fromIntegral n10)  4
                       +               fromIntegral n11

                in fromWords64 w0 w1
