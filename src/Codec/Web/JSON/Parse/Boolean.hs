module Codec.Web.JSON.Parse.Boolean
  ( falseP
  , trueP

  , copyFalseP
  , copyTrueP
  ) where

import           Data.Attoparsec.Error
import qualified Data.ByteString.Lazy.Copy as BSL

import           Data.Attoparsec.ByteString
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Numeric



falseP :: Parser ()
falseP = do
  let unex x = err $ "Unexpected byte 0x" <> showHex x []
  a <- anyWord8
  if a == 0x61 {- a -}
    then do
      l <- anyWord8
      if l == 0x6C {- l -}
        then do
          s <- anyWord8
          if s == 0x73 {- s -}
            then do
              e <- anyWord8
              if e == 0x65 {- e -}
                then pure ()
                else unex e
            else unex s
        else unex l
    else unex a



trueP :: Parser ()
trueP = do
  let unex x = err $ "Unexpected byte 0x" <> showHex x []
  r <- anyWord8
  if r == 0x72 {- r -}
    then do
      u <- anyWord8
      if u == 0x75 {- u -}
        then do
          e <- anyWord8
          if e == 0x65 {- e -}
            then pure ()
            else unex e
        else unex u
    else unex r



copyFalseP :: Int -> BSL.Copy -> Parser BSL.Copy
copyFalseP chunklen copy = do
  falseP
  pure $! do BSL.writeCopy chunklen copy 5 $ \ptr off -> do
               poke (plusPtr ptr   off    ) (0x66 {- f -} :: Word8)
               poke (plusPtr ptr $ off + 1) (0x61 {- a -} :: Word8)
               poke (plusPtr ptr $ off + 2) (0x6C {- l -} :: Word8)
               poke (plusPtr ptr $ off + 3) (0x73 {- s -} :: Word8)
               poke (plusPtr ptr $ off + 4) (0x65 {- e -} :: Word8)

copyTrueP :: Int -> BSL.Copy -> Parser BSL.Copy
copyTrueP chunklen copy = do
  trueP
  pure $! do BSL.writeCopy chunklen copy 4 $ \ptr off -> do
               poke (plusPtr ptr   off    ) (0x74 {- t -} :: Word8)
               poke (plusPtr ptr $ off + 1) (0x72 {- r -} :: Word8)
               poke (plusPtr ptr $ off + 2) (0x75 {- u -} :: Word8)
               poke (plusPtr ptr $ off + 3) (0x65 {- e -} :: Word8)
