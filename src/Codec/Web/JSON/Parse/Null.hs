module Codec.Web.JSON.Parse.Null
  ( nullP

  , copyNullP
  ) where

import           Data.Attoparsec.Error
import qualified Data.ByteString.Lazy.Copy as BSL

import           Data.Attoparsec.ByteString
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Numeric



nullP :: Parser ()
nullP = do
  let unex x = err $ "Unexpected byte 0x" <> showHex x " in null"
  u <- anyWord8
  if u == 0x75 {- u -}
    then do
      l0 <- anyWord8
      if l0 == 0x6C {- l -}
        then do
          l1 <- anyWord8
          if l1 == 0x6C {- l -}
            then pure ()
            else unex l1
        else unex l0
    else unex u



copyNullP :: Int -> BSL.Copy -> Parser BSL.Copy
copyNullP chunklen copy = do
  nullP
  pure $! do BSL.writeCopy chunklen copy 4 $ \ptr off -> do
               poke (plusPtr ptr   off    ) (0x6E {- n -} :: Word8)
               poke (plusPtr ptr $ off + 1) (0x75 {- u -} :: Word8)
               poke (plusPtr ptr $ off + 2) (0x6C {- l -} :: Word8)
               poke (plusPtr ptr $ off + 3) (0x6C {- l -} :: Word8)

