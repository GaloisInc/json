{-# LANGUAGE BangPatterns #-}

module Codec.Web.JSON.Parse.Array
  ( enterArrayP
  , tilFirstValueP
  , tilNextValueP

  , copyTilFirstValueP
  , copyTilNextValueP
  ) where

import           Data.Attoparsec.Error
import qualified Data.ByteString.Lazy.Copy as BSL

import           Data.Attoparsec.ByteString
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           Numeric



{-# INLINE isWhitespace #-}
isWhitespace :: Word8 -> Bool
isWhitespace w =
  w == 0x20 {-   -} || w == 0x09 {- \t -} || w == 0x0A {- \n -} || w == 0x0D {- \r -}



{-# INLINE enterArrayP #-}
enterArrayP :: Parser ()
enterArrayP = do
  w <- peekWord8'
  if w == 0x5B {- [ -}
    then () <$ anyWord8
    else err "Not an array value"



tilFirstValueP :: Parser Bool
tilFirstValueP = go
  where
    go = do
      w <- peekWord8'
      if isWhitespace w
        then do _ <- anyWord8
                go
        else
          case w of
            0x5D {- ] -}  -> False <$ anyWord8
            _             -> pure True



tilNextValueP :: Parser Bool
tilNextValueP = go
  where
    go = do
      w <- peekWord8'
      if isWhitespace w
        then do _ <- anyWord8
                go
        else
          case w of
            0x5D {- ] -}  -> False <$ anyWord8
            0x2C {- , -}  -> True <$ anyWord8
            _             -> err $ "Unexpected byte 0x" <> showHex w " after array value"



{-# INLINE pokeByte #-}
pokeByte :: Int -> BSL.Copy -> Word8 -> BSL.Copy
pokeByte len copy w =
  BSL.writeCopy len copy 1 $ \ptr off -> poke (plusPtr ptr off) w



copyTilFirstValueP :: Int -> BSL.Copy -> Parser (Bool, BSL.Copy)
copyTilFirstValueP chunklen = go
  where
    go !c = do
      w <- peekWord8'
      let c' = pokeByte chunklen c w
      if isWhitespace w
        then do _ <- anyWord8
                go c'
        else
          case w of
            0x5D {- ] -}  -> (False, c') <$ anyWord8
            _             -> pure (True, c)



copyTilNextValueP :: Int -> BSL.Copy -> Parser (Bool, BSL.Copy)
copyTilNextValueP chunklen = go
  where
    go !c = do
      w <- peekWord8'
      let c' = pokeByte chunklen c w
      if isWhitespace w
        then do _ <- anyWord8
                go c'
        else
          case w of
            0x5D {- ] -}  -> (False, c') <$ anyWord8
            0x2C {- , -}  -> (True, c') <$ anyWord8
            _             -> err $ "Unexpected byte 0x" <> showHex w " after array value"
