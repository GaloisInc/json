{-# LANGUAGE BangPatterns #-}

module Codec.Web.JSON.Parse.Object
  ( enterObjectP
  , tilNextNameP
  , tilNextColonP
  , tilFirstPairP
  , tilNextPairP

  , copyTilNextNameP
  , copyTilNextColonP
  , copyTilFirstPairP
  , copyTilNextPairP
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



enterObjectP :: Parser ()
enterObjectP = do
  w <- peekWord8'
  if w == 0x7B {- { -}
    then () <$ anyWord8
    else err "Not an object value"



tilNextNameP :: Parser a -> Parser a
tilNextNameP f = go
  where
    go = do
      w <- peekWord8'
      if isWhitespace w
        then do _ <- anyWord8
                go
        else
          case w of
            0x22 {- " -}  -> f
            _             -> err $ "Unexpected byte 0x" <> showHex w " before object name"



tilNextColonP :: Parser ()
tilNextColonP = go
  where
    go = do
      w <- peekWord8'
      if isWhitespace w
        then do _ <- anyWord8
                go
        else
          case w of
            0x3A {- : -}  -> () <$ anyWord8
            _             -> err $ "Unexpected byte 0x" <> showHex w " before object colon"



tilFirstPairP :: Parser Bool
tilFirstPairP = go
  where
    go = do
      w <- peekWord8'
      if isWhitespace w
        then do _ <- anyWord8
                go
        else
          case w of
            0x7D {- } -}  -> False <$ anyWord8
            _             -> pure True

tilNextPairP :: Parser Bool
tilNextPairP = go
  where
    go = do
      w <- peekWord8'
      if isWhitespace w
        then do _ <- anyWord8
                go
        else
          case w of
            0x7D {- } -}  -> False <$ anyWord8
            0x2C {- , -}  -> True <$ anyWord8
            _             -> err $ "Unexpected byte 0x" <> showHex w " after object value"



{-# INLINE pokeByte #-}
pokeByte :: Int -> BSL.Copy -> Word8 -> BSL.Copy
pokeByte len copy w =
  BSL.writeCopy len copy 1 $ \ptr off -> poke (plusPtr ptr off) w



copyTilNextNameP :: Int -> BSL.Copy -> (BSL.Copy -> Parser BSL.Copy) -> Parser BSL.Copy
copyTilNextNameP chunklen copy f = go copy
  where
    go !c0 = do
      w <- peekWord8'
      let c1 = pokeByte chunklen c0 w
      if isWhitespace w
        then do _ <- anyWord8
                go c1
        else
          case w of
            0x22 {- " -}  -> f c1
            _             -> err $ "Unexpected byte 0x" <> showHex w " before object name"



copyTilNextColonP :: Int -> BSL.Copy -> Parser BSL.Copy
copyTilNextColonP chunklen = go
  where
    go !c = do
      w <- peekWord8'
      let c' = pokeByte chunklen c w
      if isWhitespace w
        then do _ <- anyWord8
                go c'
        else
          case w of
            0x3A {- : -}  -> c' <$ anyWord8
            _             -> err $ "Unexpected byte 0x" <> showHex w " before object colon"



copyTilFirstPairP :: Int -> BSL.Copy -> Parser (Bool, BSL.Copy)
copyTilFirstPairP chunklen = go
  where
    go !c = do
      w <- peekWord8'
      let c' = pokeByte chunklen c w
      if isWhitespace w
        then do _ <- anyWord8
                go c'
        else
          case w of
            0x7D {- } -}  -> (False, c') <$ anyWord8
            _             -> pure (True, c)

copyTilNextPairP :: Int -> BSL.Copy -> Parser (Bool, BSL.Copy)
copyTilNextPairP chunklen = go
  where
    go !c = do
      w <- peekWord8'
      let c' = pokeByte chunklen c w
      if isWhitespace w
        then do _ <- anyWord8
                go c'
        else
          case w of
            0x7D {- } -}  -> (False, c') <$ anyWord8
            0x2C {- , -}  -> (True, c') <$ anyWord8
            _             -> err $ "Unexpected byte 0x" <> showHex w " after object value"
