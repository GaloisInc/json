{-# LANGUAGE BangPatterns
           , RankNTypes #-}

module Codec.Web.JSON.Parse.String
  ( stringUtf8P

  , lookupUtf8P
  , updateUtf8P
  , alterUtf8P

  , textUtf8P
  , lazyTextUtf8P

  , byteStringUtf8P
  , lazyByteStringUtf8P

  , skipUtf8P
  , copyByteStringUtf8P
  ) where

import           Encoding.Mixed.Error
import           Data.Attoparsec.Error
import qualified Data.ByteString.Copy as BS
import qualified Data.ByteString.Lazy.Copy as BSL
import           Data.RadixTree.Char (RadixTree)
import qualified Data.RadixTree.Char as Radix
import qualified Data.Text.Copy as Text

import           Data.Attoparsec.ByteString as Atto hiding (word8, Fail)
import           Data.Bits
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL (ByteString, empty)
import qualified Data.ByteString.Lazy.Internal as BSL (chunk)
import           Data.Char hiding (Surrogate)
import           Data.Text as Text hiding (copy)
import           Data.Text.Array
import qualified Data.Text.Lazy as Lazy (Text, empty)
import qualified Data.Text.Internal.Lazy as Lazy (chunk)
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.Base (unsafeChr)
import           Numeric



{-# INLINE toHex #-}
toHex :: Word8 -> Maybe Word8
toHex w = do
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



{-# INLINE utf16_escape #-}
-- | @\\u@ escape with a potential second @\\u@ low surrogate escape.
utf16_escape :: (DecodingError -> Parser a) -> (Char -> Parser a) -> Parser a
utf16_escape recover f = do
  let {-# INLINE satisfyHex #-}
      satisfyHex = do
        w <- peekWord8'
        case toHex w of
          Just a  -> a <$ anyWord8
          Nothing -> err $ "Unexpected byte 0x" <> showHex w " in UTF-16 escaped literal"

  a <- satisfyHex
  b <- satisfyHex
  c <- satisfyHex
  d <- satisfyHex

  let {-# INLINE hi #-}
      hi :: (Bits a, Num a) => a
      hi = unsafeShiftL (fromIntegral a) 12
         + unsafeShiftL (fromIntegral b) 8
         + unsafeShiftL (fromIntegral c) 4
         +               fromIntegral d

  if a == 0x0D && b >= 0x08
    then
      if b >= 0x0C
        then recover (UTF16Error $ Invalid1 hi)
        else do
          s <- peekWord8'
          if s == 0x5C {- \ -}
            then () <$ anyWord8
            else err $ "Unexpected byte 0x" <> showHex s " in a string"

          u <- peekWord8'
          if u == 0x75 {- u -}
            then () <$ anyWord8
            else err $ "Unexpected byte 0x" <> showHex u " after character escape"

          w <- satisfyHex
          x <- satisfyHex
          y <- satisfyHex
          z <- satisfyHex

          let lo = unsafeShiftL (fromIntegral w) 12
                 + unsafeShiftL (fromIntegral x) 8
                 + unsafeShiftL (fromIntegral y) 4
                 +               fromIntegral z

              v = unsafeShiftL (fromIntegral b) 18
                + unsafeShiftL (fromIntegral c) 14
                + unsafeShiftL (fromIntegral d) 10
                + unsafeShiftL (fromIntegral x) 8
                + unsafeShiftL (fromIntegral y) 4
                +               fromIntegral z
                - 0x1f0c00

          if w == 0x0D && x >= 0x0C
            then f (unsafeChr v)
            else recover (UTF16Error $ Invalid2 hi lo)

     else f (unsafeChr hi)



{-# INLINE utf8_1 #-}
-- | Single byte UTF-8 code point.
utf8_1
  :: (DecodingError -> Parser a)
  -> Word8
  -> Parser a
  -> (Char -> Parser a)
  -> (Word8 -> Parser a)
  -> Parser a
utf8_1 recover u0 end utf16f f =
  if u0 >= 0x20
    then
      case u0 of
        0x22 {- " -} -> end
        0x5c {- \ -} -> do
          _ <- anyWord8
          u1 <- peekWord8'
          case u1 of
            0x22 -> anyWord8 >> f 0x22 {- "  -}
            0x2F -> anyWord8 >> f 0x2F {- /  -}
            0x5C -> anyWord8 >> f 0x5C {- \  -}
            0x62 -> anyWord8 >> f 0x08 {- \b -}
            0x66 -> anyWord8 >> f 0x0C {- \f -}
            0x6E -> anyWord8 >> f 0x0A {- \n -}
            0x72 -> anyWord8 >> f 0x0D {- \r -}
            0x74 -> anyWord8 >> f 0x09 {- \t -}
            0x75 -> anyWord8 >> utf16_escape recover utf16f
            _    -> err $ "Unexpected byte 0x"
                 <> showHex u0 " after character escape"

        _            -> anyWord8 >> f u0

   else err $ "Unexpected byte 0x" <> showHex u0 " in a string"



{-# INLINE utf8_2 #-}
-- | Two byte UTF-8 code point.
utf8_2 :: (DecodingError -> Parser a) -> Word8 -> (Word8 -> Parser a) -> Parser a
utf8_2 recover u0 f = do
  _ <- anyWord8
  if (u0 .&. 0x1F) < 0x02
    then recover (UTF8Error $ Overlong2 u0)
    else do
      u1 <- peekWord8'
      if (u1 .&. 0xC0) /= 0x80
        then recover (UTF8Error $ Incomplete22 u0 u1)
        else anyWord8 >> f u1



{-# INLINE utf8_3 #-}
-- | Three byte UTF-8 code point.
utf8_3 :: (DecodingError -> Parser a) -> Word8 -> (Word8 -> Word8 -> Parser a) -> Parser a
utf8_3 recover u0 f = do
  _ <- anyWord8
  u1 <- peekWord8'
  if (u1 .&. 0xC0) /= 0x80
    then recover (UTF8Error $ Incomplete23 u0 u1)
    else
      if (u0 .&. 0x0F) == 0x0D && (u1 .&. 0x20) /= 0
        then recover (UTF8Error $ Surrogate u1)
        else
          if (u0 .&. 0x0F) == 0 && (u1 .&. 0x20) == 0
            then recover (UTF8Error $ Overlong3 u1)
            else do
              _ <- anyWord8
              u2 <- peekWord8'
              if (u2 .&. 0xC0) /= 0x80
                then recover (UTF8Error $ Incomplete33 u0 u1 u2)
                else anyWord8 >> f u1 u2



{-# INLINE utf8_4 #-}
-- | Four byte UTF-8 code point.
utf8_4
  :: (DecodingError -> Parser a) -> Word8 -> (Word8 -> Word8 -> Word8 -> Parser a) -> Parser a
utf8_4 recover u0 f = do
  _ <- anyWord8
  if (u0 .&. 0x07) > 0x04
    then recover (UTF8Error $ Overflow1 u0)
    else do
      u1 <- peekWord8'
      if (u1 .&. 0xC0) /= 0x80
        then recover (UTF8Error $ Incomplete24 u0 u1)
        else
          if (u0 .&. 0x07) == 0x04 && (u1 .&. 0x3F) >= 0x10
            then recover (UTF8Error $ Overflow2 u1)
            else
              if (u0 .&. 0x07) == 0x00 && (u1 .&. 0x3F) < 0x10
                then recover (UTF8Error $ Overlong4 u1)
                else do
                  _ <- anyWord8
                  u2 <- peekWord8'
                  if (u2 .&. 0xC0) /= 0x80
                    then recover (UTF8Error $ Incomplete34 u0 u1 u2)
                    else do
                      _ <- anyWord8
                      u3 <- peekWord8'
                      if (u3 .&. 0xC0) /= 0x80
                        then recover (UTF8Error $ Incomplete4 u0 u1 u2 u3)
                        else anyWord8 >> f u1 u2 u3



{-# INLINE unsafeUtf8Chr_2 #-}
unsafeUtf8Chr_2 :: Word8 -> Word8 -> Char
unsafeUtf8Chr_2 u0 u1 =
  unsafeChr $ unsafeShiftL (fromIntegral u0 .&. 0x1F) 6
            +              (fromIntegral u1 .&. 0x3F)

{-# INLINE unsafeUtf8Chr_3 #-}
unsafeUtf8Chr_3 :: Word8 -> Word8 -> Word8 -> Char
unsafeUtf8Chr_3 u0 u1 u2 =
  unsafeChr $ unsafeShiftL (fromIntegral (u0 .&. 0x0F)) 12
            + unsafeShiftL (fromIntegral (u1 .&. 0x3F))  6
            +              (fromIntegral (u2 .&. 0x3F))

{-# INLINE unsafeUtf8Chr_4 #-}
unsafeUtf8Chr_4 :: Word8 -> Word8 -> Word8 -> Word8 -> Char
unsafeUtf8Chr_4 u0 u1 u2 u3 =
  unsafeChr $ unsafeShiftL (fromIntegral (u0 .&. 0x07)) 18
            + unsafeShiftL (fromIntegral (u1 .&. 0x3F)) 12
            + unsafeShiftL (fromIntegral (u2 .&. 0x3F))  6
            +              (fromIntegral (u3 .&. 0x3F))




stringUtf8P :: DecodingHandler -> Parser String
stringUtf8P (DecodingHandler onerr) = go
  where
    {-# INLINE recover #-}
    recover e = case onerr e of
                  Fail msg     -> err msg
                  Replace char -> (:) char <$> go
                  Ignore       -> go

    go = do
      u0 <- peekWord8'
      case () of
        () | (u0 .&. 0x80) == 0 ->
               utf8_1 recover u0 ([] <$ anyWord8)
                                 (\c -> (:) c <$> go)
                                 (\w -> (:) (unsafeChr $ fromIntegral w) <$> go)

           | (u0 .&. 0x40) == 0 -> anyWord8 >> recover (UTF8Error $ Continuation u0)

           | (u0 .&. 0x20) == 0 ->
               utf8_2 recover u0 $ \u1 -> (:) (unsafeUtf8Chr_2 u0 u1) <$> go

           | (u0 .&. 0x10) == 0 ->
               utf8_3 recover u0 $ \u1 u2 -> (:) (unsafeUtf8Chr_3 u0 u1 u2) <$> go

           | (u0 .&. 0x08) == 0 ->
               utf8_4 recover u0 $ \u1 u2 u3 -> (:) (unsafeUtf8Chr_4 u0 u1 u2 u3) <$> go

           | otherwise          -> anyWord8 >> recover (UTF8Error $ Invalid u0)



skipUtf8P :: DecodingHandler -> Parser ()
skipUtf8P (DecodingHandler onerr) = go
  where
    {-# INLINE recover #-}
    recover e = case onerr e of
                  Fail msg  -> err msg
                  Replace _ -> go
                  Ignore    -> go

    go = do
      u0 <- peekWord8'
      case () of
        () | (u0 .&. 0x80) == 0 -> utf8_1 recover u0 (() <$ anyWord8) (\_ -> go) (\_ -> go)

           | (u0 .&. 0x40) == 0 -> anyWord8 >> recover (UTF8Error $ Continuation u0)

           | (u0 .&. 0x20) == 0 -> utf8_2 recover u0 $ \_ -> go

           | (u0 .&. 0x10) == 0 -> utf8_3 recover u0 $ \_ _ -> go

           | (u0 .&. 0x08) == 0 -> utf8_4 recover u0 $ \_ _ _ -> go

           | otherwise          -> anyWord8 >> recover (UTF8Error $ Invalid u0)



lookupUtf8P :: DecodingHandler -> RadixTree a -> Parser (Maybe a)
lookupUtf8P handler =
  alterUtf8P' handler (const . pure)
                      $ \_ -> do skipUtf8P handler
                                 pure Nothing

updateUtf8P :: DecodingHandler -> RadixTree a -> Parser (Maybe (a, a -> RadixTree a))
updateUtf8P handler =
  alterUtf8P' handler (\val put -> pure $ flip (,) put <$> val)
                      $ \_ -> do skipUtf8P handler
                                 pure Nothing

alterUtf8P :: DecodingHandler -> RadixTree a -> Parser (Maybe a, a -> RadixTree a)
alterUtf8P handler =
  alterUtf8P' handler (\val put -> pure (val, put))
                      $ \f -> do rest <- stringUtf8P handler
                                 pure (Nothing, f rest)

{-# INLINE alterUtf8P' #-}
alterUtf8P'
  :: DecodingHandler
  -> (Maybe a -> (a -> RadixTree a) -> Parser b)
  -> ((String -> a -> RadixTree a) -> Parser b)
  -> RadixTree a -> Parser b
alterUtf8P' (DecodingHandler onerr) ret complete = advance . Radix.view
  where
    {-# INLINE advance #-}
    advance view = case view of
                     Radix.View next val put -> go next val put
                     Radix.Free f            -> complete f

    {-# INLINE recover #-}
    recover next val put e = case onerr e of
                               Fail msg     -> err msg
                               Replace char -> advance $ next char
                               Ignore       -> go next val put

    go next val put = do
      u0 <- peekWord8'
      case () of
        () | (u0 .&. 0x80) == 0 ->
               utf8_1 (recover next val put) u0
                 ( do _ <- anyWord8
                      ret val put
                 )
                 (\c -> advance $ next c)
                 (\w -> advance $ next (unsafeChr $ fromIntegral w))

           | (u0 .&. 0x40) == 0 ->
               anyWord8 >> recover next val put (UTF8Error $ Continuation u0)

           | (u0 .&. 0x20) == 0 ->
               utf8_2 (recover next val put) u0 $ \u1 ->
                 advance $ next (unsafeUtf8Chr_2 u0 u1)

           | (u0 .&. 0x10) == 0 ->
               utf8_3 (recover next val put) u0 $ \u1 u2 ->
                 advance $ next (unsafeUtf8Chr_3 u0 u1 u2)

           | (u0 .&. 0x08) == 0 ->
               utf8_4 (recover next val put) u0 $ \u1 u2 u3 ->
                 advance $ next (unsafeUtf8Chr_4 u0 u1 u2 u3)

           | otherwise          ->
               anyWord8 >> recover next val put (UTF8Error $ Invalid u0)



{-# INLINE withUtf8_2 #-}
withUtf8_2 :: Int -> (Word8 -> Word8 -> a) -> a
withUtf8_2 i f =
  f (0xC0 + (fromIntegral $ i `unsafeShiftR` 6))
    (0x80 + (fromIntegral i .&. 0x3F))

{-# INLINE withUtf8_3 #-}
withUtf8_3 :: Int -> (Word8 -> Word8 -> Word8 -> a) -> a
withUtf8_3 i f =
  f (0xE0 + (fromIntegral $ i `unsafeShiftR` 12))
    (0x80 + (fromIntegral $ i `unsafeShiftR` 6) .&. 0x3F)
    (0x80 + (fromIntegral i .&. 0x3F))

{-# INLINE withUtf8_4 #-}
withUtf8_4 :: Int -> (Word8 -> Word8 -> Word8 -> Word8 -> a) -> a
withUtf8_4 i f =
  f (0xF0 + (fromIntegral $ i `unsafeShiftR` 18))
    (0x80 + (fromIntegral $ i `unsafeShiftR` 12) .&. 0x3F)
    (0x80 + (fromIntegral $ i `unsafeShiftR` 6) .&. 0x3F)
    (0x80 + (fromIntegral i .&. 0x3F))

{-# INLINE withUtf8_replacement #-}
withUtf8_replacement :: (Word8 -> Word8 -> Word8 -> a) -> a
withUtf8_replacement f = f 0xEF 0xBF 0xBD



{-# INLINE unsafeWriteUtf8_1 #-}
unsafeWriteUtf8_1 :: Text.Copy -> Word8 -> Text.Copy
unsafeWriteUtf8_1 copy u0 =
  Text.writeCopy copy 1 $ \marr off -> unsafeWrite marr off u0

{-# INLINE unsafeWriteUtf8_2 #-}
unsafeWriteUtf8_2 :: Text.Copy -> Word8 -> Word8 -> Text.Copy
unsafeWriteUtf8_2 copy u0 u1 =
  Text.writeCopy copy 2 $ \marr off -> do unsafeWrite marr  off      u0
                                          unsafeWrite marr (off + 1) u1

{-# INLINE unsafeWriteUtf8_3 #-}
unsafeWriteUtf8_3 :: Text.Copy -> Word8 -> Word8 -> Word8 -> Text.Copy
unsafeWriteUtf8_3 copy u0 u1 u2 =
  Text.writeCopy copy 3 $ \marr off -> do unsafeWrite marr  off      u0
                                          unsafeWrite marr (off + 1) u1
                                          unsafeWrite marr (off + 2) u2

{-# INLINE unsafeWriteUtf8_4 #-}
unsafeWriteUtf8_4 :: Text.Copy -> Word8 -> Word8 -> Word8 -> Word8 -> Text.Copy
unsafeWriteUtf8_4 copy u0 u1 u2 u3 =
  Text.writeCopy copy 4 $ \marr off -> do unsafeWrite marr  off      u0
                                          unsafeWrite marr (off + 1) u1
                                          unsafeWrite marr (off + 2) u2
                                          unsafeWrite marr (off + 3) u3

{-# INLINE unsafeWriteUtf8_char #-}
unsafeWriteUtf8_char :: Text.Copy -> Char -> Text.Copy
unsafeWriteUtf8_char copy char =
  let i = ord char
  in case () of
       () | i < 0x80      -> unsafeWriteUtf8_1 copy (fromIntegral i)

          | i < 0x800     -> withUtf8_2 i $ unsafeWriteUtf8_2 copy

          | i <= 0xFFFF   -> withUtf8_3 i $ unsafeWriteUtf8_3 copy

          | i <= 0x10FFFF -> withUtf8_4 i $ unsafeWriteUtf8_4 copy

          | otherwise     -> withUtf8_replacement $ unsafeWriteUtf8_3 copy



textUtf8P :: DecodingHandler -> Parser Text
textUtf8P (DecodingHandler onerr) = go Text.emptyCopy
  where
    {-# INLINE recover #-}
    recover copy e =
      case onerr e of
        Fail msg     -> err msg
        Replace char -> go $ unsafeWriteUtf8_char copy char
        Ignore       -> go copy

    go !copy = do
      u0 <- peekWord8'
      case () of
        () | (u0 .&. 0x80) == 0 ->
               utf8_1 (recover copy) u0
                 ( let !txt = Text.commitCopy copy
                   in txt <$ anyWord8
                 )
                 (go . unsafeWriteUtf8_char copy)
                 (go . unsafeWriteUtf8_1 copy)

           | (u0 .&. 0x40) == 0 ->
               anyWord8 >> recover copy (UTF8Error $ Continuation u0)

           | (u0 .&. 0x20) == 0 ->
               utf8_2 (recover copy) u0 $ \u1 ->
                 go $ unsafeWriteUtf8_2 copy u0 u1

           | (u0 .&. 0x10) == 0 ->
               utf8_3 (recover copy) u0 $ \u1 u2 ->
                 go $ unsafeWriteUtf8_3 copy u0 u1 u2

           | (u0 .&. 0x08) == 0 ->
               utf8_4 (recover copy) u0 $ \u1 u2 u3 ->
                 go $ unsafeWriteUtf8_4 copy u0 u1 u2 u3

           | otherwise          ->
               anyWord8 >> recover copy (UTF8Error $ Invalid u0)



textChunkUtf8P :: DecodingHandler -> Int -> Parser (Bool, Text)
textChunkUtf8P (DecodingHandler onerr) = go Text.emptyCopy
  where
    {-# INLINE recover #-}
    recover copy i e =
      case onerr e of
        Fail msg     -> err msg
        Replace char -> go (unsafeWriteUtf8_char copy char) (i - 1)
        Ignore       -> go copy i

    go !copy i
      | i < 0     = let !txt = Text.commitCopy copy
                    in pure (False, txt)
      | otherwise = do
          u0 <- peekWord8'
          case () of
            () | (u0 .&. 0x80) == 0 ->
                   utf8_1 (recover copy i) u0
                     ( let !txt = Text.commitCopy copy
                       in pure (True, txt)
                     )
                     (\c -> go (unsafeWriteUtf8_char copy c) (i - 1))
                     (\w -> go (unsafeWriteUtf8_1 copy w) (i - 1))

               | (u0 .&. 0x40) == 0 ->
                   anyWord8 >> recover copy i (UTF8Error $ Continuation u0)

               | (u0 .&. 0x20) == 0 ->
                   utf8_2 (recover copy i) u0 $ \u1 ->
                     go (unsafeWriteUtf8_2 copy u0 u1) (i - 1)

               | (u0 .&. 0x10) == 0 ->
                   utf8_3 (recover copy i) u0 $ \u1 u2 ->
                     go (unsafeWriteUtf8_3 copy u0 u1 u2) (i - 1)

               | (u0 .&. 0x08) == 0 ->
                   utf8_4 (recover copy i) u0 $ \u1 u2 u3 ->
                     go (unsafeWriteUtf8_4 copy u0 u1 u2 u3) (i - 1)

               | otherwise          ->
                   anyWord8 >> recover copy i (UTF8Error $ Invalid u0)

lazyTextUtf8P :: DecodingHandler -> Int -> Parser Lazy.Text
lazyTextUtf8P handler len = go
  where
    go = do
      (done, chunk) <- textChunkUtf8P handler len
      if done
        then Lazy.chunk chunk Lazy.empty <$ anyWord8
        else Lazy.chunk chunk <$> go




{-# INLINE pokeUtf8_1 #-}
pokeUtf8_1 :: BS.Copy -> Word8 -> BS.Copy
pokeUtf8_1 copy u0 =
  BS.writeCopy copy 1 $ \ptr off -> poke (plusPtr ptr off) u0

{-# INLINE pokeUtf8_2 #-}
pokeUtf8_2 :: BS.Copy -> Word8 -> Word8 -> BS.Copy
pokeUtf8_2 copy u0 u1 =
  BS.writeCopy copy 2 $ \ptr off -> do poke (plusPtr ptr   off    ) u0
                                       poke (plusPtr ptr $ off + 1) u1

{-# INLINE pokeUtf8_3 #-}
pokeUtf8_3 :: BS.Copy -> Word8 -> Word8 -> Word8 -> BS.Copy
pokeUtf8_3 copy u0 u1 u2 =
  BS.writeCopy copy 3 $ \ptr off -> do poke (plusPtr ptr   off    ) u0
                                       poke (plusPtr ptr $ off + 1) u1
                                       poke (plusPtr ptr $ off + 2) u2

{-# INLINE pokeUtf8_4 #-}
pokeUtf8_4 :: BS.Copy -> Word8 -> Word8 -> Word8 -> Word8 -> BS.Copy
pokeUtf8_4 copy u0 u1 u2 u3 =
  BS.writeCopy copy 4 $ \ptr off -> do poke (plusPtr ptr   off    ) u0
                                       poke (plusPtr ptr $ off + 1) u1
                                       poke (plusPtr ptr $ off + 2) u2
                                       poke (plusPtr ptr $ off + 3) u3

{-# INLINE pokeUtf8_char #-}
pokeUtf8_char :: BS.Copy -> Char -> BS.Copy
pokeUtf8_char copy char =
  let i = ord char
  in case () of
       () | i < 0x80      -> pokeUtf8_1 copy (fromIntegral i)

          | i < 0x800     -> withUtf8_2 i $ pokeUtf8_2 copy

          | i <= 0xFFFF   -> withUtf8_3 i $ pokeUtf8_3 copy

          | i <= 0x10FFFF -> withUtf8_4 i $ pokeUtf8_4 copy

          | otherwise     -> withUtf8_replacement $ pokeUtf8_3 copy



byteStringUtf8P :: DecodingHandler -> Parser BS.ByteString
byteStringUtf8P (DecodingHandler onerr) = go BS.emptyCopy
  where
    {-# INLINE recover #-}
    recover copy e =
      case onerr e of
        Fail msg     -> err msg
        Replace char -> go $ pokeUtf8_char copy char
        Ignore       -> go copy

    go !copy = do
      u0 <- peekWord8'
      case () of
        () | (u0 .&. 0x80) == 0 ->
               utf8_1 (recover copy) u0
                 ( let !bs = BS.commitCopy copy
                   in bs <$ anyWord8
                 )
                 (go . pokeUtf8_char copy)
                 (go . pokeUtf8_1    copy)

           | (u0 .&. 0x40) == 0 ->
               anyWord8 >> recover copy (UTF8Error $ Continuation u0)

           | (u0 .&. 0x20) == 0 ->
               utf8_2 (recover copy) u0 $ \u1 ->
                 go $ pokeUtf8_2 copy u0 u1

           | (u0 .&. 0x10) == 0 ->
               utf8_3 (recover copy) u0 $ \u1 u2 ->
                 go $ pokeUtf8_3 copy u0 u1 u2

           | (u0 .&. 0x08) == 0 ->
               utf8_4 (recover copy) u0 $ \u1 u2 u3 ->
                 go $ pokeUtf8_4 copy u0 u1 u2 u3

           | otherwise          ->
               anyWord8 >> recover copy (UTF8Error $ Invalid u0)



byteStringChunkUtf8P :: DecodingHandler -> Int -> Parser (Bool, BS.ByteString)
byteStringChunkUtf8P (DecodingHandler onerr) = go BS.emptyCopy
  where
    {-# INLINE recover #-}
    recover copy i e =
      case onerr e of
        Fail msg     -> err msg
        Replace char -> go (pokeUtf8_char copy char) (i - 1)
        Ignore       -> go copy i

    go !copy i
      | i <= 0    = let !bs = BS.commitCopy copy
                    in pure (False, bs)
      | otherwise = do
          u0 <- peekWord8'
          case () of
            () | (u0 .&. 0x80) == 0 ->
                   utf8_1 (recover copy i) u0
                     ( let !bs = BS.commitCopy copy
                       in pure (True, bs)
                     )
                     (\c -> go (pokeUtf8_char copy c) (i - 1))
                     (\w -> go (pokeUtf8_1 copy w) (i - 1))

               | (u0 .&. 0x40) == 0 ->
                   anyWord8 >> recover copy i (UTF8Error $ Continuation u0)

               | (u0 .&. 0x20) == 0 ->
                   utf8_2 (recover copy i) u0 $ \u1 ->
                     go (pokeUtf8_2 copy u0 u1) (i - 1)

               | (u0 .&. 0x10) == 0 ->
                   utf8_3 (recover copy i) u0 $ \u1 u2 ->
                     go (pokeUtf8_3 copy u0 u1 u2) (i - 1)

               | (u0 .&. 0x08) == 0 ->
                   utf8_4 (recover copy i) u0 $ \u1 u2 u3 ->
                     go (pokeUtf8_4 copy u0 u1 u2 u3) (i - 1)

               | otherwise          ->
                   anyWord8 >> recover copy i (UTF8Error $ Invalid u0)

lazyByteStringUtf8P :: DecodingHandler -> Int -> Parser BSL.ByteString
lazyByteStringUtf8P handler len = go
  where
    go = do
      (done, chunk) <- byteStringChunkUtf8P handler len
      if done
        then BSL.chunk chunk BSL.empty <$ anyWord8
        else BSL.chunk chunk <$> go



{-# INLINE pokeLazyUtf8_1 #-}
pokeLazyUtf8_1 :: Int -> BSL.Copy -> Word8 -> BSL.Copy
pokeLazyUtf8_1 len copy u0 =
  BSL.writeCopy len copy 1 $ \ptr off -> poke (plusPtr ptr off) u0

{-# INLINE pokeLazyUtf8_2 #-}
pokeLazyUtf8_2 :: Int -> BSL.Copy -> Word8 -> Word8 -> BSL.Copy
pokeLazyUtf8_2 len copy u0 u1 =
  BSL.writeCopy len copy 2 $ \ptr off -> do poke (plusPtr ptr   off    ) u0
                                            poke (plusPtr ptr $ off + 1) u1

{-# INLINE pokeLazyUtf8_3 #-}
pokeLazyUtf8_3 :: Int -> BSL.Copy -> Word8 -> Word8 -> Word8 -> BSL.Copy
pokeLazyUtf8_3 len copy u0 u1 u2 =
  BSL.writeCopy len copy 3 $ \ptr off -> do poke (plusPtr ptr   off    ) u0
                                            poke (plusPtr ptr $ off + 1) u1
                                            poke (plusPtr ptr $ off + 2) u2

{-# INLINE pokeLazyUtf8_4 #-}
pokeLazyUtf8_4 :: Int -> BSL.Copy -> Word8 -> Word8 -> Word8 -> Word8 -> BSL.Copy
pokeLazyUtf8_4 len copy u0 u1 u2 u3 =
  BSL.writeCopy len copy 4 $ \ptr off -> do poke (plusPtr ptr   off    ) u0
                                            poke (plusPtr ptr $ off + 1) u1
                                            poke (plusPtr ptr $ off + 2) u2
                                            poke (plusPtr ptr $ off + 3) u3



{-# INLINE copyUtf16_escape #-}
copyUtf16_escape
  :: Int
  -> BSL.Copy
  -> (BSL.Copy -> DecodingError -> Parser BSL.Copy)
  -> (BSL.Copy -> Parser BSL.Copy)
  -> Parser BSL.Copy
copyUtf16_escape chunklen copy0 recover f = do
  let {-# INLINE satisfyHex #-}
      satisfyHex = do
        w <- peekWord8'
        case toHex w of
          Just a  -> (a, w) <$ anyWord8
          Nothing -> err $ "Unexpected byte 0x" <> showHex w " in UTF-16 escaped literal"

  (a, a_) <- satisfyHex
  (b, b_) <- satisfyHex
  (c, c_) <- satisfyHex
  (d, d_) <- satisfyHex

  let copy1 = (\copy' -> pokeLazyUtf8_4 chunklen copy' a_ b_ c_ d_)
            $ pokeLazyUtf8_2 chunklen copy0 0x5C 0x75

      {-# INLINE hi #-}
      hi :: (Bits a, Num a) => a
      hi = unsafeShiftL (fromIntegral a) 12
         + unsafeShiftL (fromIntegral b) 8
         + unsafeShiftL (fromIntegral c) 4
         +               fromIntegral d

  if a == 0x0D && b >= 0x08
    then
      if b >= 0x0C
        then recover copy1 (UTF16Error $ Invalid1 hi)
        else do
          s <- peekWord8'
          if s == 0x5C {- \ -}
            then () <$ anyWord8
            else err $ "Unexpected byte 0x" <> showHex s " in a string"

          u <- peekWord8'
          if u == 0x75 {- u -}
            then () <$ anyWord8
            else err $ "Unexpected byte 0x" <> showHex u " after character escape"

          (w, w_) <- satisfyHex
          (x, x_) <- satisfyHex
          (y, y_) <- satisfyHex
          (z, z_) <- satisfyHex

          let lo = unsafeShiftL (fromIntegral w) 12
                 + unsafeShiftL (fromIntegral x) 8
                 + unsafeShiftL (fromIntegral y) 4
                 +               fromIntegral z

              copy2 = (\copy' -> pokeLazyUtf8_4 chunklen copy' w_ x_ y_ z_)
                    $ pokeLazyUtf8_2 chunklen copy1 0x5C 0x75

          if w == 0x0D && x >= 0x0C
            then f copy2
            else recover copy2 (UTF16Error $ Invalid2 hi lo)

     else f copy1



copyByteStringUtf8P :: DecodingHandler -> Int -> BSL.Copy -> Parser BSL.Copy
copyByteStringUtf8P (DecodingHandler onerr) chunklen = go
  where
    {-# INLINE recover #-}
    recover copy e =
      case onerr e of
        Fail msg     -> err msg
        _            -> go copy

    go !copy0 = do
      u0 <- peekWord8'
      let copy1 = pokeLazyUtf8_1 chunklen copy0 u0
      case () of
        () | (u0 .&. 0x80) == 0 ->
               if u0 >= 0x20
                 then
                   case u0 of
                     0x22 {- " -} -> copy1 <$ anyWord8

                     0x5c {- \ -} -> do
                       _ <- anyWord8
                       u1 <- peekWord8'
                       if    u1 == 0x22 || u1 == 0x2F || u1 == 0x5C || u1 == 0x62
                          || u1 == 0x66 || u1 == 0x6E || u1 == 0x72 || u1 == 0x74
                         then do
                           _ <- anyWord8
                           go $ pokeLazyUtf8_2 chunklen copy0 u0 u1

                         else
                           if u1 == 0x75
                             then do
                               _ <- anyWord8
                               copyUtf16_escape chunklen copy0 recover go

                             else err $ "Unexpected byte 0x"
                                     <> showHex u0 " after character escape"

                     _            -> do
                       _ <- anyWord8
                       go copy1

                else err $ "Unexpected byte 0x" <> showHex u0 " in a string"

           | (u0 .&. 0x40) == 0 -> do
               _ <- anyWord8
               recover copy1 (UTF8Error $ Continuation u0)

           | (u0 .&. 0x20) == 0 -> do
               _ <- anyWord8
               if (u0 .&. 0x1F) < 0x02
                 then recover copy1 (UTF8Error $ Overlong2 u0)
                 else do
                   u1 <- peekWord8'
                   if (u1 .&. 0xC0) /= 0x80
                     then recover copy1 (UTF8Error $ Incomplete22 u0 u1)
                     else do
                       _ <- anyWord8
                       go $ pokeLazyUtf8_2 chunklen copy0 u0 u1

           | (u0 .&. 0x10) == 0 -> do
               _ <- anyWord8
               u1 <- peekWord8'
               if (u1 .&. 0xC0) /= 0x80
                 then recover copy1 (UTF8Error $ Incomplete23 u0 u1)
                 else
                   if (u0 .&. 0x0F) == 0x0D && (u1 .&. 0x20) /= 0
                     then recover copy1 (UTF8Error $ Surrogate u1)
                     else
                       if (u0 .&. 0x0F) == 0 && (u1 .&. 0x20) == 0
                         then recover copy1 (UTF8Error $ Overlong3 u1)
                         else do
                           _ <- anyWord8
                           u2 <- peekWord8'
                           if (u2 .&. 0xC0) /= 0x80
                             then recover (pokeLazyUtf8_2 chunklen copy0 u0 u1)
                                          (UTF8Error $ Incomplete33 u0 u1 u2)
                             else do
                               _ <- anyWord8
                               go $ pokeLazyUtf8_3 chunklen copy0 u0 u1 u2

           | (u0 .&. 0x08) == 0 -> do
               _ <- anyWord8
               if (u0 .&. 0x07) > 0x04
                 then recover copy1 (UTF8Error $ Overflow1 u0)
                 else do
                   u1 <- peekWord8'
                   if (u1 .&. 0xC0) /= 0x80
                     then recover copy1 (UTF8Error $ Incomplete24 u0 u1)
                     else
                       if (u0 .&. 0x07) == 0x04 && (u1 .&. 0x3F) >= 0x10
                         then recover copy1 (UTF8Error $ Overflow2 u1)
                         else
                           if (u0 .&. 0x07) == 0x00 && (u1 .&. 0x3F) < 0x10
                             then recover copy1 (UTF8Error $ Overlong4 u1)
                             else do
                               _ <- anyWord8
                               let copy2 = pokeLazyUtf8_2 chunklen copy0 u0 u1
                               u2 <- peekWord8'
                               if (u2 .&. 0xC0) /= 0x80
                                 then recover copy2 (UTF8Error $ Incomplete34 u0 u1 u2)
                                 else do
                                   _ <- anyWord8
                                   u3 <- peekWord8'
                                   if (u3 .&. 0xC0) /= 0x80
                                     then recover (pokeLazyUtf8_3 chunklen copy0 u0 u1 u2)
                                                  (UTF8Error $ Incomplete4 u0 u1 u2 u3)
                                     else do
                                       _ <- anyWord8
                                       go $ pokeLazyUtf8_4 chunklen copy0 u0 u1 u2 u3

           | otherwise          -> do
               _ <- anyWord8
               recover copy1 (UTF8Error $ Invalid u0)
