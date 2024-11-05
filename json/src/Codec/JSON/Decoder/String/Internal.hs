{-# LANGUAGE BangPatterns
           , OverloadedStrings
           , RankNTypes
           , UnboxedTuples #-}

{-# OPTIONS_HADDOCK hide #-}

module Codec.JSON.Decoder.String.Internal
  ( SurrogateHandling_ (..)
  , stringP_

  , SurrogateHandling (..)
  , stringP

  , Chunk (..)
  , chunkP

  , embedP
  ) where

import           Codec.JSON.Decoder.Internal

import           Control.Monad.ST
import           Control.Monad.ST.Unsafe
import           Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Internal as LB (ByteString (..))
import qualified Data.ByteString.Unsafe as B
import           Data.Primitive.ByteArray
import qualified Data.Text.Array as Array
import           Data.Text.Encoding
import qualified Data.Text.Internal as T
import           Data.Word
import           GHC.Base (unsafeChr)
import           Parser.Lathe
import qualified Parser.Lathe.Binary as Lathe (word8)
import           Parser.Lathe.Encoding.UTF8
import           Parser.Lathe.Numeric.FixedWidth.Internal
import           Parser.Lathe.Unsafe (Res (..), unsafeRead)



missingHex4, badHighSurrogate, missingSurrogate, badLowSurrogate, wellFormedOnly :: String
missingHex4      = "JSON string UTF-16 escape must be followed by four hexadecimal digits"
badHighSurrogate = "First surrogate in a JSON string UTF-16 character escape is not high"
missingSurrogate = "JSON string UTF-16 high surrogate escape must be followed by a low surrogate"
badLowSurrogate  = "Second surrogate in a JSON string UTF-16 character escape is not low"
wellFormedOnly   = "JSON string contains surrogates"


jsonUtf16 :: Path -> Parser (Path, Error) Word32
jsonUtf16 =
  jsonUtf16'
    (\u0    -> pure $! fromIntegral u0)
    (\u0 u1 -> let !w = 0x000010000
                      + ((fromIntegral u0 - 0xD800) `unsafeShiftL` 10)
                      +  (fromIntegral u1 - 0xDC00)

               in (# Yes w #))

jsonUtf16_ :: Path -> Parser (Path, Error) ()
jsonUtf16_ = jsonUtf16' (\_ -> pure ()) (\_ _ -> (# Yes () #))


{-# INLINE jsonUtf16' #-}
jsonUtf16'
  :: (Word16 -> Parser (Path, Error) a)
  -> (Word16 -> Word16 -> (# Res (Path, Error) a #))
  -> Path
  -> Parser (Path, Error) a
jsonUtf16' conv1 conv2 path = do
  u0 <- unsafeRead 4
          ( \b -> case w16HexFixed () b of
                    (# Yes i #) -> (# Yes i #)
                    (# No _ #) -> (# No (path, Malformed Fatal missingHex4) #)
          )
          (path, Malformed Fatal missingHex4)

  if (u0 .&. 0xF800) /= 0xD800
    then conv1 u0
    else
      if u0 >= 0xDC00
        then err (path, Malformed Fatal badHighSurrogate)
        else
          unsafeRead 6
            ( \b ->
                if B.unsafeIndex b 0 == 0x5C && B.unsafeIndex b 1 == 0x75
                  then case w16HexFixed () (B.unsafeDrop 2 b) of
                         (# Yes u1 #) ->
                           if (u1 .&. 0xFC00) == 0xDC00
                             then conv2 u0 u1
                             else (# No (path, Malformed Fatal badLowSurrogate) #)

                         (# No _ #) -> (# No (path, Malformed Fatal missingHex4) #)

                  else (# No (path, Malformed Fatal missingSurrogate) #)
            )
            (path, Malformed Fatal missingSurrogate)



data SurrogateHandling = Replace
                       | Complain
                       | Preserve


data Bite = Edge
              {-# UNPACK #-} !Int -- ^ Byte count

          | Escape
              {-# UNPACK #-} !Int -- ^ Byte count excluding the backslash

          | Quotes
              {-# UNPACK #-} !Int -- ^ Byte count excluding the quotes

          | Surrogate
              {-# UNPACK #-} !Int -- ^ Byte count excluding the surrogate
            deriving Show

malformedUTF8, nonStandardEscape, unescapedControl :: String
malformedUTF8     = "JSON string contains malformed UTF-8 characters"
nonStandardEscape = "JSON string contains a non-standard escape sequence"
unescapedControl  = "JSON string contains unescaped control codes"



biteP :: SurrogateHandling -> Path -> Int -> Parser (Path, Error) Bite
biteP handl path len = go 0
  where
    malformed = (path, Malformed Fatal malformedUTF8)

    go !n
      | n >= len  = pure $ Edge n
      | otherwise = do
          u <- unitUtf8 (\_ -> malformed)
                        (path, AbruptEnd)
          case u of
            UTF8_1 (UTF8Point w) ->
              case w of
                0x22 -> pure $ Quotes n
                0x5C -> pure $ Escape n
                _    ->
                  if w < 0x20
                    then err (path, Malformed Fatal unescapedControl)
                    else go (n + 1)

            _ -> case u of
                   UTF8_2 u2 -> do
                     skipUtf8_2 malformed (path, AbruptEnd) u2
                     go (n + 2)

                   UTF8_3 u3 -> do
                     isSurr <- skipUtf8_3 (\_ -> malformed) (path, AbruptEnd) u3
                     let !n' = n + 3
                     if isSurr
                       then case handl of
                              Replace  -> pure $ Surrogate n
                              Complain -> err (path, Malformed Benign wellFormedOnly)
                              Preserve -> go n'

                       else go n'

                   UTF8_4 u4 -> do
                     skipUtf8_4 (\_ -> malformed) (path, AbruptEnd) u4
                     go (n + 4)



data SurrogateHandling_ = Complain_
                        | Preserve_

data Bite_ = Escape_
           | Quotes_
             deriving Show

biteP_ :: SurrogateHandling_ -> Path -> Parser (Path, Error) Bite_
biteP_ handl path = go
  where
    malformed = (path, Malformed Fatal malformedUTF8)

    go = do
      u <- unitUtf8 (\_ -> malformed)
                    (path, AbruptEnd)
      case u of
        UTF8_1 (UTF8Point w) ->
          case w of
            0x22 -> pure Quotes_
            0x5C -> pure Escape_
            _    ->
              if w < 0x20
                then err (path, Malformed Fatal unescapedControl)
                else go

        _ -> case u of
               UTF8_2 u2 -> do
                 skipUtf8_2 malformed (path, AbruptEnd) u2
                 go

               UTF8_3 u3 -> do
                 isSurr <- skipUtf8_3 (\_ -> malformed) (path, AbruptEnd) u3
                 if isSurr
                   then case handl of
                          Complain_ -> err (path, Malformed Benign wellFormedOnly)
                          Preserve_ -> go

                   else go

               UTF8_4 u4 -> do
                 skipUtf8_4 (\_ -> malformed) (path, AbruptEnd) u4
                 go



-- | Skips JSON string contents until (and including) the double quotes.
stringP_ :: SurrogateHandling_ -> Path -> Parser (Path, Error) ()
stringP_ handl path = go
  where
    go = do
      b <- biteP_ handl path
      case b of
        Quotes_ -> pure ()
        Escape_ -> do
          w <- Lathe.word8 (path, AbruptEnd)
          if w == 0x22 || w == 0x5C || w == 0x2F || w == 0x62
                       || w == 0x66 || w == 0x6E || w == 0x72 || w == 0x74
            then go
            else if w == 0x75
                   then do
                     jsonUtf16_ path
                     go

                   else err (path, Malformed Fatal nonStandardEscape)



stringP :: SurrogateHandling -> Path -> Parser (Path, Error) String
stringP handl path = go id
  where
    malformed = (path, Malformed Fatal malformedUTF8)

    go acc = do
      u <- unitUtf8 (\_ -> malformed)
                    (path, AbruptEnd)
      case u of
        UTF8_1 u1@(UTF8Point w0)
          | w0 < 0x20  -> err (path, Malformed Fatal unescapedControl)
          | w0 == 0x22 -> pure $! acc []
          | w0 == 0x5C -> do
              w1 <- Lathe.word8 (path, AbruptEnd)
              case w1 of
                0x22 -> go (acc . (:) '\x22')
                0x5C -> go (acc . (:) '\x5C')
                0x2F -> go (acc . (:) '\x2F')
                0x62 -> go (acc . (:) '\x08')
                0x66 -> go (acc . (:) '\x0C')
                0x6E -> go (acc . (:) '\x0A')
                0x72 -> go (acc . (:) '\x0D')
                0x74 -> go (acc . (:) '\x09')
                0x75 -> do w2 <- jsonUtf16 path
                           let !c = unsafeChr (fromIntegral w2)
                           go (acc . (:) c)

                _    -> err (path, Malformed Fatal nonStandardEscape)

          | otherwise -> let !c = fromUtf8 u1
                         in go (acc . (:) c)

        UTF8_2 u2 -> do c <- contUtf8_2 malformed (path, AbruptEnd) u2
                        go (acc . (:) c)

        UTF8_3 u3 -> do r <- contUtf8_3 (\_ -> malformed) (path, AbruptEnd) u3
                        if isSurrogate r
                          then case handl of
                                 Replace  -> go (acc . (:) '\xFFFD')
                                 Complain -> err (path, Malformed Benign wellFormedOnly)
                                 Preserve -> let !c = fromUtf8 r
                                             in go (acc . (:) c)

                          else let !c = fromUtf8 r
                               in go (acc . (:) c)

        UTF8_4 u4 -> do c <- contUtf8_4 (\_ -> malformed) (path, AbruptEnd) u4
                        go (acc . (:) c)




-- Blindlly copies a bytestring in its entirety to the offset.
lbsCopy :: MutableByteArray s -> Int -> LB.ByteString -> ST s ()
lbsCopy !marr = go
  where
    go !n lbs =
      case lbs of
        LB.Chunk bs lbs' -> do
          let len = B.length bs

          unsafeIOToST $
            B.unsafeUseAsCString bs $ \ptr -> do
              unsafeSTToIO $
                copyPtrToMutableByteArray marr n ptr len

          go (n + len) lbs'

        LB.Empty   -> pure ()


-- Returns both the number of bytes needed to represent and the operation itself.
utf8Mold :: Word32 -> (# Int, MutableByteArray s -> Int -> ST s () #)
utf8Mold i
  | i <  0x0080 =
      (# 1, \marr n -> writeByteArray marr n (fromIntegral i :: Word8) #)

  | i <  0x0800 =
      (# 2, \marr n -> do let u0 = 0xC0 + fromIntegral (i `unsafeShiftR` 6)
                              u1 = 0x80 + fromIntegral (i .&. 0x3F)

                          writeByteArray marr  n      (u0 :: Word8)
                          writeByteArray marr (n + 1) (u1 :: Word8) #)

  | i < 0x10000 =
      (# 3, \marr n -> do let u0 = 0xE0 + fromIntegral  (i `unsafeShiftR` 12)
                              u1 = 0x80 + fromIntegral ((i `unsafeShiftR` 6)  .&. 0x3F)
                              u2 = 0x80 + fromIntegral  (i                    .&. 0x3F)

                          writeByteArray marr  n      (u0 :: Word8)
                          writeByteArray marr (n + 1) (u1 :: Word8)
                          writeByteArray marr (n + 2) (u2 :: Word8) #)

  | otherwise   =
      (# 4, \marr n -> do let u0 = 0xF0 + fromIntegral  (i `unsafeShiftR` 18)
                              u1 = 0x80 + fromIntegral ((i `unsafeShiftR` 12) .&. 0x3F)
                              u2 = 0x80 + fromIntegral ((i `unsafeShiftR` 6)  .&. 0x3F)
                              u3 = 0x80 + fromIntegral  (i                    .&. 0x3F)

                          writeByteArray marr  n      (u0 :: Word8)
                          writeByteArray marr (n + 1) (u1 :: Word8)
                          writeByteArray marr (n + 2) (u2 :: Word8)
                          writeByteArray marr (n + 3) (u3 :: Word8) #)


-- Allocates a chunk and executes the operations.
petrify
  :: (forall s. MutableByteArray s -> ST s ())
  -> Int                                       -- ^ Allocation size
  -> Int                                       -- ^ Visible size
  -> T.Text
petrify ps len n =
  let !(ByteArray arr) = runST $ do
                           marr <- newByteArray len
                           ps marr
                           unsafeFreezeByteArray marr

  in T.Text (Array.ByteArray arr) 0 n




data Chunk = Chunk
               {-# UNPACK #-} !More
               !T.Text              -- ^ Non-empty.

           | Nil
             deriving Show

-- | Parses single chunk of a JSON string up to the given length in size.
--   Closing double quotes are consumed before returning the last chunk.
--
--   Provided chunk length __must__ be greater than or equal to 3.

--   A chunk is built out of four basic blocks:
--
--   - Consecutive pieces of plain UTF-8 text. These are copied to the chunk in bulk.
--
--   - Escape sequences. Previous match contains the backslash,
--     which is written to the chunk, but is immediately overwritten by the
--     escaped character.
--
--   - Double quotes. Previous match contains the double quotes, which is
--     then left hanging in the resulting allocation.
--
--   - Surrogate characters. Previous match contains the surrogate,
--     which is written to the chunk, but is immediately overwritten by @U+FFFD@.
chunkP :: SurrogateHandling -> Path -> Int -> Parser (Path, Error) Chunk
chunkP handl path len = chunk (\_ -> pure ()) 0
  where
    chunk
      :: (forall s. MutableByteArray s -> ST s ())
      -> Int
      -> Parser (Path, Error) Chunk
    chunk ps !n = do
      (lbs, res) <- match $ biteP handl path ((len - 3) - n)
      case res of
        Edge n' ->
          let !(# ps' #) | n' == 0   = (# ps #)
                         | otherwise = (# \marr -> do lbsCopy marr n lbs
                                                      ps marr #)

              !m = n + n'

              !c = petrify ps' m m

          in pure $ Chunk More c

        Escape n' -> do
          w <- do w1 <- Lathe.word8 (path, AbruptEnd)
                  case w1 of
                    0x22 -> pure 0x22
                    0x5C -> pure 0x5C
                    0x2F -> pure 0x2F
                    0x62 -> pure 0x08
                    0x66 -> pure 0x0C
                    0x6E -> pure 0x0A
                    0x72 -> pure 0x0D
                    0x74 -> pure 0x09
                    0x75 -> jsonUtf16 path
                    _    -> err (path, Malformed Fatal nonStandardEscape)

          let !(# delta, escape #) = utf8Mold w

              !m = n + n'

              ps' marr = do
                lbsCopy marr n lbs
                escape marr m
                ps marr

          chunk ps' (m + delta)

        Quotes n' ->
          let !m = n + n'

              !c | m == 0    = Nil
                 | otherwise =
                     let !(# ps' #) | n' == 0   = (# ps #)
                                    | otherwise = (# \marr -> do lbsCopy marr n lbs
                                                                 ps marr #)

                     in Chunk End $ petrify ps' (m + 1) m

          in pure c

        Surrogate n' ->
          let !m = n + n'

              ps' marr = do
                lbsCopy marr n lbs
                writeByteArray marr  m      (0xEF :: Word8)
                writeByteArray marr (m + 1) (0xBF :: Word8)
                writeByteArray marr (m + 2) (0xBD :: Word8)
                ps marr

          in chunk ps' (m + 3)




-- | Parse a JSON string, feeding parsed chunks of given size into the given parser.
--
--   The parser must only return 'Benign' errors with a 'Path' that is compatible
--   with the input one.
--
--   This function automatically consumes any remaining input if the given parser
--   succeeds early.
embedP :: Int -> Path -> Parser (Path, Error) a -> Parser (Path, Error) a
embedP len path parser = do
  skipEndOr1

  chunk <- chunkP Preserve path len
  case chunk of
    Chunk more txt ->
      let !blank0 = prepare 0 (encodeUtf8 txt) LB.empty more
      in go more (draw parser blank0)

    Nil            -> do
      case draw parser $ prepare 0 B.empty LB.empty End of
        Partial _    ->
          let str = "Embedded JSON string parser was passed an empty string, \
                    \yet requested more input"
          in err (path, Malformed Benign str)

        Done (_, ei) ->
          case ei of
            Right a -> pure a
            Left e  -> err e
  where
    go more x =
      case x of
        Partial resupply ->
          case more of
            End  -> go End $ resupply EndOfInput
            More -> do
              chunk <- chunkP Preserve path len
              case chunk of
                Nil              -> go End $ resupply EndOfInput
                Chunk more' txt' -> go more' $ resupply (Supply $ encodeUtf8 txt')

        Done (_, ei)     ->
          case ei of
            Right a -> a <$ case more of
                              More -> stringP_ Preserve_ path
                              End  -> pure ()

            Left e  -> err e
