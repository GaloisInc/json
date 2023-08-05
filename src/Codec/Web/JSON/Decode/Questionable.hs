{- | 'Decoder's for parsing data encoded in an unconventional way.
 -}

module Codec.Web.JSON.Decode.Questionable
  ( -- * Number
    -- ** Integer
    -- *** Unsigned
    Codec.Web.JSON.Decode.Questionable.word8
  , word16
  , word32
  , word64
  , word
  , natural
    -- *** Signed
  , int8
  , int16
  , int32
  , int64
  , int
  , integer
    -- ** Floating-point
  , float
  , double
  , scientific

    -- * Boolean
  , stringBoolean

    -- * Null
  , stringNull
  ) where

import           Codec.Web.JSON.Decode.Core
import           Codec.Web.JSON.Parse.Boolean
import           Codec.Web.JSON.Parse.Number
import           Codec.Web.JSON.Parse.Null
import           Data.Attoparsec.Error

import           Data.Attoparsec.ByteString as Atto
import           Data.Int
import           Data.List.NonEmpty
import           Data.Scientific (Scientific)
import           Data.Word
import           Numeric
import           Numeric.Natural



{-# INLINE withQuestionableNumber #-}
withQuestionableNumber :: Parser a -> Decoder a
withQuestionableNumber f =
  withValue $ \v ->
    case v of
      N -> f
      S -> do _ <- anyWord8 -- 0x22 {- " -}
              n <- f
              q <- peekWord8'
              if q == 0x22
                then n <$ anyWord8
                else err $ "Unexpected byte 0x"
                        <> showHex q " in a number encoded as a JSON string"

      _ -> err $ expectedFound (N :| [S]) v



-- | Parses a JSON number or string as a 'Word8'.
--
--   Negative zero is a valid input.
word8 :: Decoder Word8
word8 = withQuestionableNumber word8P

-- | Parses a JSON number or string as a 'Word16'.
--
--   Negative zero is a valid input.
word16 :: Decoder Word16
word16 = withQuestionableNumber word16P

-- | Parses a JSON number or string as a 'Word32'.
--
--   Negative zero is a valid input.
word32 :: Decoder Word32
word32 = withQuestionableNumber word32P

-- | Parses a JSON number or string as a 'Word64'.
--
--   Negative zero is a valid input.
word64 :: Decoder Word64
word64 = withQuestionableNumber word64P

-- | Parses a JSON number or string as a 'Word'.
--
--   Negative zero is a valid input.
word :: Decoder Word
word = withQuestionableNumber wordP

-- | Parses a JSON number or string as a 'Natural'.
--
--   Negative zero is a valid input.
natural :: Decoder Natural
natural = withQuestionableNumber naturalP



-- | Parses a JSON number or string as an 'Int8'.
int8 :: Decoder Int8
int8 = withQuestionableNumber int8P

-- | Parses a JSON number or string as an 'Int16'.
int16 :: Decoder Int16
int16 = withQuestionableNumber int16P

-- | Parses a JSON number or string as an 'Int32'.
int32 :: Decoder Int32
int32 = withQuestionableNumber int32P

-- | Parses a JSON number or string as an 'Int64'.
int64 :: Decoder Int64
int64 = withQuestionableNumber int64P

-- | Parses a JSON number or string as an 'Int'.
int :: Decoder Int
int = withQuestionableNumber intP

-- | Parses a JSON number or string as an 'Integer'.
integer :: Decoder Integer
integer = withQuestionableNumber integerP



-- | Parses a JSON number or string as a 'Float'.
--
--   Returns infinity or zero if the number is too large or too small respectively.
--   Honors negative zero.
float :: Decoder Float
float = withQuestionableNumber floatP

-- | Parses a JSON number or string as a 'Double'.
--
--   Returns infinity or zero if the number is too large or too small respectively.
--   Honors negative zero.
double :: Decoder Double
double = withQuestionableNumber doubleP

-- | Parses a JSON number or string as a 'Scientific'.
--
--   Does not honor negative zero because 'Scientific' has no notion of such a state.
--
--   Fails if the exponent cannot fit into an 'Int'.
scientific :: Decoder Scientific
scientific = withQuestionableNumber scientificP



-- | Parses a JSON boolean or string as a 'Bool'.
stringBoolean :: Decoder Bool
stringBoolean =
  withValue $ \v ->
    case v of
      F -> do _ <- anyWord8
              False <$ falseP

      T -> do _ <- anyWord8
              True  <$ trueP

      S -> do _ <- anyWord8 -- 0x22 {- " -}
              w <- peekWord8'
              case w of
                0x66 {- f -} -> do
                  _ <- anyWord8
                  falseP
                  q <- peekWord8'
                  if q == 0x22
                    then False <$ anyWord8
                    else err $ "Unexpected byte 0x"
                            <> showHex q " in a boolean"

                0x74 {- t -} -> do
                  _ <- anyWord8
                  trueP
                  q <- peekWord8'
                  if q == 0x22
                    then True <$ anyWord8
                    else err $ "Unexpected byte 0x"
                            <> showHex q " in a boolean"

                _ -> err $ "Unexpected byte 0x"
                        <> showHex w " in a boolean"

      _ -> err $ expectedFound (F :| [T, S]) v



-- | Parses a JSON null or a @"null"@ string.
stringNull :: Decoder ()
stringNull =
  withValue $ \v ->
    case v of
      X -> do _ <- anyWord8
              nullP

      S -> do _ <- anyWord8 -- 0x22 {- " -}
              w <- peekWord8'
              case w of
                0x6e {- n -} -> do
                  _ <- anyWord8
                  nullP
                  q <- peekWord8'
                  if q == 0x22
                    then () <$ anyWord8
                    else err $ "Unexpected byte 0x"
                            <> showHex q " in a null"

                _ -> err $ "Unexpected byte 0x"
                            <> showHex w " in a null"

      _ -> err $ expectedFound (N :| [S]) v
