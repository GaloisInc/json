{-# OPTIONS_HADDOCK not-home #-}

{- | More granular access to string decoding handling.

     Values, objects and arrays may contain strings inside them, so decoders that
     skip or return said strings verbatim also have this option.
 -}

module Codec.Web.JSON.Decode.Encoding
  ( -- * Error type
    -- ** UTF-8
    UTF8Error (..)
  , showsUTF8Error

    -- ** UTF-16
  , UTF16Error (..)
  , showsUTF16Error

    -- ** Mixed
  , DecodingError (..)
  , showsDecodingError

    -- * Handling
  , DecodingHandler (..)
  , DecodingHandling (..)

    -- ** Convenience handlers
  , strict
  , lenient
  , replace
  , ignore

    -- * Value
  , skipValue'
  , rawValue'

    -- ** Object
  , skipObject'
  , rawObject'

    -- *** Bounded
  , boundedObject'

    -- *** Linear
  , linearObject'

    -- *** Plain
  , object'

    -- ** Array
  , skipArray'
  , rawArray'

    -- ** String
  , string'
  , byteString'
  , lazyByteString'
  , text'
  , lazyText'

  , skipString'
  , rawString'
  ) where

import           Codec.Web.JSON.Decode.Core
import           Codec.Web.JSON.Decode.Object.Bounded
import           Codec.Web.JSON.Decode.Object.Linear
import           Codec.Web.JSON.Decode.Object.Plain
import           Codec.Web.JSON.Parse.Knot
import           Codec.Web.JSON.Parse.String
import           Data.ByteString.Lazy.Copy
import           Encoding.Mixed.Error

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)



-- | Fail with 'showsDecodingError' on invalid byte sequence.
strict :: DecodingHandler
strict = DecodingHandler $ Fail . flip showsDecodingError []

-- | Replace any encountered invalid byte sequence with @�@ (@U+FFFD@).
--
--   Note that radix trees inside the library treat @�@ like any other character.
lenient :: DecodingHandler
lenient = DecodingHandler $ \_ -> Replace '\xFFFD'

-- | Replace any encountered invalid byte sequence with the given character.
replace :: Char -> DecodingHandler
replace char = DecodingHandler $ \_ -> Replace char

-- | Ignore any encountered invalid byte sequence.
ignore :: DecodingHandler
ignore = DecodingHandler $ \_ -> Ignore



-- | Parses a JSON value and discards it.
--
--   Strings are decoded using a custom 'DecodingHandler'.
skipValue' :: DecodingHandler -> Decoder ()
skipValue' = Decoder . skipValueP

-- | Parses a JSON value and returns it verbatim.
--
--   Strings are decoded using a custom 'DecodingHandler'.
--
--   Output 'BSL.LazyByteString' is chunked every @Int@ decoded characters.
rawValue' :: DecodingHandler -> Int -> Decoder BSL.ByteString
rawValue' handler len =
  Decoder $ do
    copy <- copyValueP handler len emptyCopy
    pure $! commitCopy copy



-- | Parses a JSON object and discards it.
--
--   Strings are decoded using a custom 'DecodingHandler'.
skipObject' :: DecodingHandler -> Decoder ()
skipObject' = withObject . skipObjectP

-- | Parses a JSON object and returns it verbatim.
--
--   Strings are decoded using a custom 'DecodingHandler'.
--
--   Output 'BSL.LazyByteString' is chunked every @Int@ decoded characters.
rawObject' :: DecodingHandler -> Int -> Decoder BSL.ByteString
rawObject' handler len =
  Decoder .
    copyingObjectP len emptyCopy $ \copy0 -> do
      copy1 <- copyObjectP handler len copy0
      pure $! commitCopy copy1



-- | Parses a JSON array and discards it
--
--   Strings are decoded using a custom 'DecodingHandler'.
skipArray' :: DecodingHandler -> Decoder ()
skipArray' = withArray . skipArrayP

-- | Parses a JSON array and returns it verbatim.
--
--   Strings are decoded using a custom 'DecodingHandler'.
--
--   Output 'BSL.LazyByteString' is chunked every @Int@ decoded characters.
rawArray' :: DecodingHandler -> Int -> Decoder BSL.ByteString
rawArray' handler len =
  Decoder .
    copyingArrayP len emptyCopy $ \copy0 -> do
      copy <- copyArrayP handler len copy0
      pure $! commitCopy copy



-- | Parses a JSON string into a 'String' using a custom 'DecodingHandler'.
string' :: DecodingHandler -> Decoder String
string' = withString . stringUtf8P

-- | Parses a JSON string into a UTF-8 encoded 'Data.ByteString.StrictByteString'
--   using a custom 'DecodingHandler'.
byteString' :: DecodingHandler -> Decoder ByteString
byteString' = withString . byteStringUtf8P

-- | Parses a JSON string into a UTF-8 encoded 'BSL.LazyByteString'
--   using a custom 'DecodingHandler'.
lazyByteString' :: DecodingHandler -> Int -> Decoder BSL.ByteString
lazyByteString' handler = withString . lazyByteStringUtf8P handler

-- | Parses a JSON string into a strict 'Text' using a custom 'DecodingHandler'.
text' :: DecodingHandler -> Decoder Text
text' = withString . textUtf8P

-- | Parses a JSON string into a lazy 'Text' using a custom 'DecodingHandler'.
lazyText' :: DecodingHandler -> Int -> Decoder Lazy.Text
lazyText' handler = withString . lazyTextUtf8P handler



-- | Parses a JSON string using a custom 'DecodingHandler' and discards it.
skipString' :: DecodingHandler -> Decoder ()
skipString' = withString . skipUtf8P

-- | Parses a JSON string using a custom 'DecodingHandler' and returns it verbatim.
--
--   Double quotes around the string are preserved.
--
--   Output 'BSL.LazyByteString' is chunked every @Int@ decoded characters.
rawString' :: DecodingHandler -> Int -> Decoder BSL.ByteString
rawString' handler len =
  Decoder .
    copyingStringP len emptyCopy $ \copy0 -> do
      copy1 <- copyByteStringUtf8P handler len copy0
      pure $! commitCopy copy1
