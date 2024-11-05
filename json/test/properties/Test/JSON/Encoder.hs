{-# LANGUAGE OverloadedStrings #-}

module Test.JSON.Encoder
  ( core
  ) where

import           Codec.JSON.Encoder as JSON
import           Data.JSON.Unsafe

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.String
import           Test.Hspec



check :: Encoder -> LB.ByteString -> Spec
check x r =
  it (LB.unpack r) $
    toLazyByteString (encode x) `shouldBe` r



stringlike :: IsString a => (a -> Encoder) -> Spec
stringlike encoder = do
  check (encoder "\0\"\\/\b\f\n\r\t\x0F\x1F\x20")
    "\"\\u0000\\\"\\\\/\\b\\f\\n\\r\\t\\u000f\\u001f \""

  check (encoder "$£€\x10000\x24B62\x10FFFF")
    "\"$\xC2\xA3\xE2\x82\xAC\xF0\x90\x80\x80\xF0\xA4\xAD\xA2\xF4\x8F\xBF\xBF\""



core :: Spec
core = do
  describe "null" $ do
    check JSON.null "null"

  describe "maybe" $ do
    check (JSON.maybe Nothing) "null"
    check (JSON.maybe . Just $ JSON.bool True) "true"

  describe "bool" $ do
    check (JSON.bool True) "true"
    check (JSON.bool False) "false"

  describe "number" $ do
    describe "word8"   $ check (JSON.word8   123) "123"
    describe "word16"  $ check (JSON.word16  12345) "12345"
    describe "word32"  $ check (JSON.word32  123456789) "123456789"
    describe "word64"  $ check (JSON.word64  1234567890987654321) "1234567890987654321"
    describe "word"    $ check (JSON.word    12345) "12345"
    describe "natural" $ check (JSON.natural 12345678909876543210123456789)
                                            "12345678909876543210123456789"

    describe "int8"    $ do check (JSON.int8  123) "123"
                            check (JSON.int8  (-123)) "-123"
    describe "int16"   $ do check (JSON.int16 12345) "12345"
                            check (JSON.int16 (-12345)) "-12345"
    describe "int32"   $ do check (JSON.int32 123456789) "123456789"
                            check (JSON.int32 (-123456789)) "-123456789"
    describe "int64"   $ do check (JSON.int64 1234567890987654321) "1234567890987654321"
                            check (JSON.int64 (-1234567890987654321)) "-1234567890987654321"
    describe "int"     $ do check (JSON.int   12345) "12345"
                            check (JSON.int   (-12345)) "-12345"
    describe "integer" $ check (JSON.integer 12345678909876543210123456789)
                                            "12345678909876543210123456789"
    describe "integer" $ check (JSON.integer (-12345678909876543210123456789))
                                             "-12345678909876543210123456789"

    describe "float" $ do check (JSON.float 12345)  "12345.0"
    describe "float" $ do check (JSON.float 1.5e-30)  "1.5e-30"
    describe "float" $ do check (JSON.float (-1.5e30)) "-1.5e30"

    describe "double" $ do check (JSON.double 12345)  "12345.0"
    describe "double" $ do check (JSON.double 1.5e-30)  "1.5e-30"
    describe "double" $ do check (JSON.double (-1.5e30)) "-1.5e30"

  describe "string" $ do
    describe "string" $ stringlike JSON.string
    describe "text" $ stringlike JSON.text
    describe "lazyText" $ stringlike JSON.lazyText

  describe "array" $ do
    describe "emptyArray" $ do
      check emptyArray "[]"

    describe "array1" $ do
      check (array1 $ element (int 1) <> element (int 2) <> element (int 3)) "[1,2,3]"

    describe "list" $ do
      check (list []) "[]"
      check (list [int 1, int 2, int 3]) "[1,2,3]"

  describe "object" $ do
    describe "emptyObject" $ do
      check emptyObject "{}"

    describe "object1" $ do
      check ( object1 $
                   pair (stringKey "a")                    (int 1)
                <> pair (textKey "b")                      (int 2)
                <> pair (lazyTextKey "c")                  (int 3)
                <> pair (jsonKey (JSONKey (JSON "\"e\""))) (int 5)
            )
        "{\"a\":1,\"b\":2,\"c\":3,\"e\":5}"

    describe "pairs" $ do
      check (pairs []) "{}"
      check ( pairs
                [ stringKey "a"                    .= int 1
                , textKey "b"                      ?= Just (int 2)
                , lazyTextKey "c"                  .= int 3
                , textKey "d"                      ?= Nothing
                , jsonKey (JSONKey (JSON "\"e\"")) ?= Just (int 5)
                ]
            )
        "{\"a\":1,\"b\":2,\"c\":3,\"e\":5}"

  describe "raw" $ do
    describe "json" $ do
      check (JSON.json (JSON "\"verbatim\"")) "\"verbatim\""
