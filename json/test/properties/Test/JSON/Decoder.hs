{-# LANGUAGE BangPatterns
           , OverloadedLists
           , OverloadedStrings
           , TemplateHaskell #-}

module Test.JSON.Decoder
  ( core
  ) where

import           Codec.JSON.Decoder as JSON
import           Codec.JSON.Decoder.TH as JSON
import           Codec.JSON.Decoder.Unsafe as JSON
import qualified Codec.JSON.Encoder as Encoder
import           Data.JSON as JSON

import           Data.ByteString.Builder
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LB
import           Data.Functor.Apply
import           Data.Primitive.ByteArray
import qualified Data.RadixTree.Word8.Strict as Radix
import           Data.String
import qualified Data.Text.Array as T
import qualified Data.Text.Internal as T (Text (..))
import qualified Data.Text.Internal.Lazy as LT (Text (..))
import           GHC.Base (unsafeChr)
import           GHC.Real (infinity)
import           System.Random
import           Test.Hspec



check :: (Eq a, Show a) => Decoder a -> LB.ByteString -> a -> Spec
check decoder input a =
  it (LB.unpack input) $
    check_ decoder input a

check_ :: (Eq a, Show a) => Decoder a -> LB.ByteString -> a -> Expectation
check_ decoder input a = do
  let (Scrap _ bs more, ei) = decode decoder input
  case ei of
    Left err -> fail $ show err
    Right b  ->
      if b /= a
        then b `shouldBe` a
        else if not $ LB.null bs
               then fail "Non-empty remainder"
               else case more of
                      More -> fail "Unconsumed chunks remain"
                      End  -> pure ()

fails :: (Eq a, Show a) => Decoder a -> LB.ByteString -> Spec
fails decoder input =
  it (LB.unpack input) $ do
    let (Scrap _ _ _, ei) = decode decoder input
    case ei of
      Left _  -> pure () :: Expectation
      Right _ -> fail "Did not fail"



unsignedIntegral :: (Eq a, Num a, Show a) => Decoder a -> Spec
unsignedIntegral decoder = do
  fails decoder "00"
  fails decoder "-4"
  fails decoder "0.1"
  fails decoder "10e-2"

  check decoder "0.002e4" 20
  check decoder "15400E-2" 154
  check decoder "2E+2" 200
  check decoder "0.000000000000000e100000000000000000000000" 0
  check decoder "-0.000000000000000e100000000000000000000000" (-0)
  check decoder "0.000009e+0000000000000000000000000000007" 90
  check decoder "0.0000000000000000000000123e25" 123

signedIntegral :: (Eq a, Num a, Show a) => Decoder a -> Spec
signedIntegral decoder = do
  fails decoder "00"
  fails decoder "0.1"
  fails decoder "10e-2"

  check decoder "0.002e4" 20
  check decoder "-12400E-2" (-124)
  check decoder "1E+2" 100
  check decoder "0.000000000000000e100000000000000000000000" 0
  check decoder "-0.000000000000000e100000000000000000000000" (-0)
  check decoder "-0.000009e+0000000000000000000000000000007" (-90)
  check decoder "0.0000000000000000000000123e25" 123

floating :: (Eq a, Floating a, Show a) => Decoder a -> Spec
floating decoder = do
  fails decoder "00"

  check decoder "100.002e1" 1000.02
  check decoder "154E-2" 1.54
  check decoder "123e+2" 12300
  check decoder "567E-4" 5.67e-2
  check decoder "123E2" 12300
  check decoder "0.000000000000000e100000000000000000000000" 0
  check decoder "-0.000000000000000e100000000000000000000000" (-0)
  check decoder "6e-100000000000000000000000000000" 0
  check decoder "-7e-100000000000000000000000000000" (-0)
  check decoder "8e1000000000000000000000000000000" (fromRational infinity)
  check decoder "-9e1000000000000000000000000000000" (fromRational (negate infinity))
  check decoder "123.456e+0000000000000000000000000000007" 1.23456e9
  check decoder "-0.00000000000000012345e20" (-12345)

hugenumber :: Spec
hugenumber = do
  fails bignum "00"

  check bignum "12345678909876543210000.0000987654321234567890E+0000123456789876543210000987654321234567890" (123456789098765432100000000987654321234567890, 123456789876543210000987654321234567868)
  check bignum "-0.000000000000000e100000000000000000000000" (0, 0)
  check bignum "6e-100000000000000000000000000000" (6, -100000000000000000000000000000)
  check bignum "-9e10000000000000000000000000000000" (-9, 10000000000000000000000000000000)
  check bignum "123.456e+0000000000000000000000000000007" (123456,4)
  check bignum "-0.00000000000000012345e20" (-12345, 0)

rawnumberlike :: Decoder LB.ByteString -> Spec
rawnumberlike decoder = do
  fails decoder " 00 "

  check decoder " 123456789.123456789e+123456789 "
                 "123456789.123456789e+123456789"

  check decoder " -0.0000000001E-00000000000001 "
                 "-0.0000000001E-00000000000001"



stringlike :: (Eq a, IsString a, Show a) => Decoder a -> Spec
stringlike decoder = do
  fails decoder "\"\15\""
  fails decoder "\"\\uDC00\""
  fails decoder "\"\\uD800\\uD800\""

  check decoder " \"\"" ""
  check decoder "\" abcdefghijklmnopqrstuvwxyz/\DEL\"" " abcdefghijklmnopqrstuvwxyz/\DEL"
  check decoder "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0020\"" "\"\\/\b\f\n\r\t "
  check decoder "\"\\uD800\\uDC00\\uD852\\uDF62\\uDBFF\\uDFFF\"" "\x10000\x24B62\x10FFFF"
  check decoder "\"$\xC2\xA3\xE2\x82\xAC\xF0\x90\x80\x80\xF0\xA4\xAD\xA2\xF4\x8F\xBF\xBF\""
                  "$£€\x10000\x24B62\x10FFFF"

  it "<massive>" $ do
    let massive = massivestring 1048576 (mkStdGen 0)
    check_ decoder (toLazyByteString . Encoder.encode $ Encoder.string massive)
                   (fromString massive)



rawstringlike :: Decoder LB.ByteString -> Spec
rawstringlike decoder = do
  fails decoder "\"\15\""
  fails decoder "\"\\uDC00\""
  fails decoder "\"\\uD800\\uD800\""

  check decoder " \"\"" "\"\""
  check decoder "\" abcdefghijklmnopqrstuvwxyz/\DEL\""
                "\" abcdefghijklmnopqrstuvwxyz/\DEL\""
  check decoder "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0020\""
                "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0020\""
  check decoder "\"\\uD800\\uDC00\\uD852\\uDF62\\uDBFF\\uDFFF\""
                "\"\\uD800\\uDC00\\uD852\\uDF62\\uDBFF\\uDFFF\""
  check decoder "\"$\xC2\xA3\xE2\x82\xAC\xF0\x90\x80\x80\xF0\xA4\xAD\xA2\xF4\x8F\xBF\xBF\""
                "\"$\xC2\xA3\xE2\x82\xAC\xF0\x90\x80\x80\xF0\xA4\xAD\xA2\xF4\x8F\xBF\xBF\""

  it "<massive>" $ do
    let massive = toLazyByteString . Encoder.encode $
                                       Encoder.string $
                                         massivestring 1048576 (mkStdGen 0)
    check_ decoder massive massive



massivestring :: Int -> StdGen -> String
massivestring = go
  where
    go n g
      | n <= 0    = []
      | otherwise =
          let (i, g') = uniformR (0, 0x10F7FF) g
              !c = unsafeChr $ if i >= 0xD800
                                 then i + 0x0800
                                 else i

          in c : go (n - 1) g'

surrogates :: LB.ByteString
surrogates = "\"abcd\xED\xA0\x80\xED\xBF\xBF|ef\xED\xA1\x92ghij\""



arraylike :: Decoder (Int, Bool, Float, ()) -> Spec
arraylike decoder = do
  fails decoder "[1, true, 3]"
  fails decoder "[1, true, 3, null, 5]"

  check decoder "[1,true,3,null]" (1, True, 3, ())
  check decoder " [ 1 , true , 3 , null ] " (1, True, 3, ())

rawarraylike :: Decoder LB.ByteString -> Spec
rawarraylike decoder = do
  check decoder "[]" "[]"
  check decoder " [ ] " "[ ]"

  check (JSON.getRaw <$> JSON.jsonArray) "[\"item\",2]" "[\"item\",2]"
  check (JSON.getRaw <$> JSON.jsonArray) " [ \"item\", 2 ] " "[ \"item\", 2 ]"




insertByteString :: B.ByteString -> a -> Radix.RadixTree a -> Radix.RadixTree a
insertByteString k a = Radix.insert (Radix.feedByteString k) a

radixlike :: Decoder (Radix.RadixTree LB.ByteString) -> Spec
radixlike decoder = do
  check decoder "{}" Radix.empty
  check decoder " { } " Radix.empty

  check decoder
    "{\"first\":1,\"second\":null,\"third\":3.0e-3,\"that\":true}"
    $   insertByteString "first" "1"
      . insertByteString "second" "null"
      . insertByteString "third" "3.0e-3"
      . insertByteString "that" "true"
      $ Radix.empty

  check decoder
    " { \"first\" : 1 , \"second\" : null , \"third\" : 3.0e-3 , \"that\":true } "
    $   insertByteString "first" "1"
      . insertByteString "second" "null"
      . insertByteString "third" "3.0e-3"
      . insertByteString "that" "true"
      $ Radix.empty



objectlike :: Decoder (Int, (), Maybe Float, Maybe Bool) -> Spec
objectlike decoder = do
  fails decoder "{}"
  fails decoder "{\"first\":1,\"third\":3.0e-3,\"that\":true}"

  check decoder
    "{\"first\":1,\"second\":null,\"third\":3.0e-3,\"that\":true}"
    (1, (), Just 3e-3, Just True)

  check decoder
    "{\"first\":1,\"second\":null, \"that\":null}"
    (1, (), Nothing, Nothing)

  check decoder
    " { \"first\" : 1 , \"second\" : null , \"third\" : 3.0e-3 , \"that\" : true } "
    (1, (), Just 3e-3, Just True)

rawobjectlike :: Decoder LB.ByteString -> Spec
rawobjectlike decoder = do
  check decoder "{}" "{}"
  check decoder " { } " "{ }"

  check decoder
    "{\"first\":1,\"second\":null,\"third\":3.0e-3,\"that\":true}"
    "{\"first\":1,\"second\":null,\"third\":3.0e-3,\"that\":true}"

  check decoder
    " { \"first\" : 1 , \"second\" : null , \"third\" : 3.0e-3 , \"that\" : true } "
     "{ \"first\" : 1 , \"second\" : null , \"third\" : 3.0e-3 , \"that\" : true }"




core :: Spec
core = do
  describe "null" $ do
    check JSON.null "null" ()

    describe "maybe" $ do
      check (JSON.maybe JSON.bool) "true" (Just True)
      check (JSON.maybe JSON.bool) "null" Nothing


  describe "bool" $ do
    check JSON.bool "true" True
    check JSON.bool "false" False


  describe "number" $ do
    describe "word8"  $ unsignedIntegral JSON.word8
    describe "word16" $ unsignedIntegral JSON.word16
    describe "word32" $ unsignedIntegral JSON.word32
    describe "word64" $ unsignedIntegral JSON.word64
    describe "word"   $ unsignedIntegral JSON.word

    describe "int8"  $ signedIntegral JSON.int8
    describe "int16" $ signedIntegral JSON.int16
    describe "int32" $ signedIntegral JSON.int32
    describe "int64" $ signedIntegral JSON.int64
    describe "int"   $ signedIntegral JSON.int

    describe "float"  $ floating JSON.float
    describe "double" $ floating JSON.double

    describe "bignum" hugenumber


  describe "string" $ do
    describe "string"       $ do
      stringlike JSON.string
      check JSON.string surrogates "abcd\xFFFD\xFFFD|ef\xFFFDghij"

    describe "string'"      $ do
      stringlike JSON.string'
      fails JSON.string' surrogates

    describe "unsafeString" $ do
      stringlike JSON.unsafeString
      check JSON.unsafeString surrogates "abcd\xD800\xDFFF|ef\xd852ghij"

    describe "text"       $ do
      stringlike JSON.text
      check JSON.text surrogates "abcd\xFFFD\xFFFD|ef\xFFFDghij"

    describe "text'"      $ do
      stringlike JSON.text'
      fails JSON.text' surrogates

    describe "unsafeText" $ do
      stringlike JSON.unsafeText
      check JSON.unsafeText surrogates $
        let !(ByteArray arr) =
              [ 0x61, 0x62, 0x63, 0x64, 0xED, 0xA0, 0x80, 0xED, 0xBF, 0xBF
              , 0x7C, 0x65, 0x66, 0xED, 0xA1, 0x92, 0x67, 0x68, 0x69, 0x6A ]

        in T.Text (T.ByteArray arr) 0 20

    describe "lazyText"       $ do
      stringlike JSON.lazyText
      check JSON.lazyText surrogates "abcd\xFFFD\xFFFD|ef\xFFFDghij"

    describe "lazyText'"      $ do
      stringlike JSON.lazyText'
      fails JSON.lazyText' surrogates

    describe "unsafeLazyText" $ do
      stringlike JSON.unsafeLazyText
      check JSON.unsafeLazyText surrogates $
        let !(ByteArray arr) =
              [ 0x61, 0x62, 0x63, 0x64, 0xED, 0xA0, 0x80, 0xED, 0xBF, 0xBF
              , 0x7C, 0x65, 0x66, 0xED, 0xA1, 0x92, 0x67, 0x68, 0x69, 0x6A ]

        in LT.Chunk (T.Text (T.ByteArray arr) 0 20) LT.Empty


  describe "array" $ do
    describe "list" $ do
      check (JSON.list JSON.int) "[]" []
      check (JSON.list JSON.int) " [ ] " []
      check (JSON.list JSON.int) "[1,2,3,4,5,6]" [1, 2, 3, 4, 5, 6]
      check (JSON.list JSON.int) " [ 1 , 2 , 3 , 4 , 5 , 6 ] " [1, 2, 3, 4, 5, 6]

    describe "snocList" $ do
      check (JSON.snocList JSON.int) "[]" (0, [])
      check (JSON.snocList JSON.int) " [ ] " (0, [])
      check (JSON.snocList JSON.int) "[1,2,3,4,5,6]" (6, [6, 5, 4, 3, 2, 1])
      check (JSON.snocList JSON.int) " [ 1 , 2 , 3 , 4 , 5 , 6 ] " (6, [6, 5, 4, 3, 2, 1])

    describe "elementsA" $
      arraylike $
        elementsA $
          (,,,)
            <$> element JSON.int
            <.> element JSON.bool
            <.> element JSON.float
            <.> element JSON.null

    describe "elementsA/TH" $
      arraylike $
        $$( elementsQ $
             [|| (,,,) ||]
               |$| elementQ [|| JSON.int   ||]
               |*| elementQ [|| JSON.bool  ||]
               |*| elementQ [|| JSON.float ||]
               |*| elementQ [|| JSON.null  ||]
          )


  describe "object" $ do
    describe "radixTree" $ do
      radixlike (fmap JSON.getRaw <$> JSON.radixTree)

      check (fmap JSON.getRaw <$> JSON.radixTree)
        "{\"single\":\"\xED\xA0\x80\"}"
        $   insertByteString "single" "\"\xED\xA0\x80\""
          $ Radix.empty

    describe "radixTree'" $ do
      radixlike (fmap JSON.getRaw <$> JSON.radixTree')

      fails (fmap JSON.getRaw <$> JSON.radixTree')
        "{\"single\":\"\xED\xA0\x80\"}"

    describe "pairsM" $
      objectlike $
        pairsM $
          (,,,)
            <$> "first"  .: JSON.int
            <*> "second" .: JSON.null
            <*> "third"  ?: JSON.float
            <*> "that"   ?: JSON.bool

    describe "pairsA" $
      objectlike $
        pairsA $
          (,,,)
            <$> "first"  .: JSON.int
            <*> "second" .: JSON.null
            <*> "third"  ?: JSON.float
            <*> "that"   ?: JSON.bool

    describe "pairsA/TH" $
      objectlike $
        $$( pairsQ $
              [|| (,,,) ||]
                |$| "first"  .| [|| JSON.int ||]
                |*| "second" .| [|| JSON.null ||]
                |*| "third"  ?| [|| JSON.float ||]
                |*| "that"   ?| [|| JSON.bool ||]
          )


  describe "raw" $ do
    describe "null" $
      check (JSON.getRaw <$> JSON.jsonNull) " null " "null"

    describe "bool" $ do
      check (JSON.getRaw <$> JSON.jsonBoolean) " true " "true"
      check (JSON.getRaw <$> JSON.jsonBoolean) " false " "false"

    describe "number" $ do
      rawnumberlike (JSON.getRaw <$> JSON.jsonNumber)

    describe "string" $ do
      describe "jsonString" $ do
        rawstringlike (JSON.getRaw <$> JSON.jsonString)
        check (JSON.getRaw <$> JSON.jsonString) surrogates surrogates

      describe "jsonString'" $ do
        rawstringlike (JSON.getRaw <$> JSON.jsonString')
        fails (JSON.getRaw <$> JSON.jsonString') surrogates

    describe "array" $ do
      let arraySurrogates = "[" <> surrogates <> "]"
      describe "jsonArray" $ do
        rawarraylike (JSON.getRaw <$> JSON.jsonArray)
        check (JSON.getRaw <$> JSON.jsonArray) arraySurrogates arraySurrogates

      describe "jsonArray'" $ do
        rawarraylike (JSON.getRaw <$> JSON.jsonArray')
        fails (JSON.getRaw <$> JSON.jsonArray') arraySurrogates

    describe "object" $ do
      let objectSurrogates = "{ \"single\" : " <> surrogates <> " }"
      describe "jsonObject" $ do
        rawobjectlike (JSON.getRaw <$> JSON.jsonObject)
        check (JSON.getRaw <$> JSON.jsonObject) objectSurrogates objectSurrogates

      describe "jsonObject'" $ do
        rawobjectlike (JSON.getRaw <$> JSON.jsonObject')
        fails (JSON.getRaw <$> JSON.jsonObject') objectSurrogates

    describe "any" $ do
      let blob decoder = do
            check decoder " null " "null"
            check decoder " true " "true"
            rawnumberlike decoder
            rawstringlike decoder
            rawarraylike decoder
            rawobjectlike decoder

      describe "json" $ do
        blob (JSON.getRaw <$> JSON.json)
        check (JSON.getRaw <$> JSON.json) surrogates surrogates

      describe "json'" $ do
        blob (JSON.getRaw <$> JSON.json')
        fails (JSON.getRaw <$> JSON.json') surrogates


  describe "copy" $ do
    check ( (\(r, x) -> (JSON.getRaw r, x)) <$> do JSON.copy $ JSON.list JSON.int )
      " [ 1, 2, 3, 4, 5, 6 ] "
      ("[ 1, 2, 3, 4, 5, 6 ]", [1, 2, 3, 4, 5, 6])
