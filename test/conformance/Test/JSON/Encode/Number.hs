{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Encode.Number
  ( number
  ) where

import           Codec.Web.JSON.Decode as Decode
import           Codec.Web.JSON.Encode as Encode

import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



check :: (Eq a, Show a) => Decoder a -> (a -> Encoder) -> a -> Expectation
check decoder encoder a =
  decode decoder (encode $ encoder a) `shouldBe` Success "" a



number :: Spec
number =
  describe "Number" $ do
    it "Word8" $
      check Decode.word8 Encode.word8 127

    it "Word16" $
      check Decode.word16 Encode.word16 32678

    it "Word32" $
      check Decode.word32 Encode.word32 2147483648

    it "Word64" $
      check Decode.word64 Encode.word64 9223372036854775808

    it "Word" $
      check Decode.word Encode.word 2147483648

    it "Natural" $
      check Decode.natural Encode.natural 170141183460469231731687303715884105728

    describe "Int8" $ do
      it "Positive" $ do
        check Decode.int8 Encode.int8 63

      it "Negative" $ do
        check Decode.int8 Encode.int8 (-63)

    describe "Int16" $ do
      it "Positive" $ do
        check Decode.int16 Encode.int16 16384

      it "Negative" $ do
        check Decode.int16 Encode.int16 (-16384)

    describe "Int32" $ do
      it "Positive" $ do
        check Decode.int32 Encode.int32 1073741824

      it "Negative" $ do
        check Decode.int32 Encode.int32 (-1073741824)

    describe "Int64" $ do
      it "Positive" $ do
        check Decode.int64 Encode.int64 4611686018427387904

      it "Negative" $ do
        check Decode.int64 Encode.int64 (-4611686018427387904)

    describe "Int" $ do
      it "Positive" $ do
        check Decode.int Encode.int 1073741824

      it "Negative" $ do
        check Decode.int Encode.int (-1073741824)

    describe "Integer" $ do
      it "Positive" $ do
        check Decode.integer Encode.integer 85070591730234615865843651857942052864

      it "Negative" $ do
        check Decode.integer Encode.integer (-85070591730234615865843651857942052864)

    describe "Float" $ do
      it "Negative zero" $ do
        decode Decode.float (encode $ Encode.float (-0))
          `shouldSatisfy` \f -> case f of
                                  Success "" z -> isNegativeZero z
                                  _            -> False

      it "Positive" $ do
        check Decode.float Encode.float 123.4567

      it "Negative" $ do
        check Decode.float Encode.float (-1234.567)

    describe "Double" $ do
      it "Negative zero" $ do
        decode Decode.double (encode $ Encode.double (-0))
          `shouldSatisfy` \f -> case f of
                                  Success "" z -> isNegativeZero z
                                  _            -> False

      it "Positive" $ do
        check Decode.double Encode.double 1234567.1234567

      it "Negative" $ do
        check Decode.double Encode.double (-7654321.7654321)

    describe "Scientific" $ do
      it "Positive" $ do
        check Decode.scientific Encode.scientific 123456789123456789.123456789123456789

      it "Negative" $ do
        check Decode.scientific Encode.scientific (-987654321987654321.987654321987654321)
