{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Encode.String
  ( Test.JSON.Encode.String.string
  ) where

import           Codec.Web.JSON.Decode as Decode
import           Codec.Web.JSON.Encode as Encode

import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



check :: (Eq a, Show a) => Decoder a -> (a -> Encoder) -> a -> Expectation
check decoder encoder a =
  decode decoder (encode $ encoder a) `shouldBe` Success "" a



string :: Spec
string =
  describe "String" $ do
    describe "String" $ do
      it "Empty" $ do
        check Decode.string Encode.string ""

      it "UTF-8" $ do
        check Decode.string Encode.string "$£Иह𐍈"

      it "UTF-16" $ do
        check Decode.string Encode.stringUtf16 "$£Иह𐍈"

    describe "Strict Text" $ do
      it "Empty" $ do
        check Decode.text Encode.text ""

      it "UTF-8" $ do
        check Decode.text Encode.text "$£Иह𐍈"

      it "UTF-16" $ do
        check Decode.text Encode.textUtf16 "$£Иह𐍈"

    describe "Lazy Text" $ do
      it "Empty" $ do
        check Decode.lazyText Encode.lazyText ""

      it "UTF-8" $ do
        check Decode.lazyText Encode.lazyText "$£Иह𐍈"

      it "UTF-16" $ do
        check Decode.lazyText Encode.lazyTextUtf16 "$£Иह𐍈"
