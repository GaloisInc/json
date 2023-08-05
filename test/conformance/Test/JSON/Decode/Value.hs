{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Decode.Value
  ( value
  ) where

import           Codec.Web.JSON.Decode as JSON

import           Data.String
import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



variants :: IsString a => (a -> Expectation) -> Spec
variants f = do
  describe "Object" $ do
    it "Empty" $
      f " \r{    }"

    it "Nonempty" $
      f " {\"foo\":1.5, \"bar\":\"data\", \"baz\":true, \"other\":{}}"

  describe "Array" $ do
    it "Empty" $
      f " \r[    ]"

    it "Nonempty" $
      f " [ 1.5, \"data\", \ttrue, {} ]"

  describe "String" $ do
    it "Empty" $
      f "  \"\""

    it "UTF-8" $
      f "\"\x24\xC2\xA3\xD0\x98\xE0\xA4\xB9\xF0\x90\x8D\x88\""

    it "UTF-16" $
      f "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0000\\uD834\\uDD1E\""

  describe "Number" $ do
    it "Zero" $
      f " 0"

    it "Negative zero" $
      f " -0.0"

    it "Plain integer" $
      f " 1234182341234"

    it "Negative fractional" $
      f " -987654321.987654321"

    it "Large fractional" $
      f " 123456789123456789.987654321987654321e1234"

  describe "Boolean" $ do
    it "False" $
      f " \nfalse"

    it "True" $
      f " \t \rtrue"

  it "Null" $
    f "null"



value :: Spec
value =
  describe "Value" $ do
    describe "Skip" $ do
      describe "No trail" $ do
        variants $ \a ->
          decode skipValue a `shouldBe` Success "" ()

      describe "Trail" $ do
        variants $ \a ->
          decode skipValue (a <> "A") `shouldBe` Success "A" ()

    describe "Raw" $ do
      describe "No trail" $ do
        variants $ \a ->
          decode rawValue a `shouldBe` Success "" a

      describe "Trail" $ do
        variants $ \a ->
          decode rawValue (a <> "A") `shouldBe` Success "A" a
