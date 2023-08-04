{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Decode.Value
  ( value
  ) where

import           Codec.Web.JSON.Decode as JSON

import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



value :: Spec
value =
  describe "Value" $ do
    describe "Object" $ do
      it "Empty" $
        let ref = " \r{    }"
        in decode rawValue ref `shouldBe` Success "" ref

      it "Nonempty (trail)" $
        let ref = " {\"foo\":1.5, \"bar\":\"data\", \"baz\":true, \"other\":{}}"
        in decode rawValue (ref <> "A") `shouldBe` Success "A" ref

    describe "Array" $ do
      it "Empty" $
        let ref = " \r[    ]"
        in decode rawValue ref `shouldBe` Success "" ref

      it "Nonempty (trail)" $
        let ref = " [ 1.5, \"data\", \ttrue, {} ]"
        in decode rawValue (ref <> "A") `shouldBe` Success "A" ref

    describe "String" $ do
      it "Empty" $
        let ref = "  \"\""
        in decode rawValue ref `shouldBe` Success "" ref

      it "UTF-8 (trail)" $
        let ref = "\"\x24\xC2\xA3\xD0\x98\xE0\xA4\xB9\xF0\x90\x8D\x88\""
        in decode rawValue (ref <> "A") `shouldBe` Success "A" ref

      it "UTF-16 (trail)" $
        let ref = "\"\\\"\\\\\\/\\b\\f\\n\\r\\t\\u0000\\uD834\\uDD1E\""
        in decode rawValue (ref <> "A") `shouldBe` Success "A" ref

    describe "Number" $ do
      it "Zero" $
        let ref = " 0"
        in decode rawValue ref `shouldBe` Success "" ref

      it "Negative zero" $
        let ref = " -0.0"
        in decode rawValue ref `shouldBe` Success "" ref

      it "Plain integer (trail)" $
        let ref = " 1234182341234"
        in decode rawValue (ref <> "A") `shouldBe` Success "A" ref

      it "Negative fractional (trail)" $
        let ref = " -987654321.987654321"
        in decode rawValue (ref <> "A") `shouldBe` Success "A" ref

      it "Large fractional" $
        let ref = " 123456789123456789.987654321987654321e1234"
        in decode rawValue ref `shouldBe` Success "" ref

    describe "Boolean" $ do
      it "False" $
        let ref = " \nfalse"
        in decode rawValue ref `shouldBe` Success "" ref

      it "True (trail)" $
        let ref = " \t \rtrue"
        in decode rawValue (ref <> "A") `shouldBe` Success "A" ref

    it "Null" $
      let ref = "null"
      in decode rawValue ref `shouldBe` Success "" ref
