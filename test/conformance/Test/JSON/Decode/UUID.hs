{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Decode.UUID
  ( Test.JSON.Decode.UUID.uuid
  ) where

import           Codec.Web.JSON.Decode as JSON

import           Data.ByteString.Lazy (ByteString)
import           Data.UUID.Types as UUID
import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



compareRaw :: ByteString -> ByteString -> String -> Expectation
compareRaw input left raw =
  case UUID.fromString raw of
    Just uuuu -> decode JSON.uuid input `shouldBe` Success left uuuu
    Nothing   -> fail "Not a UUID"



uuid :: Spec
uuid =
  describe "UUID" $ do
    it "Ascending" $
      compareRaw "\"00112233-4455-6677-8899-aabbccddeeff\""
                 "" "00112233-4455-6677-8899-aabbccddeeff"

    it "Ascending (trail)" $
      compareRaw "\"00112233-4455-6677-8899-aabbccddeeff\"A"
                 "A" "00112233-4455-6677-8899-aabbccddeeff"

    it "Descending" $
      compareRaw "\"ffeeddcc-bbaa-9988-7766-554433221100\""
                 "" "ffeeddcc-bbaa-9988-7766-554433221100"

    it "Descending (trail)" $
      compareRaw "\"ffeeddcc-bbaa-9988-7766-554433221100\"A"
                 "A" "ffeeddcc-bbaa-9988-7766-554433221100"

    it "Symmetrical" $
      compareRaw "\"01234567-89ab-cdef-fedc-ba9876543210\""
                 "" "01234567-89ab-cdef-fedc-ba9876543210"

    it "Descending (trail)" $
      compareRaw "\"01234567-89ab-cdef-fedc-ba9876543210\"A"
                 "A" "01234567-89ab-cdef-fedc-ba9876543210"


