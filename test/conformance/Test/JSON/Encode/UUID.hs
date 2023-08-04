{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Encode.UUID
  ( Test.JSON.Encode.UUID.uuid
  ) where

import           Codec.Web.JSON.Decode as Decode
import           Codec.Web.JSON.Encode as Encode

import           Data.UUID.Types as UUID
import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



mkUUID :: String -> IO UUID
mkUUID raw =
  case UUID.fromString raw of
    Just uuuu -> pure uuuu
    Nothing   -> fail "Not a UUID"



uuid :: Spec
uuid =
  describe "UUID" $ do
    it "Ascending" $ do
      uuuu <- mkUUID "00112233-4455-6677-8899-aabbccddeeff"
      decode Decode.uuid (encode $ Encode.uuid uuuu) `shouldBe` Success "" uuuu

    it "Descending" $ do
      uuuu <- mkUUID "ffeeddcc-bbaa-9988-7766-554433221100"
      decode Decode.uuid (encode $ Encode.uuid uuuu) `shouldBe` Success "" uuuu

