{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Codec.JSON.Decoder (decode)
import qualified Codec.JSON.Decoder.UUID as Decoder
import           Codec.JSON.Encoder (encode)
import qualified Codec.JSON.Encoder.UUID as Encoder

import           Data.ByteString.Builder
import           Data.UUID.Types as UUID
import           Test.Hspec



main :: IO ()
main =
  hspec $ do
    describe "Decoder" $ do
      it "uuid" $ do
        let (_, ei) = decode Decoder.uuid " \"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\" "
        case UUID.fromString "f81d4fae-7dec-11d0-a765-00a0c91e6bf6" of
          Nothing  -> fail "Invalid reference UUID"
          Just ref ->
            case ei of
              Left err -> fail $ show err
              Right a  -> a `shouldBe` ref

    describe "Encoder" $ do
      it "uuid" $ do
        case UUID.fromString "f81d4fae-7dec-11d0-a765-00a0c91e6bf6" of
          Nothing  -> fail "Invalid reference UUID"
          Just ref ->
            toLazyByteString (encode $ Encoder.uuid ref)
              `shouldBe` "\"f81d4fae-7dec-11d0-a765-00a0c91e6bf6\""
