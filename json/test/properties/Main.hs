module Main
  ( main
  ) where

import           Test.JSON.Decoder        as Decoder
import           Test.JSON.Decoder.Stream as Decoder
import           Test.JSON.Encoder        as Encoder

import           Test.Hspec



main :: IO ()
main =
  hspec $ do
    describe "Decoder" $ do
      describe "_"      Decoder.core
      describe "Stream" Decoder.stream

    describe "Encoder" $ do
      describe "_"      Encoder.core
