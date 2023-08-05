module Main where

import           Test.JSON.Decode.Array as Decode
import           Test.JSON.Decode.Boolean as Decode
import           Test.JSON.Decode.Decoders as Decode
import           Test.JSON.Decode.Null as Decode
import           Test.JSON.Decode.Number as Decode
import           Test.JSON.Decode.Object as Decode
import           Test.JSON.Decode.Stream as Decode
import           Test.JSON.Decode.String as Decode
import           Test.JSON.Decode.Time as Decode
import           Test.JSON.Decode.UUID as Decode
import           Test.JSON.Decode.Value as Decode
import           Test.JSON.Encode.Array as Encode
import           Test.JSON.Encode.Boolean as Encode
import           Test.JSON.Encode.Null as Encode
import           Test.JSON.Encode.Number as Encode
import           Test.JSON.Encode.Object as Encode
import           Test.JSON.Encode.String as Encode
import           Test.JSON.Encode.Time as Encode
import           Test.JSON.Encode.UUID as Encode

import           Test.Hspec



main :: IO ()
main =
  hspec $ do
    Decode.array
    Decode.boolean
    Decode.decoders
    Decode.null
    Decode.number
    Decode.object
    Decode.stream
    Decode.string
    Decode.time
    Decode.uuid
    Decode.value

    Encode.array
    Encode.boolean
    Encode.null
    Encode.number
    Encode.object
    Encode.string
    Encode.time
    Encode.uuid
