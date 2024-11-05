{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import           Codec.JSON.Decoder (Decoder, decode)
import qualified Codec.JSON.Decoder.Time as Decoder
import           Codec.JSON.Encoder (Encoder, encode)
import qualified Codec.JSON.Encoder.Time as Encoder

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LB
import           Data.Time.Calendar
import           Data.Time.Calendar.Month
import           Data.Time.Calendar.Quarter
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Test.Hspec



deriving instance Eq ZonedTime



check :: (Eq a, Show a) => Decoder a -> a -> LB.ByteString -> Expectation
check decoder ref input = do
  let (_, ei) = decode decoder input
  case ei of
    Left err -> fail $ show err
    Right a  -> a `shouldBe` ref



built :: (a -> Encoder) -> a -> LB.ByteString -> Expectation
built encoder a input =
  toLazyByteString (encode $ encoder a) `shouldBe` input



main :: IO ()
main =
  hspec $ do
    describe "Decoder" $ do
      it "year"        $ check Decoder.year 1234 " \"1234\" "
      it "quarter"     $ check Decoder.quarter (YearQuarter 123 Q4) " \"0123-Q4\" "
      it "month"       $ check Decoder.month (YearMonth 6543 12) " \"6543-12\" "
      it "monthDate"   $ check Decoder.monthDate (YearMonthDay 1234 12 23) " \"1234-12-23\" "
      it "weekDate"    $ check Decoder.weekDate (YearMonthDay 1234 12 23) " \"1234-W51-6\" "
      it "ordinalDate" $ check Decoder.ordinalDate (YearMonthDay 1234 12 23) " \"1234-357\" "
      it "timeOfDay"   $ check Decoder.timeOfDay (TimeOfDay 12 34 56.789) " \"12:34:56.789\" "
      it "localTime"   $ check Decoder.localTime
                           (LocalTime (YearMonthDay 1234 12 23) (TimeOfDay 12 34 56.789))
                           " \"1234-12-23T12:34:56.789\" "

      it "timeZone"    $ check Decoder.timeZone (minutesToTimeZone (-754)) " \"-12:34\""
      it "zonedTime"   $ check Decoder.zonedTime
                           ( ZonedTime
                               (LocalTime (YearMonthDay 1234 12 23) (TimeOfDay 12 34 56.789))
                               (minutesToTimeZone 754)
                           )
                           " \"1234-12-23T12:34:56.789+12:34\" "


    describe "Encoder" $ do
      it "year"        $ built Encoder.year 1234 "\"1234\""
      it "quarter"     $ built Encoder.quarter (YearQuarter 123 Q4) "\"0123-Q4\""
      it "month"       $ built Encoder.month (YearMonth 6543 12) "\"6543-12\""
      it "monthDate"   $ built Encoder.monthDate (YearMonthDay 1234 12 23) "\"1234-12-23\""
      it "weekDate"    $ built Encoder.weekDate (YearMonthDay 1234 12 23) "\"1234-W51-6\""
      it "ordinalDate" $ built Encoder.ordinalDate (YearMonthDay 1234 12 23) "\"1234-357\""
      it "timeOfDay"   $ built Encoder.timeOfDay (TimeOfDay 12 34 56.789) "\"12:34:56.789\""

      it "localTime"   $ built Encoder.localTime
                           (LocalTime (YearMonthDay 1234 12 23) (TimeOfDay 12 34 56.789))
                           "\"1234-12-23T12:34:56.789\""

      it "utcTime"     $ built Encoder.utcTime
                           ( UTCTime
                               (YearMonthDay 1234 12 23)
                               (timeOfDayToTime $ TimeOfDay 12 34 56.789))
                           "\"1234-12-23T12:34:56.789Z\""

      it "timeZone"    $ built Encoder.timeZone (minutesToTimeZone (-754)) "\"-12:34\""
      it "zonedTime"   $ built Encoder.zonedTime
                           ( ZonedTime
                               (LocalTime (YearMonthDay 1234 12 23) (TimeOfDay 12 34 56.789))
                               (minutesToTimeZone 754)
                           )
                           "\"1234-12-23T12:34:56.789+12:34\""
