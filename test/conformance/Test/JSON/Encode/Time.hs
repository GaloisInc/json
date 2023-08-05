{-# LANGUAGE OverloadedStrings
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Encode.Time
  ( time
  ) where

import           Codec.Web.JSON.Decode as Decode
import           Codec.Web.JSON.Encode as Encode

import           Data.Time.Calendar
import           Data.Time.Calendar.Month.Compat
import           Data.Time.Calendar.Quarter.Compat
import           Data.Time.Clock
import           Data.Time.LocalTime.Compat
import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



instance Eq ZonedTime where
  ZonedTime a c == ZonedTime b d = a == b && c == d



check :: (Eq a, Show a) => Decoder a -> (a -> Encoder) -> a -> Expectation
check decoder encoder a =
  decode decoder (encode $ encoder a) `shouldBe` Success "" a



time :: Spec
time =
  describe "Time" $ do
    describe "Year" $ do
      it "2134" $
        check Decode.year Encode.year 2134

      it "0567" $
        check Decode.year Encode.year 567

      it "0089" $
        check Decode.year Encode.year 89

      it "0001" $
        check Decode.year Encode.year 1

      it "0000" $
        check Decode.year Encode.year 0

    it "Quarter" $
      check Decode.quarter Encode.quarter $ YearQuarter 2134 Q3

    it "Month" $
      check Decode.month Encode.month $ fromYearMonth 2134 05

    it "Week" $
      check Decode.week (uncurry Encode.week) (2134, 15)

    it "Calendar date" $
      check Decode.calendarDate Encode.calendarDate $ fromGregorian 2134 5 28

    it "Week date" $
      check Decode.weekDate Encode.weekDate $ fromGregorian 2134 5 28

    it "Ordinal date" $
      check Decode.ordinalDate Encode.ordinalDate $ fromGregorian 2134 5 28

    describe "Time of day" $ do
      it "12:34:56.789012345678" $
        check Decode.timeOfDay Encode.timeOfDay $ TimeOfDay 12 34 56.789012345678

      it "12:34:56.00001234" $
        check Decode.timeOfDay Encode.timeOfDay $ TimeOfDay 12 34 56.00001234

      it "00:00:00" $
        check Decode.timeOfDay Encode.timeOfDay $ TimeOfDay 0 0 0

    describe "Time zone" $ do
      it "+00:00" $
        check Decode.timeZone Encode.timeZone $ minutesToTimeZone 0

      it "-04:20" $
        check Decode.timeZone Encode.timeZone $ minutesToTimeZone (-260)

    it "UTC time" $
      check Decode.utcTime Encode.utcTime $
        UTCTime (fromGregorian 2134 5 28) $ sinceMidnight (TimeOfDay 12 34 56.789012345678)

    it "Local time" $
      check Decode.localTime Encode.localTime $
        LocalTime (fromGregorian 2134 5 28) (TimeOfDay 12 34 56.789012345678)

    it "Zoned time" $
      check Decode.zonedTime Encode.zonedTime $
        ZonedTime
          ( LocalTime (fromGregorian 2134 5 28) (TimeOfDay 12 34 56.789012345678) )
          ( minutesToTimeZone 3438 )
