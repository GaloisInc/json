{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables
           , StandaloneDeriving #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Test.JSON.Decode.Time
  ( time
  ) where

import           Codec.Web.JSON.Decode

import           Data.Time.Calendar
import           Data.Time.Calendar.Month.Compat
import           Data.Time.Calendar.OrdinalDate.Compat
import           Data.Time.Calendar.Quarter.Compat
import           Data.Time.Calendar.WeekDate.Compat
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Test.Hspec



deriving instance Eq Path
deriving instance (Eq r, Eq a) => Eq (Result r a)



instance Eq ZonedTime where
  ZonedTime a c == ZonedTime b d = a == b && c == d



isFailure :: Result r a -> Bool
isFailure (Failure _ _ _) = True
isFailure _               = False



time :: Spec
time =
  describe "Time" $ do
    describe "Year" $ do
      it "2134" $
        decode year "\"2134\"" `shouldBe` Success "" 2134

      it "0567 (trail)" $
        decode year "\"0567\"A" `shouldBe` Success "A" 567

      it "0089" $
        decode year "\"0089\"" `shouldBe` Success "" 89

      it "0001" $
        decode year "\"0001\"" `shouldBe` Success "" 1

      it "0000 (trail)" $
        decode year "\"0000\"A" `shouldBe` Success "A" 0

    describe "Quarter" $ do
      it "2134-Q0" $
        decode quarter "\"2134-Q0\"" `shouldSatisfy` isFailure

      it "2134-Q1" $
        decode quarter "\"2134-Q1\"" `shouldBe` Success "" (YearQuarter 2134 Q1)

      it "2134-Q2 (trail)" $
        decode quarter "\"2134-Q2\"A" `shouldBe` Success "A" (YearQuarter 2134 Q2)

      it "2134-Q3" $
        decode quarter "\"2134-Q3\"" `shouldBe` Success "" (YearQuarter 2134 Q3)

      it "2134-Q4 (trail)" $
        decode quarter "\"2134-Q4\"A" `shouldBe` Success "A" (YearQuarter 2134 Q4)

      it "2134-Q5 (trail)" $
        decode quarter "\"2134-Q5\"A" `shouldSatisfy` isFailure

    describe "Month" $ do
      it "2134-00" $
        decode month "\"2134-00\"" `shouldSatisfy` isFailure

      it "2134-01 (trail)" $
        decode month "\"2134-01\"A" `shouldBe` Success "A" (fromYearMonth 2134 1)

      it "2134-05 (trail)" $
        decode month "\"2134-05\"A" `shouldBe` Success "A" (fromYearMonth 2134 5)

      it "2134-12" $
        decode month "\"2134-12\"" `shouldBe` Success "" (fromYearMonth 2134 12)

      it "2134-13 (trail)" $
        decode month "\"2134-13\"A" `shouldSatisfy` isFailure

    describe "Week" $ do
      it "2134-W00" $
        decode week "\"2134-W00\"" `shouldSatisfy` isFailure

      it "2134-W01" $
        decode week "\"2134-W01\"" `shouldBe` Success "" (2134, 1)

      it "2134-15 (trail)" $
        decode week "\"2134-W15\"A" `shouldBe` Success "A" (2134, 15)

      it "2134-52 (trail)" $
        decode week "\"2134-W52\"A" `shouldBe` Success "A" (2134, 52)

      it "1976-53" $
        decode week "\"1976-W53\"" `shouldBe` Success "" (1976, 53)

      it "2134-53" $
        decode week "\"2134-W53\"" `shouldSatisfy` isFailure

      it "2134-54 (trail)" $
        decode week "\"2134-W53\"A" `shouldSatisfy` isFailure

    describe "Calendar date" $ do
      it "2134-05-00" $
        decode calendarDate "\"2134-05-00\"" `shouldSatisfy` isFailure

      it "2134-05-01" $
        decode calendarDate "\"2134-05-01\"" `shouldBe` Success "" (fromGregorian 2134 5 1)

      it "2134-05-15 (trail)" $
        decode calendarDate "\"2134-05-15\"A"
          `shouldBe` Success "A" (fromGregorian 2134 5 15)

      it "2134-05-31 (trail)" $
        decode calendarDate "\"2134-05-31\"A"
          `shouldBe` Success "A" (fromGregorian 2134 05 31)

      it "2134-06-31" $
        decode calendarDate "\"2134-06-31\"" `shouldSatisfy` isFailure

      it "2134-05-32" $
        decode calendarDate "\"2134-05-32\"" `shouldSatisfy` isFailure

    describe "Week date" $ do
      it "2134-W05-0" $
        decode weekDate "\"2134-W05-0\"" `shouldSatisfy` isFailure

      it "2134-W05-1" $
        decode weekDate "\"2134-W05-1\"" `shouldBe` Success "" (fromWeekDate 2134 5 1)

      it "2134-W05-5 (trail)" $
        decode weekDate "\"2134-W05-5\"A" `shouldBe` Success "A" (fromWeekDate 2134 5 5)

      it "2134-W05-7 (trail)" $
        decode weekDate "\"2134-W05-7\"A" `shouldBe` Success "A" (fromWeekDate 2134 05 7)

      it "2134-W05-8" $
        decode weekDate "\"2134-W05-8\"" `shouldSatisfy` isFailure

    describe "Ordinal date" $ do
      it "2134-000" $
        decode ordinalDate "\"2134-000\"" `shouldSatisfy` isFailure

      it "2134-001" $
        decode ordinalDate "\"2134-001\"" `shouldBe` Success "" (fromOrdinalDate 2134 1)

      it "2134-058 (trail)" $
        decode ordinalDate "\"2134-058\"A" `shouldBe` Success "A" (fromOrdinalDate 2134 58)

      it "2134-365 (trail)" $
        decode ordinalDate "\"2134-365\"A"
          `shouldBe` Success "A" (fromOrdinalDate 2134 365)

      it "2134-366" $
        decode ordinalDate "\"2134-366\"A" `shouldSatisfy` isFailure

      it "2132-366" $
        decode ordinalDate "\"2132-366\"" `shouldBe` Success "" (fromOrdinalDate 2132 366)

      it "2134-367" $
        decode ordinalDate "\"2134-367\"" `shouldSatisfy` isFailure

    describe "Time of day" $ do
      it "00:00:00" $
        decode timeOfDay "\"00:00:00\"" `shouldBe` Success "" (TimeOfDay 0 0 0)

      it "00:00:00.123456789" $
        decode timeOfDay "\"00:00:00.123456789\""
          `shouldBe` Success "" (TimeOfDay 0 0 0.123456789)

      it "00:00:00.12345678912341234 (trail)" $
        decode timeOfDay "\"00:00:00.12345678912341234\"A"
          `shouldBe` Success "A" (TimeOfDay 0 0 0.123456789123)

      it "00:00:00.00000000012341234" $
        decode timeOfDay "\"00:00:00.00000000012341234\""
          `shouldBe` Success "" (TimeOfDay 0 0 0.000000000123)

      it "00:00:60" $
        decode timeOfDay "\"00:00:60\"" `shouldBe` Success "" (TimeOfDay 0 0 60)

      it "00:00:61" $
        decode timeOfDay "\"00:00:61\"" `shouldSatisfy` isFailure

      it "00:59:00 (trail)" $
        decode timeOfDay "\"00:59:00\"A" `shouldBe` Success "A" (TimeOfDay 0 59 0)

      it "00:60:00" $
        decode timeOfDay "\"00:60:00\"" `shouldSatisfy` isFailure

      it "23:00:00" $
        decode timeOfDay "\"23:00:00\"" `shouldBe` Success "" (TimeOfDay 23 0 0)

      it "24:00:00" $
        decode timeOfDay "\"24:00:00\"" `shouldSatisfy` isFailure

    describe "Time zone" $ do
      it "+00:00" $
        decode timeZone "\"+00:00\"" `shouldBe` Success "" (minutesToTimeZone 0)

      it "-00:00 (trail)" $
        decode timeZone "\"-00:00\"A" `shouldBe` Success "A" (minutesToTimeZone 0)

      it "+12:34 (trail)" $
        decode timeZone "\"+12:34\"A" `shouldBe` Success "A" (minutesToTimeZone 754)

      it "-56:78" $
        decode timeZone "\"-56:78\"" `shouldBe` Success "" (minutesToTimeZone (-3438))

    describe "UTC time" $ do
      it "2134-05-28T12:34:56.789012345678901234Z" $
        decode utcTime "\"2134-05-28T12:34:56.789012345678901234Z\""
          `shouldBe` Success "" ( UTCTime (fromGregorian 2134 5 28)
                                    $ sinceMidnight (TimeOfDay 12 34 56.789012345678)
                                )

      it "0000-01-01T00:00:00Z (trail)" $
        decode utcTime "\"0000-01-01T00:00:00Z\"A"
          `shouldBe` Success "A" (UTCTime (fromGregorian 0 1 1) 0)

    describe "Local time" $ do
      it "2134-05-28T12:34:56.789012345678901234" $
        decode localTime "\"2134-05-28T12:34:56.789012345678901234\""
          `shouldBe` Success "" ( LocalTime (fromGregorian 2134 5 28)
                                    (TimeOfDay 12 34 56.789012345678)
                                )

      it "0000-01-01T00:00:00 (trail)" $
        decode localTime "\"0000-01-01T00:00:00\"A"
          `shouldBe` Success "A" (LocalTime (fromGregorian 0 1 1) (TimeOfDay 0 0 0))

    describe "Zoned time" $ do
      it "2134-05-28T12:34:56.789012345678901234+56:78" $
        decode zonedTime "\"2134-05-28T12:34:56.789012345678901234+56:78\""
          `shouldBe` Success "" ( ZonedTime
                                    ( LocalTime (fromGregorian 2134 5 28)
                                        (TimeOfDay 12 34 56.789012345678)
                                    )
                                    ( minutesToTimeZone 3438 )
                                )

      it "0000-01-01T00:00:00-00:00 (trail)" $
        decode zonedTime "\"0000-01-01T00:00:00-00:00\"A"
          `shouldBe` Success "A" ( ZonedTime
                                     ( LocalTime (fromGregorian 0 1 1) (TimeOfDay 0 0 0) )
                                     ( minutesToTimeZone 0 )
                                 )
