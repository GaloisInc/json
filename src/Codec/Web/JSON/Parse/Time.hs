{-# LANGUAGE BangPatterns
           , NumericUnderscores #-}

module Codec.Web.JSON.Parse.Time
  ( yearP

  , quarterP

  , monthP

  , weekP

  , calendarDateP
  , weekDateP
  , ordinalDateP

  , timeOfDayP
  , timeZoneP

  , utcTimeP
  , localTimeP
  , zonedTimeP
  ) where

import           Data.Attoparsec.Error

import           Data.Attoparsec.ByteString
import           Data.Fixed
import           Data.Time.Calendar
import           Data.Time.Calendar.Month.Compat
import           Data.Time.Calendar.MonthDay.Compat
import           Data.Time.Calendar.OrdinalDate.Compat
import           Data.Time.Calendar.Quarter.Compat
import           Data.Time.Calendar.WeekDate.Compat
import           Data.Time.Clock
import           Data.Time.Format.ISO8601.Compat
import           Data.Time.LocalTime.Compat
import           Data.Word
import           Numeric



{-# INLINE is0to9 #-}
is0to9 :: Word8 -> Bool
is0to9 w = w <= 0x39 {- 9 -} && w >= 0x30 {- 0 -}

{-# INLINE peekDigit #-}
peekDigit :: String -> Parser Word8
peekDigit str = do
  w <- peekWord8'
  if is0to9 w
    then pure $ w - 0x30 {- 0 -}
    else err $ "Unexpected byte 0x" <> showHex w str

{-# INLINE byte #-}
byte :: Word8 -> String -> Parser ()
byte x str = do
  w <- peekWord8'
  if w == x
    then () <$ anyWord8
    else err $ "Unexpected byte 0x" <> showHex w str



{-# INLINE justYearP #-}
justYearP :: Parser Year
justYearP = do
  let here = " in year"
  y0 <- peekDigit here
  _ <- anyWord8
  y1 <- peekDigit here
  _ <- anyWord8
  y2 <- peekDigit here
  _ <- anyWord8
  y3 <- peekDigit here
  _ <- anyWord8
  pure $ fromIntegral y0 * 1000
       + fromIntegral y1 * 100
       + fromIntegral y2 * 10
       + fromIntegral y3



yearP :: Parser Year
yearP = do
  year <- justYearP
  year <$ byte 0x22 " in year"



quarterP :: Parser Quarter
quarterP = do
  let here = " in quarter"
  year <- justYearP
  byte 0x2d {- - -} here
  byte 0x51 {- Q -} here
  q <- peekWord8'
  case q of
    0x31 -> do _ <- anyWord8
               fromYearQuarter year Q1 <$ byte 0x22 here
    0x32 -> do _ <- anyWord8
               fromYearQuarter year Q2 <$ byte 0x22 here
    0x33 -> do _ <- anyWord8
               fromYearQuarter year Q3 <$ byte 0x22 here
    0x34 -> do _ <- anyWord8
               fromYearQuarter year Q4 <$ byte 0x22 here
    _ | is0to9 q  -> err "Quarter must be in range 1-4"
      | otherwise -> err $ "Unexpected byte 0x" <> showHex q here



{-# INLINE justMonthP #-}
justMonthP :: Parser MonthOfYear
justMonthP = do
  let here = " in month"
  m0 <- peekDigit here
  _ <- anyWord8
  m1 <- peekDigit here

  let m = fromIntegral m0 * 10 + fromIntegral m1
  if m > 12 || m < 1
    then err "Month must be in 01-12 range"
    else do
      _ <- anyWord8
      pure m



monthP :: Parser Month
monthP = do
  let here = " in month"
  year <- justYearP
  byte 0x2d {- - -} here
  month <- justMonthP
  fromYearMonth year month <$ byte 0x22 here



justCalendarDateP :: Parser Day
justCalendarDateP = do
  let here = " in calendar date"
  year <- justYearP
  byte 0x2d {- - -} here
  month <- justMonthP
  byte 0x2d {- - -} here

  d0 <- peekDigit here
  _ <- anyWord8
  d1 <- peekDigit here

  let largestDay = monthLength (isLeapYear year) month
      day = fromIntegral d0 * 10 + fromIntegral d1
  if day > largestDay || day < 1
    then err $ "For month of "
            <> shows (fromYearMonth year month) " day must be in 01-"
            <> shows largestDay " range"

    else do
      _ <- anyWord8
      pure $ fromGregorian year month day



calendarDateP :: Parser Day
calendarDateP = do
  date <- justCalendarDateP
  date <$ byte 0x22 " in calendar date"



{-# INLINE justWeekP #-}
justWeekP :: Parser WeekOfYear
justWeekP = do
  let here = " in week"
  ww <- peekWord8'
  if ww == 0x57 {- W -}
    then do
      _ <- anyWord8
      w0 <- peekDigit here
      _ <- anyWord8
      w1 <- peekDigit here

      let w = fromIntegral w0 * 10 + fromIntegral w1
      if w > 53 || w < 1
        then err "Week must be in 01-53 range"
        else do _ <- anyWord8
                pure w

    else err $ "Unexpected byte 0x" <> showHex ww here


weekP :: Parser (Year, WeekOfYear)
weekP = do
  let here = " in week"
  year <- justYearP
  byte 0x2d {- - -} here
  week <- justWeekP
  byte 0x22 here
  case fromWeekDateValid year week 1 of
    Just _  -> pure (year, week)
    Nothing -> err . showString "Year of " . (formatShow yearFormat year <>)
                   . showString " has no W" $ show week



weekDateP :: Parser Day
weekDateP = do
  let here = " in week day"
  year <- justYearP
  byte 0x2d {- - -} here
  week <- justWeekP
  byte 0x2d {- - -} here

  d0 <- peekDigit here

  let d = fromIntegral d0
  if d > 7 || d < 1
    then err "Week day must be in 1-7 range"
    else case fromWeekDateValid year week d of
           Just day -> do _ <- anyWord8
                          day <$ byte 0x22 here
           Nothing  ->
             err . showString "Year of " . (formatShow yearFormat year <>)
                 . showString " has no W" . shows week
                 $ showChar '-' (show d)



ordinalDateP :: Parser Day
ordinalDateP = do
  let here = " in ordinal date"
  year <- justYearP
  byte 0x2d {- - -} here

  d0 <- peekDigit here
  _ <- anyWord8
  d1 <- peekDigit here
  _ <- anyWord8
  d2 <- peekDigit here

  let d = fromIntegral d0 * 100 + fromIntegral d1 * 10 + fromIntegral d2
  if d > 366 || d < 1
    then err "Ordinal day must be in 1-366 range"
    else
      case fromOrdinalDateValid year d of
        Just day -> do _ <- anyWord8
                       day <$ byte 0x22 here
        Nothing  ->
          err . showString "Year of " . (formatShow yearFormat year <>)
              . showString " has no ordinal day " $ show d





secondsP :: Parser Pico
secondsP = do
  let here = " in time of date"
  s0 <- peekDigit here
  _ <- anyWord8
  s1 <- peekDigit here
  _ <- anyWord8

  let seconds = fromIntegral s0 * 10 + fromIntegral s1
  if seconds > 60
    then err "Seconds must be in the 00-60 range"
    else pure ()

  p <- peekWord8'
  if p == 0x2e
    then do _ <- anyWord8
            precise seconds 12

    else pure . MkFixed $ seconds * 1_000_000_000_000
  where
    precise :: Integer -> Int -> Parser Pico
    precise n i
      | i <= 0    = do skipWhile is0to9
                       pure $ MkFixed n
      | otherwise = do
          w <- peekWord8'
          if is0to9 w
            then do _ <- anyWord8
                    precise (n * 10 + fromIntegral w - 0x30) (i - 1)
            else pure . MkFixed $ n * (10 ^ i)



justTimeOfDayP :: Parser TimeOfDay
justTimeOfDayP = do
  let here = " in time of day"
  h0 <- peekDigit here
  _ <- anyWord8
  h1 <- peekDigit here
  _ <- anyWord8

  let hours = fromIntegral h0 * 10 + fromIntegral h1
  if hours >= 24
    then err "Hours must be in the 00-24 range"
    else pure ()

  byte 0x3a {- : -} here

  m0 <- peekDigit here
  _ <- anyWord8
  m1 <- peekDigit here
  _ <- anyWord8

  let minutes = fromIntegral m0 * 10 + fromIntegral m1
  if minutes >= 60
    then err "Minutes must be in the 00-59 range"
    else pure ()

  byte 0x3a {- : -} here

  TimeOfDay hours minutes <$> secondsP



timeOfDayP :: Parser TimeOfDay
timeOfDayP = do
  time <- justTimeOfDayP
  time <$ byte 0x22 " in time of day"



{-# INLINE justTimeZoneP #-}
justTimeZoneP :: Parser TimeZone
justTimeZoneP = do
  let here = " in time zone"

  negative <- do s <- peekWord8'
                 case s of
                   0x2b {- + -} -> False <$ anyWord8
                   0x2d {- - -} -> True <$ anyWord8
                   _            -> err $ "Unexpected byte 0x" <> showHex s here

  h0 <- peekDigit here
  _ <- anyWord8
  h1 <- peekDigit here
  _ <- anyWord8

  byte 0x3a {- : -} here

  m0 <- peekDigit here
  _ <- anyWord8
  m1 <- peekDigit here
  _ <- anyWord8

  let minutes = fromIntegral h0 * 600
              + fromIntegral h1 * 60
              + fromIntegral m0 * 10
              + fromIntegral m1

  pure . minutesToTimeZone $ if negative then negate minutes else minutes



timeZoneP :: Parser TimeZone
timeZoneP = do
  zone <- justTimeZoneP
  zone <$ byte 0x22 " in time zone"



isT :: Word8 -> Bool
isT w = w == 0x20 {-   -} || w == 0x54 {- T -} || w == 0x74 {- t -}

isZ :: Word8 -> Bool
isZ w = w == 0x5A {- Z -} || w == 0x7A {- z -}



utcTimeP :: Parser UTCTime
utcTimeP = do
  let here = " in UTC timestamp"

  date <- justCalendarDateP

  _ <- do w <- peekWord8'
          if isT w
            then () <$ anyWord8
            else err $ "Unexpected byte 0x" <> showHex w here

  time <- justTimeOfDayP

  _ <- do w <- peekWord8'
          if isZ w
            then () <$ anyWord8
            else err $ "Unexpected byte 0x" <> showHex w here

  UTCTime date (sinceMidnight time) <$ byte 0x22 here



{-# INLINE justLocalTimeP #-}
justLocalTimeP :: Parser LocalTime
justLocalTimeP = do
  let here = " in UTC timestamp"

  date <- justCalendarDateP

  _ <- do w <- peekWord8'
          if isT w
            then () <$ anyWord8
            else err $ "Unexpected byte 0x" <> showHex w here

  LocalTime date <$> justTimeOfDayP



localTimeP :: Parser LocalTime
localTimeP = do
  time <- justLocalTimeP
  time <$ byte 0x22 " in local time"



zonedTimeP :: Parser ZonedTime
zonedTimeP = do
  time <- justLocalTimeP
  zone <- justTimeZoneP
  ZonedTime time zone <$ byte 0x22 " in zoned time"
