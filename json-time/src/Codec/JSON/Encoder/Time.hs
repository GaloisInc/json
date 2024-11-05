{-# LANGUAGE DataKinds #-}

{-| Functions for encoding common time formats in RFC 8259 JSONs.
 -}

module Codec.JSON.Encoder.Time
  ( -- * Calendar
    year
  , quarter

    -- ** Month
  , month
  , monthDate

    -- ** Week
  , weekDate

    -- ** Ordinal
  , ordinalDate

    -- * Time of day
  , timeOfDay

    -- ** Local
  , localTime

    -- ** Zoned
  , utcTime
  , timeZone
  , zonedTime
  ) where

import           Codec.JSON.Encoder.Unsafe

import qualified Builder.Lathe.Time as Builder
import           Data.ByteString.Builder
import           Data.Time.Calendar
import           Data.Time.Calendar.Month
import           Data.Time.Calendar.Quarter
import           Data.Time.Clock
import           Data.Time.LocalTime



-- | Encode a year as a JSON string in ISO 8601 extended @[-…y]yyyy@ format
--
--   Compatible with RFC 3339 if the year is in the \([0,10000)\) interval.
year :: Year -> Encoder
year y = Encoder $ word8 0x22 <> Builder.year y <> word8 0x22



-- | Encode a year quarter as a JSON string in @[-…y]yyyy-Qq@ format.
--
--   Note that year quarters are not defined by ISO 8601, this is an informal convention.
quarter :: Quarter -> Encoder
quarter q = Encoder $ word8 0x22 <> Builder.quarter q <> word8 0x22



-- | Encode a month date as a JSON string, in ISO 8601 extended @[-…y]yyyy-mm@ format.
--
--   RFC 3339 compatibility matches that of the 'year' function.
month :: Month -> Encoder
month m = Encoder $ word8 0x22 <> Builder.month m <> word8 0x22



-- | Encode a month date as a JSON string in ISO 8601 extended @[-…y]yyyy-mm-dd@ format.
--
--   RFC 3339 compatibility matches that of the 'year' function.
monthDate :: Day -> Encoder
monthDate d = Encoder $ word8 0x22 <> Builder.monthDate d <> word8 0x22



-- | Encode a week date as a JSON string in ISO 8601 extended @[-…y]yyyy-Www-d@ format.
weekDate :: Day -> Encoder
weekDate d = Encoder $ word8 0x22 <> Builder.weekDate d <> word8 0x22



-- | Encode an ordinal date as a JSON string in ISO 8601 extended @[-…y]yyyy-ddd@ format.
ordinalDate :: Day -> Encoder
ordinalDate d = Encoder $ word8 0x22 <> Builder.ordinalDate d <> word8 0x22



-- | Encode a time of day as a JSON string in RFC 3339 @hh:mm:ss[.s…]@ format.
timeOfDay :: TimeOfDay -> Encoder
timeOfDay t = Encoder $ word8 0x22 <> Builder.timeOfDay t <> word8 0x22



-- | Encode a local time as a JSON string in ISO 8601 extended
--   @[-…y]yyyy-mm-ddThh:mm:ss[.s…]@ format.
--
--   RFC 3339 compatibility matches that of the 'year' function.
localTime :: LocalTime -> Encoder
localTime l = Encoder $ word8 0x22 <> Builder.localTime l <> word8 0x22



-- | Encode a UTC time as a JSON string in ISO 8601 extended
--   @[-…y]yyyy-mm-ddThh:mm:ss[.s…]Z@ format.
--
--   RFC 3339 compatibility matches that of the 'year' function.
utcTime :: UTCTime -> Encoder
utcTime u = Encoder $ word8 0x22 <> Builder.utcTime u <> word8 0x22



-- | Encode a time zone as a JSON string in ISO 8601 extended @±[…h]hh:mm@ format.
--
--   Compatible with RFC 3339 if the minute offset from UTC is
--   in the \((-24 \cdot 60, 24 \cdot 60)\) interval.
timeZone :: TimeZone -> Encoder
timeZone t = Encoder $ word8 0x22 <> Builder.timeZone t <> word8 0x22



-- | Encode a zoned time as a JSON string in ISO 8601 extended
--   @[-…y]yyyy-mm-ddThh:mm:ss[.s…]±[…h]hh:mm@ format.
--
--   RFC 3339 compatibility matches that of both the 'year' and 'timeZone' functions.
zonedTime :: ZonedTime -> Encoder
zonedTime z = Encoder $ word8 0x22 <> Builder.zonedTime z <> word8 0x22
