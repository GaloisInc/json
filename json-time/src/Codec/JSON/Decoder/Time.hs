{-# LANGUAGE BangPatterns #-}

{-| Functions for decoding common time formats in RFC 8259 JSONs.
 -}

module Codec.JSON.Decoder.Time
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
  , timeZone
  , zonedTime
  ) where

import           Codec.JSON.Decoder
import           Codec.JSON.Decoder.Unsafe
import           Codec.JSON.Decoder.String.Internal

import           Data.Time.Calendar
import           Data.Time.Calendar.Month
import           Data.Time.Calendar.Quarter
import           Data.Time.LocalTime
import           Parser.Lathe
import qualified Parser.Lathe.Time as Lathe



oob :: String -> Error
oob name =
  Malformed Benign $
    showString name " is out of bounds"


malformed :: String -> String -> Error
malformed name format =
  Malformed Benign $
    showString "Expected " . showString name $ showString " encoded as " format

trailing :: String -> Error
trailing name =
  Malformed Benign $
    showString "Found trailing data after the " name



-- | Decode a JSON string as an RFC 3339 year, in the @yyyy@ format.
year :: Decoder Year
year =
  Decoder $ \path bits k ->
    case k of
      S -> embedP 64 path $ do
             u <- Lathe.year (path, malformed "year" "yyyy") (path, AbruptEnd)
             end <- atEnd
             if end
               then pure u
               else err (path, trailing "year")

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)



-- | Decode a JSON string as a quarter, in the @yyyy-Qqq@ format.
--
--   Note that year quarters are not defined by ISO 8601, this is an informal convention.
quarter :: Decoder Quarter
quarter =
  Decoder $ \path bits k ->
    case k of
      S -> embedP 64 path $ do
             u <- Lathe.quarter (path, oob "Quarter")
                                (path, malformed "quarter" "yyyy-Qq")
                                (path, AbruptEnd)
             end <- atEnd
             if end
               then pure u
               else err (path, trailing "quarter")

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)



-- | Decode a JSON string as an RFC 3339 month, in the @yyyy-mm@ format.
month :: Decoder Month
month =
  Decoder $ \path bits k ->
    case k of
      S -> embedP 64 path $ do
             u <- Lathe.month (path, oob "Month")
                              (path, malformed "month" "yyyy-mm")
                              (path, AbruptEnd)
             end <- atEnd
             if end
               then pure u
               else err (path, trailing "month")

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)



-- | Decode a JSON string as an RFC 3339 month date, in the @yyyy-mm-dd@ format.
monthDate :: Decoder Day
monthDate =
  Decoder $ \path bits k ->
    case k of
      S -> embedP 64 path $ do
             u <- Lathe.monthDate (path, oob "Month")
                                  (path, malformed "month" "yyyy-mm-dd")
                                  (path, AbruptEnd)
             end <- atEnd
             if end
               then pure u
               else err (path, trailing "month date")

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)



-- | Decode a JSON string as an ISO 8601 week date, in the @yyyy-Www-d@ format.
weekDate :: Decoder Day
weekDate =
  Decoder $ \path bits k ->
    case k of
      S -> embedP 64 path $ do
             u <- Lathe.weekDate (path, oob "Week date")
                                 (path, malformed "week date" "yyyy-Www-d")
                                 (path, AbruptEnd)
             end <- atEnd
             if end
               then pure u
               else err (path, trailing "week date")

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)



-- | Decode a JSON string as an ISO 8601 ordinal date, in the @yyyy-ddd@ format.
ordinalDate :: Decoder Day
ordinalDate =
  Decoder $ \path bits k ->
    case k of
      S -> embedP 64 path $ do
             u <- Lathe.ordinalDate (path, oob "Ordinal date")
                                    (path, malformed "ordinal date" "yyyy-ddd")
                                    (path, AbruptEnd)
             end <- atEnd
             if end
               then pure u
               else err (path, trailing "ordinal date")

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)



-- | Decode a JSON string as an RFC 3339 time of day, in the @hh:mm:ss[.s…]@ format.
timeOfDay :: Decoder TimeOfDay
timeOfDay =
  Decoder $ \path bits k ->
    case k of
      S -> embedP 64 path $ do
             u <- Lathe.timeOfDay (path, oob "Time of day")
                                  (path, malformed "time of day" "hh:mm:ss[.s…]")
                                  (path, AbruptEnd)
             end <- atEnd
             if end
               then pure u
               else err (path, trailing "time of day")

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)



-- | Decode a JSON string as an RFC 3339 local time, in the @yyyy-mm-ddThh:mm:ss[.s…]@ format.
--
--   Either @t@ or space is accepted instead of @T@.
localTime :: Decoder LocalTime
localTime =
  Decoder $ \path bits k ->
    case k of
      S -> embedP 64 path $ do
             u <- Lathe.localTime (path, oob "Local time")
                                  (path, malformed "local time" "yyyy-mm-ddThh:mm:ss[.s…]")
                                  (path, AbruptEnd)
             end <- atEnd
             if end
               then pure u
               else err (path, trailing "local time")

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)



-- | Decode a JSON string as an RFC 3339 time zone, in either @Z@ or @±hh:mm@ format.
--
--   @z@ is accepted instead of @Z@.
timeZone :: Decoder TimeZone
timeZone =
  Decoder $ \path bits k ->
    case k of
      S -> embedP 64 path $ do
             u <- Lathe.timeZone (path, oob "Time zone")
                                 (path, malformed "time zone" "either Z or ±hh:mm")
                                 (path, AbruptEnd)
             end <- atEnd
             if end
               then pure u
               else err (path, trailing "time zone")

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)




-- | Decode a JSON string as an RFC 3339 zoned time,
--   in the @yyyy-mm-ddThh:mm:ss[.s…]±hh:mm@ format.
--
--   Combines behaviors of both 'timeOfDay' and 'zonedTime'.
zonedTime :: Decoder ZonedTime
zonedTime =
  Decoder $ \path bits k ->
    case k of
      S -> embedP 64 path $ do
             u <- Lathe.zonedTime (path, oob "Zoned time")
                                  (path, malformed "zoned time" "yyyy-mm-ddThh:mm:ss[.s…]±hh:mm")
                                  (path, AbruptEnd)
             end <- atEnd
             if end
               then pure u
               else err (path, trailing "zoned time")

      _ ->
        let !bits' = bits <> StringBit
        in err (path, Mismatch bits' k)
