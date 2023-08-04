{-# LANGUAGE NumericUnderscores #-}

module Codec.Web.JSON.Build.Time
  ( yearB

  , quarterB

  , monthB

  , weekB

  , calendarDateB
  , weekDateB
  , ordinalDateB

  , timeOfDayB
  , timeZoneB

  , utcTimeB
  , localTimeB
  , zonedTimeB
  ) where

import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as Build
import           Data.ByteString.Builder.Prim
import           Data.Fixed
import           Data.Time.Calendar
import           Data.Time.Calendar.Month.Compat
import           Data.Time.Calendar.OrdinalDate.Compat
import           Data.Time.Calendar.Quarter.Compat
import           Data.Time.Calendar.WeekDate.Compat
import           Data.Time.Clock
import           Data.Time.LocalTime



yearPrim :: Int -> Builder
yearPrim q0 =
  primFixed (word8 >*< word8 >*< word8 >*< word8) $
           let (q1, y3) = quotRem q0 10
               (q2, y2) = quotRem q1 10
               (y0, y1) = quotRem q2 10

           in ( fromIntegral y0 + 0x30
              , ( fromIntegral y1 + 0x30
                , ( fromIntegral y2 + 0x30
                  , fromIntegral y3 + 0x30
                  )
                )
              )


yearB :: Year -> Builder
yearB year =
  if year >= 0 && year < 10000
    then yearPrim $ fromIntegral year
    else if year < 0 && year > -10000
           then Build.word8 0x2d {- - -} <> yearPrim (abs $ fromIntegral year)
           else Build.integerDec year



quarterB :: Quarter -> Builder
quarterB (YearQuarter year q) =
  yearB year <> primFixed (word8 >*< word8 >*< word8)
                  ( 0x2d {- - -}
                  , ( 0x51 {- Q -}
                    , case q of
                        Q1 -> 0x31
                        Q2 -> 0x32
                        Q3 -> 0x33
                        Q4 -> 0x34
                    )
                  )



monthB :: Month -> Builder
monthB month =
  let (year, mm) = toYearMonth month

      (m0, m1) = quotRem mm 10

  in yearB year <> primFixed (word8 >*< word8 >*< word8)
                     ( 0x2d {- - -}
                     , ( fromIntegral m0 + 0x30
                       , fromIntegral m1 + 0x30
                       )
                     )



calendarDateB :: Day -> Builder
calendarDateB day =
  let (year, mm, dd) = toGregorian day

      (m0, m1) = quotRem mm 10
      (d0, d1) = quotRem dd 10

  in yearB year <> primFixed (word8 >*< word8 >*< word8 >*< word8 >*< word8 >*< word8)
                     ( 0x2d {- - -}
                     , ( fromIntegral m0 + 0x30
                       , ( fromIntegral m1 + 0x30
                         , ( 0x2d {- - -}
                           , ( fromIntegral d0 + 0x30
                             , fromIntegral d1 + 0x30
                             )
                           )
                         )
                       )
                     )



weekB :: Year -> WeekOfYear -> Builder
weekB year week =
  let (w0, w1) = quotRem (rem week 100) 10

  in yearB year <> primFixed (word8 >*< word8 >*< word8 >*< word8)
                     ( 0x2d {- - -}
                     , ( 0x57 {- W -}
                       , ( fromIntegral w0 + 0x30
                         , fromIntegral w1 + 0x30
                         )
                       )
                     )



weekDateB :: Day -> Builder
weekDateB day =
  let (year, ww, d) = toWeekDate day

      (w0, w1) = quotRem ww 10

  in yearB year <> primFixed (word8 >*< word8 >*< word8 >*< word8 >*< word8 >*< word8)
                     ( 0x2d {- - -}
                     , ( 0x57 {- W -}
                       , ( fromIntegral w0 + 0x30
                         , ( fromIntegral w1 + 0x30
                           , ( 0x2d {- - -}
                             , fromIntegral d + 0x30
                             )
                           )
                         )
                       )
                     )



ordinalDateB :: Day -> Builder
ordinalDateB day =
  let (year, ddd) = toOrdinalDate day

      (dd, d2) = quotRem ddd 10
      (d0, d1) = quotRem dd  10

  in yearB year <> primFixed (word8 >*< word8 >*< word8 >*< word8)
                     ( 0x2d {- - -}
                     , ( fromIntegral d0 + 0x30
                       , ( fromIntegral d1 + 0x30
                         , fromIntegral d2 + 0x30
                         )
                       )
                     )



-- | Converts the lowermost 12 digits into a string of form .123456 with trailing zeroes
--   omitted.
subpico :: Integer -> Builder
subpico = detrail 12
  where
    detrail n i = let (q, r) = quotRem i 10
                  in if r == 0
                       then if n <= 0
                              then mempty
                              else detrail (n - 1) q

                       else rebuild n i

    rebuild n i
      | n <= 0    = mempty
      | otherwise = let (q, r) = quotRem i 10
                    in if q == 0
                         then fill (n - 1)      <> Build.word8 (fromIntegral r + 0x30)
                         else rebuild (n - 1) q <> Build.word8 (fromIntegral r + 0x30)

    fill :: Int -> Builder
    fill n
      | n <= 0    = Build.word8 0x2e {- . -}
      | otherwise = fill (n - 1) <> Build.word8 0x30 {- 0 -}




timeOfDayB :: TimeOfDay -> Builder
timeOfDayB (TimeOfDay hours minutes (MkFixed seconds)) =
  let (h0, h1) = quotRem hours   10
      (m0, m1) = quotRem minutes 10

      (ss, sub) = quotRem seconds 1_000_000_000_000

      (s0, s1) = quotRem ss 10

  in primFixed ( word8 >*< word8 >*< word8 >*< word8 >*< word8
                                 >*< word8 >*< word8 >*< word8 )
       ( fromIntegral h0 + 0x30
       , ( fromIntegral h1 + 0x30
         , ( 0x3a {- : -}
           , ( fromIntegral m0 + 0x30
             , ( fromIntegral m1 + 0x30
               , ( 0x3a {- : -}
                 , ( fromIntegral s0 + 0x30
                   , fromIntegral s1 + 0x30
                   )
                 )
               )
             )
           )
         )
       )

   <> subpico sub



timeZoneB :: TimeZone -> Builder
timeZoneB (TimeZone minutes _ _) =
  let sign | minutes < 0 = 0x2d {- - -}
           | otherwise   = 0x2b {- + -}

      (hh, mm) = quotRem (abs minutes) 60

      (h0, h1) = quotRem hh 10
      (m0, m1) = quotRem mm 10

  in primFixed (word8 >*< word8 >*< word8 >*< word8 >*< word8 >*< word8)
       ( sign
       , ( fromIntegral h0 + 0x30
         , ( fromIntegral h1 + 0x30
           , ( 0x3a {- : -}
             , ( fromIntegral m0 + 0x30
               , fromIntegral m1 + 0x30
               )
             )
           )
         )
       )



utcTimeB :: UTCTime -> Builder
utcTimeB (UTCTime day diff) =
  calendarDateB day <> Build.word8 0x54 {- T -}
                    <> timeOfDayB (pastMidnight diff) <> Build.word8 0x5a {- Z -}

localTimeB :: LocalTime -> Builder
localTimeB (LocalTime day time) =
  calendarDateB day <> Build.word8 0x54 {- T -} <> timeOfDayB time

zonedTimeB :: ZonedTime -> Builder
zonedTimeB (ZonedTime time zone) = localTimeB time <> timeZoneB zone
