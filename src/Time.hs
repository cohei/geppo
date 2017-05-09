module Time (getLocalToday, lastMonth, beginningOfMonth, endOfMonth, YearMonth, toYearMonth) where

import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime, localDay,
                  Day, fromGregorian, toGregorian, gregorianMonthLength, addGregorianMonthsClip)

getLocalToday :: IO Day
getLocalToday = (localDay .) . utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

lastMonth :: Day -> Day
lastMonth = addGregorianMonthsClip (-1)

data YearMonth = YearMonth { year :: Integer, month :: Int }

toYearMonth :: Day -> YearMonth
toYearMonth d = YearMonth { year = y, month = m }
  where
    (y, m, _) = toGregorian d

beginningOfMonth, endOfMonth :: YearMonth -> Day
beginningOfMonth ym =
  let
    y = year ym
    m = month ym
  in
    fromGregorian y m 1
endOfMonth ym =
  let
    y = year ym
    m = month ym
  in
    fromGregorian y m (gregorianMonthLength y m)
