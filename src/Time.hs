module Time (getLocalToday, lastMonth, beginningOfMonth, endOfMonth) where

import Data.Time (getCurrentTime, getCurrentTimeZone, utcToLocalTime, localDay,
                  Day, fromGregorian, toGregorian, gregorianMonthLength, addGregorianMonthsClip)

getLocalToday :: IO Day
getLocalToday = (localDay .) . utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

lastMonth :: Day -> Day
lastMonth = addGregorianMonthsClip (-1)

beginningOfMonth, endOfMonth :: Day -> Day
beginningOfMonth day =
  let
    (y, m, _) = toGregorian day
  in
    fromGregorian y m 1
endOfMonth day =
  let
    (y, m, _) = toGregorian day
  in
    fromGregorian y m (gregorianMonthLength y m)
