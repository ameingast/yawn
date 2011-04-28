module Yawn.Util.Time where

import Time (getClockTime, toCalendarTime, formatCalendarTime, calendarTimeToString)
import System.Locale(defaultTimeLocale)

-- ANSI C's asctime() format
-- > Sun Nov  6 08:49:37 1994
getAscDate :: IO (String)
getAscDate = 
  let timestr = "%a %b %d %H:%M:%S %Y"
  in getClockTime >>= toCalendarTime >>= (return . formatCalendarTime defaultTimeLocale timestr)

getCalendarTime :: IO (String)
getCalendarTime = getClockTime >>= toCalendarTime >>= (return . calendarTimeToString)
