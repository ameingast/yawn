module Yawn.Util.Time (
  getAscDate,
  getCalendarTime,
  clockTimeToAscDate
) where

import System.Locale(defaultTimeLocale)
import Time (ClockTime, getClockTime, toCalendarTime, formatCalendarTime, calendarTimeToString)

type TimeString = String

-- ANSI C's asctime() format
-- > Sun Nov  6 08:49:37 1994
getAscDate :: IO (String)
getAscDate = 
  getClockTime >>= clockTimeToAscDate

clockTimeToAscDate :: ClockTime -> IO (String)
clockTimeToAscDate time = 
  let timestr = "%a %b %d %H:%M:%S %Y"
  in toCalendarTime time >>= return . formatCalendarTime defaultTimeLocale timestr

getCalendarTime :: IO (String)
getCalendarTime = 
  getClockTime >>= toCalendarTime >>= (return . calendarTimeToString)
