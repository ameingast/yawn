module Yawn.Logger(
  info,
  debug,
  err
) where

import Time

info :: Show a => a -> IO ()
info s = formatMessage s "info" >>= putStrLn

debug :: Show a => a -> IO ()
debug s = formatMessage s "debug" >>= putStrLn

err :: Show a => a -> IO ()
err s = formatMessage s "error" >>= putStrLn

formatMessage :: Show a => a -> String -> IO (String)
formatMessage s ch = time >>= \t -> return $ "[" ++ t ++ "] [" ++ ch ++ "] " ++ show s

time :: IO String
time = getClockTime >>= toCalendarTime >>= (return . calendarTimeToString)
