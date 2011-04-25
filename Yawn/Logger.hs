module Yawn.Logger(
  Level (LOG_INFO, LOG_DEBUG, LOG_ERROR),
  doLog,
  doLog'
) where

import System.IO (IOMode (AppendMode), hPutStr, withFile)
import System.IO.Error (try)
import Time (getClockTime, toCalendarTime, calendarTimeToString)
import Yawn.Configuration (Configuration, logRoot)

data Level = LOG_INFO | LOG_DEBUG | LOG_ERROR deriving (Eq)

instance Show Level where
  show LOG_INFO = "info"
  show LOG_DEBUG = "debug"
  show LOG_ERROR = "error"

-- TODO: introduce logFileHandles into Configuration so they don't have to be 
-- re-opened on each doLog-call
doLog :: Show a => Configuration -> Level -> a -> IO ()
doLog c l s = formatMessage s l >>= writeOut c l

doLog' :: Show a => Level -> a -> IO ()
doLog' l s = formatMessage s l >>= putStrLn

writeOut :: Configuration -> Level -> String -> IO ()
writeOut c l s = do
  let logFile = logRoot c ++ show l ++ ".log"
  try (withFile logFile AppendMode (\h -> hPutStr h (s ++ "\n"))) >>= \st -> case st of
    Left e -> error ("Unable to open logfile. " ++ show e) >> return () 
    Right _ok -> putStrLn s

formatMessage :: Show a => a -> Level -> IO (String)
formatMessage s l = time >>= \t -> return $ "[" ++ t ++ "] [" ++ show l ++ "] " ++ show s

time :: IO (String)
time = getClockTime >>= toCalendarTime >>= (return . calendarTimeToString)
