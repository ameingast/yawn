module Yawn.Logger(
  Level (LOG_INFO, LOG_DEBUG, LOG_ERROR),
  doLog,
  system,
  trace
) where

import System.IO (IOMode (AppendMode), hPutStr, withFile)
import System.IO.Error (try)
import Yawn.Util.Time (getCalendarTime)
import Yawn.Configuration (Configuration, logRoot)

data Level = LOG_INFO | LOG_DEBUG | LOG_ERROR | LOG_TRACE deriving (Eq)

instance Show Level where
  show LOG_INFO = "info"
  show LOG_DEBUG = "debug"
  show LOG_ERROR = "error"
  show LOG_TRACE = "trace"

doLog :: Show a => Configuration -> Level -> a -> IO ()
doLog c l s = formatMessage s l >>= writeOut c l

system :: Show a => a -> IO ()
system s = formatMessage s LOG_INFO >>= putStrLn

trace :: Show a => a -> IO ()
trace s = formatMessage s LOG_TRACE >>= putStrLn

writeOut :: Configuration -> Level -> String -> IO ()
writeOut c l s = do
  let logFile = logRoot c ++ show l ++ ".log"
  try (withFile logFile AppendMode (\h -> hPutStr h (s ++ "\n"))) >>= \st -> case st of
    Left e -> error ("Unable to open logfile. " ++ show e) >> return () 
    Right _ok -> putStrLn s

formatMessage :: Show a => a -> Level -> IO (String)
formatMessage s l = getCalendarTime >>= \t -> return $ "[" ++ t ++ "] [" ++ show l ++ "] " ++ show s
