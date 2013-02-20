module Yawn.Test.Common (
  transmit,
  transmitLines,
  withServer
) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import System.IO (hPutStr, hFlush, hGetLine, hClose, Handle)
import Network (PortID(PortNumber), connectTo)
import Yawn.Application (start)
import Yawn.Logger (system)

-- FIXME: replace this with haskells internal HTTP implementation
transmit :: String -> IO (String)
transmit s = transmitLines s 1

transmitLines :: String -> Integer -> IO (String)
transmitLines s l = do
  handle <- connectTo "127.0.0.1" (PortNumber 9000)
  hPutStr handle $ s ++ "\r\n\r\n" 
  hFlush handle
  answer <- getLines l handle
  hClose handle
  return answer
  
-- FIXME: refactor into something actually usable
-- FIXME: throws EOL exception if l > available content
getLines :: Integer -> Handle -> IO (String)
getLines l handle | l <= 0 = return ""
getLines l handle = do
  x <- hGetLine handle
  more <- getLines (l - 1) handle
  return $ x ++ more

withServer :: String -> IO () -> IO () 
withServer testRoot work = do
  serverThread <- forkIO $ start "www"
  sleep 2
  work
  sleep 2
  killThread serverThread

sleep :: Int -> IO ()
sleep s = threadDelay $ fromIntegral s * 100000
