module Yawn.Test.Common (
  transmit,
  withServer
) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import IO (hPutStr, hFlush, hGetLine, hClose)
import Network (PortID(PortNumber), connectTo)
import Yawn.Application (start)
import Yawn.Logger (system)

transmit :: String -> IO (String)
transmit s = do
  handle <- connectTo "127.0.0.1" (PortNumber 9000)
  hPutStr handle $ s ++ "\r\n\r\n" 
  hFlush handle
  answer <- hGetLine handle 
  hClose handle
  return answer

withServer :: String -> IO () -> IO () 
withServer testRoot work = do
  serverThread <- forkIO $ start "www"
  sleep 2
  work
  sleep 2
  killThread serverThread

sleep :: Int -> IO ()
sleep s = threadDelay $ fromIntegral s * 100000
