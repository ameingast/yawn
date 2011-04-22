module Yawn.Server(
  run
) where

import Network
import System.IO
import Control.Exception
import Control.Concurrent

import Yawn.Data
import Yawn.Logger as Log
import Yawn.Parser as Parser

run :: Configuration -> IO ()
run (Configuration port _) = do
  bracket (listenOn $ PortNumber p) sClose f
    where p = fromIntegral port 
          f s = (Log.debug $ "Opened socket on port: " ++ show p) >> loop s

loop :: Socket -> IO ()
loop socket = do
  (h, n, p) <- accept socket
  Log.info $ "Accepted connection: " ++ n ++ ":" ++ show p
  hSetBuffering h NoBuffering
  startWorker h n p
  loop socket

startWorker :: Handle -> String -> PortNumber -> IO (ThreadId)
startWorker h n p = forkIO $ work h n p

work :: Handle -> HostName -> PortNumber -> IO ()
work h n p = do
  i <- readInput h
  Log.info $ "Received: " ++ i
  let pRequest= Parser.parseRequest i
  Log.debug $ "Parsed: " ++ show pRequest
  hClose h

readInput :: Handle -> IO (String)
readInput h = do
  input <- hGetLine h
  -- '\n' is stripped via hGetLine
  if input == "\r" then return "\r\n"
  else readInput h >>= \rest -> return $ input ++ rest
