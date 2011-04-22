module Yawn.Server(
  run
) where

import Control.Exception
import Control.Concurrent
import Network
import System.IO
import Yawn.Data
import Yawn.Logger as Log
import Yawn.Parser as Parser

run :: Configuration -> IO ()
run c = let p = (listenOn . PortNumber . fromIntegral . port) c 
        in bracket p sClose startSocket

startSocket :: Socket -> IO ()
startSocket s = do
  (Log.debug $ "Listening on port: " ++ show s)
  lock <- newMVar ()
  loop s lock

loop :: Socket -> MVar () -> IO ()
loop socket l = do
  (h, n, p) <- accept socket
  Log.info $ "Accepted connection: " ++ n ++ ":" ++ show p
  hSetBuffering h NoBuffering
  startWorker l h n p
  loop socket l

startWorker :: MVar () -> Handle -> String -> PortNumber -> IO (ThreadId)
startWorker l h n p = forkIO $ work l h n p

work :: MVar () -> Handle -> HostName -> PortNumber -> IO ()
work l h n p = do
  i <- readInput h
  Log.info $ "Received: " ++ i
  case Parser.parseRequest i of
    Left e  -> Log.err e
    Right r -> (Log.debug $ "Parsed: " ++ show r) >> dispatchRequest l r h
  hClose h

readInput :: Handle -> IO (String)
readInput h = do
  input <- hGetLine h
  -- '\n' is stripped via hGetLine
  if input == "\r" then return "\r\n"
  else readInput h >>= \rest -> return $ input ++ rest

dispatchRequest :: MVar () -> Request -> Handle -> IO ()
dispatchRequest l r h = case method(r) of
                          GET -> getResource l r h
                          _ -> Log.err $ "Unsupported request" ++ show r

getResource :: MVar () -> Request -> Handle -> IO ()
getResource l r h = do
  f <- readFile filePath 
  withMVar l (\a -> hPutStr h f >> return a)
  where filePath = "Main.hs"

