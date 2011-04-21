module Yawn.Server(
  run
) where

import Network
import System.IO
import Control.Exception
import Control.Concurrent

import Yawn.Data as Y
import Yawn.Logger as L

run :: Y.Configuration -> IO ()
run (Y.Configuration port _) = do
  L.debug $ "Opening Socket on: " ++ show p 
  bracket (listenOn $ PortNumber p) sClose loop
    where p = fromIntegral port 

loop :: Socket -> IO ()
loop socket = do
  (h, n, p) <- accept socket
  hSetBuffering h LineBuffering
  _ <- forkIO $ work h n p 
  loop socket

work :: Handle -> HostName -> PortNumber -> IO ()
work h n p = do
  L.info $ "Accepting connection: " ++ n ++ ":" ++ show p
  i <- readInput h
  L.debug $ "Received: " ++ (show i)
  hClose h

readInput :: Handle -> IO ([String])
readInput h = do
  input <- hGetLine h
  if endOfRequest input then return []
  else readInput h >>= \rest -> return $ (stripLineFeeds input):rest
      
endOfRequest :: String -> Bool
endOfRequest s = s == "\r" || s == "\n" || s == "\r\n"

stripLineFeeds :: String -> String
stripLineFeeds = filter ('\r' /=) . filter ('\n' /=)

