module Yawn.Server(
  start
) where

import Control.Concurrent (MVar, newMVar, forkIO)
import Control.Exception (bracket)
import Network (Socket, PortID (PortNumber), listenOn, sClose, accept)
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import Yawn.Configuration (Configuration, port)
import Yawn.Context (Context, makeContext, configuration, get, close)
import Yawn.Dispatcher (dispatchRequest, badRequest)
import Yawn.HTTP.RequestParser (parseRequest)
import Yawn.HTTP.Request (Request, HttpVersion(HTTP_1_0, HTTP_1_1), findHeader, version)
import Yawn.Logger (Level (LOG_DEBUG, LOG_INFO), doLog)
import Yawn.Mime (MimeDictionary)

start :: Configuration -> MimeDictionary -> IO ()
start conf dict = 
  let run = (listenOn . PortNumber . fromIntegral . port) conf
  in bracket run sClose (startSocket conf dict)

startSocket :: Configuration -> MimeDictionary -> Socket -> IO ()
startSocket conf dict socket = do
  doLog conf LOG_DEBUG $ "Listening on port: " ++ (show $ port conf)
  lock <- newMVar ()
  loop conf dict socket lock

-- TODO: add First parameter. if first then less timeout else http-1.1 std timeout
loop :: Configuration -> MimeDictionary -> Socket -> MVar () -> IO ()
loop conf dict socket l = do
  (h, n, p) <- accept socket
  doLog conf LOG_INFO $ "Accepted connection from " ++ n ++ ":" ++ show p
  hSetBuffering h NoBuffering
  forkIO $ work $ makeContext conf dict l h
  loop conf dict socket l

-- TODO: only use close for http/1.0 connections and use a timeout for 1.1
work :: Context -> IO ()
work ctx  = do
  let conf = configuration ctx
  i <- get ctx
  doLog conf LOG_INFO $ "Received: " ++ i
  case parseRequest i of
    Left _e -> badRequest ctx
    Right r -> (doLog conf LOG_DEBUG $ "Parsed: " ++ show r) >> dispatchRequest ctx r 
  -- if keep-alive && <= keep-alive timeout listen for more input
  close ctx

-- Under HTTP/1.0 all connections are closed unless Connection: Keep-Alive is supplied
-- Under HTTP/1.1 all connections are open unless Connection: close is supplied
isKeepAlive :: Request -> Bool
isKeepAlive r = case findHeader "Connection" r of 
  Nothing -> version r == HTTP_1_1
  Just con -> if version r == HTTP_1_0 then "Keep-Alive" == con
              else "close" /= con
