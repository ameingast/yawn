module Yawn.Server(
  start
) where

import Control.Concurrent (MVar, newMVar, forkIO)
import Control.Exception (bracket)
import Network (Socket, PortID (PortNumber), listenOn, sClose, accept)
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import Yawn.Configuration (Configuration, port)
import Yawn.Context (Context, makeContext, configuration, get, getBlocking, getBytes, close)
import Yawn.Dispatcher (dispatchRequest, badRequest)
import Yawn.HTTP.Request
import Yawn.HTTP.RequestParser (parse)
import Yawn.Logger (Level (LOG_DEBUG, LOG_INFO), doLog)
import Yawn.Mime (MimeDictionary)
import Yawn.Util.Counter (makeCounter)
import Yawn.Util.Maybe (liftIOMaybe)
import qualified Data.ByteString as BS (ByteString, length, append)
import qualified Data.ByteString.Char8 as BS8 (unpack)

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
--       bind socket properly
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
  liftIOMaybe Nothing (work' ctx) (get ctx)
  -- if keep-alive && <= keep-alive timeout listen for more input
  close ctx

work' :: Context -> BS.ByteString -> IO (Maybe ())
work' ctx bs = do
  let conf = configuration ctx
  doLog conf LOG_DEBUG $ "Received: " ++ show bs
  counter <- makeCounter 0
  parse (getBlocking ctx counter) bs >>= \p -> case p of 
    Left e -> doLog conf LOG_DEBUG (show e) >> badRequest ctx
    Right parsed -> handleParse ctx parsed

handleParse :: Context -> (BS.ByteString, Request) -> IO (Maybe ())
handleParse ctx (unconsumed, r) = do
  let conf = configuration ctx
  doLog conf LOG_DEBUG ("Parsed request: " ++ show r) 
  dispatchRequest ctx =<< addBody ctx (unconsumed, r)

addBody :: Context -> (BS.ByteString, Request) -> IO (Request)
addBody ctx (unconsumed, r) = do
  let conf = configuration ctx
  case contentLength r of 
    0 -> return r
    len -> do 
      let remainingLength = len - BS.length unconsumed
      getBytes ctx remainingLength >>= \rest -> case rest of
        Nothing -> return r
        Just rest' -> do
          let fullBody = BS8.unpack $ unconsumed `BS.append` rest'
          doLog conf LOG_DEBUG $ "Received request body: " ++ show fullBody
          return $ r { body = fullBody }
