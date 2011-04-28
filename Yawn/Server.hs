module Yawn.Server(
  start
) where

import Control.Concurrent (newMVar, forkIO)
import Control.Exception (bracket)
import Network (Socket, PortID (PortNumber), listenOn, sClose, accept)
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import Yawn.Configuration (Configuration, port)
import Yawn.Context (Context, makeContext, get, getBlocking, getBytes, close, info, failure)
import Yawn.Dispatcher (dispatchRequest, badRequest)
import Yawn.HTTP.Request
import Yawn.HTTP.Response
import Yawn.HTTP.RequestParser (parse)
import Yawn.Logger (system, trace)
import Yawn.Mime (MimeDictionary)
import Yawn.Util.Maybe (fromIOMaybe, fromIOMaybe_)
import Yawn.Util.Counter (makeCounter)
import qualified Data.ByteString as BS (ByteString, length, append)

start :: Configuration -> MimeDictionary -> IO ()
start conf dict = 
  let run = (listenOn . PortNumber . fromIntegral . port) conf
  in bracket run sClose (startSocket conf dict)

startSocket :: Configuration -> MimeDictionary -> Socket -> IO ()
startSocket conf dict socket = do
  system $ "Listening on port: " ++ (show $ port conf)
  loop conf dict socket

-- TODO: add First parameter. if first then less timeout else http-1.1 std timeout
--       bind socket properly
--       catch exceptions on accept
loop :: Configuration -> MimeDictionary -> Socket -> IO ()
loop conf dict socket = do
  (h, n, p) <- accept socket
  hSetBuffering h NoBuffering
  l <- newMVar ()
  let context = makeContext conf dict l h
  info context $ "Accepted connection from " ++ n ++ ":" ++ show p
  forkIO $ work context 
  loop conf dict socket

work :: Context -> IO ()
work ctx  = work' ctx >> close ctx

-- TODO: only use close for http/1.0 connections and use a timeout for 1.1
work' :: Context -> IO (Maybe (Response))
work' ctx =
  -- getBlockingUntilClosed
  fromIOMaybe_ (get ctx) $ \bs -> do
    fromIOMaybe (badRequest ctx) (parseRequest ctx bs) $ \request -> do 
      trace $ "Parsed request: " ++ show request
      response <- dispatchRequest ctx request
      -- if isKeepAlive request then trace "KEEP-ALIVE..." >> work' ctx
      -- else return response
      return response

parseRequest :: Context -> BS.ByteString -> IO (Maybe (Request))
parseRequest ctx bs = do
  trace $ "Parsing input: " ++ show bs
  counter <- makeCounter 0
  parse (getBlocking ctx counter) bs >>= \p -> case p of 
    Left e -> failure ctx (show e) >> return Nothing 
    Right parsed -> return . Just =<< addBody ctx parsed

addBody :: Context -> (BS.ByteString, Request) -> IO (Request)
addBody ctx (unconsumed, r) = case contentLength r of 
  0 -> return r
  len -> getBytes ctx (len - BS.length unconsumed) >>= \rest -> case rest of
    Nothing -> return $ r { body = unconsumed }
    Just rest' -> do
      let fullBody = unconsumed `BS.append` rest'
      return $ r { body = fullBody }
