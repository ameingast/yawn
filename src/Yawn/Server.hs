module Yawn.Server(
  start
) where

import Control.Concurrent (newMVar, forkIO, QSem, newQSem, waitQSem, signalQSem)
import Control.Exception (bracket)
import Network (Socket, sClose, accept)
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import Yawn.Configuration (Configuration, port, maxClients, host)
import Yawn.Context (Context, makeContext, get, getBlocking, getBytes, close, info, failure)
import Yawn.Dispatcher (dispatchRequest, badRequest)
import Yawn.HTTP.Request
import Yawn.HTTP.RequestParser (parse)
import Yawn.HTTP.Response
import Yawn.Logger (trace)
import Yawn.Mime (MimeDictionary)
import Yawn.Util.Counter (makeCounter)
import Yawn.Network (bindServer)
import Yawn.Util.Maybe (fromIOMaybe, fromIOMaybe_)
import qualified Data.ByteString as BS (ByteString, length, append)

start :: Configuration -> MimeDictionary -> IO ()
start conf dict = do
  let bindPort = fromIntegral $ port conf
  clientsCounter <- newQSem (maxClients conf)
  bracket (bindServer bindPort (host conf)) sClose (loop conf dict clientsCounter)

-- TODO: add First parameter. if first then less timeout else http-1.1 std timeout
loop :: Configuration -> MimeDictionary -> QSem -> Socket -> IO ()
loop conf dict counter socket = do
  waitQSem counter
  (h, n, p) <- Network.accept socket
  hSetBuffering h NoBuffering
  l <- newMVar ()
  let context = makeContext conf dict l h
  info context $ "Accepted connection from " ++ n ++ ":" ++ show p
  forkIO $ work context counter
  loop conf dict counter socket

-- catch exceptions on work just in case, so the maxClients lock gets shut down properly
work :: Context -> QSem -> IO ()
work ctx counter = do
  work' ctx
  close ctx
  signalQSem counter
  return ()

-- (get ctx) is wrong here. it doesn not block and only works on the first request
-- a lazy bytestring socket would probably fix it
work' :: Context -> IO (Maybe (Response))
work' ctx =
  fromIOMaybe_ (get ctx) $ \bs -> do
    fromIOMaybe (badRequest ctx) (parseRequest ctx bs) $ \request -> do 
      trace $ "Parsed request: " ++ show request
      response <- dispatchRequest ctx request
      --if isKeepAlive request then trace "KEEP-ALIVE..." >> work' ctx
      --else return response
      return response

parseRequest :: Context -> BS.ByteString -> IO (Maybe (Request))
parseRequest ctx bs = do
  trace $ "Parsing input: " ++ show bs
  counter <- makeCounter 0
  parse (getBlocking ctx counter) bs >>= \p -> case p of 
    Left e -> failure ctx (show e) >> return Nothing 
    Right parsed -> return . Just =<< addBody ctx parsed

addBody :: Context -> (BS.ByteString, Request) -> IO (Request)
addBody ctx (unconsumed, r) = 
  case contentLength r of 
    0 -> return r
    len -> getBytes ctx (len - BS.length unconsumed) >>= \rest -> case rest of
      Nothing -> return $ r { body = unconsumed }
      Just rest' -> do
        let fullBody = unconsumed `BS.append` rest'
        return $ r { body = fullBody }
