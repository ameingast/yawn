module Yawn.Network (
  receive,
  receiveBlocking,
  receiveBytes,
  send,
  bindServer
) where

import Control.Concurrent (MVar, withMVar, threadDelay)
import Control.Monad (liftM)
import Network.Socket hiding (send)
import System.IO (Handle)
import System.IO.Error (try)
import Yawn.Logger (trace)
import Yawn.Util.Counter (Counter, readCounter, incCounter)
import Yawn.Util.Maybe
import qualified Data.ByteString as BS (ByteString, hPut, hGetNonBlocking, null, length, append)

-- TODO: replace receive* with Socket.ByteString.Lazy
receive :: Handle -> Int -> IO (Maybe (BS.ByteString))
receive h bufsize = do
  fromIOMaybe_ (tryIO (BS.hGetNonBlocking h bufsize)) $ \d -> do 
    if BS.null d then return Nothing 
    else return $ Just d

receiveBlocking :: Handle -> Int -> Int -> Counter -> IO (Maybe (BS.ByteString))
receiveBlocking h bufsize timeout counter = receiveBlocking' h bufsize timeout counter

receiveBlocking' :: Handle -> Int -> Int -> Counter -> IO (Maybe (BS.ByteString))
receiveBlocking' h bufsize timeout cnt = do
  oldCount <- readCounter cnt
  let currentRequestTime = 2 ^ (abs $ oldCount - 1)
  let delay = 2 ^ oldCount - 1
  trace $ "receiveBlocking for: " ++ show delay ++ "ms. Try: " ++ show oldCount
  if timeout >= currentRequestTime * 1000000 then return Nothing 
  else do 
    threadDelay delay
    fromIOMaybe 
      (incCounter cnt >> receiveBlocking' h bufsize timeout cnt)
      (receive h bufsize) $ \x -> do
        trace $ "receiveBlocking: " ++ show x
        return $ Just x

receiveBytes :: Handle -> Int -> Int -> Int -> IO (Maybe BS.ByteString)
receiveBytes h bufsize timeout len = receiveBytes' h bufsize timeout len 0 

receiveBytes' :: Handle -> Int -> Int -> Int -> Int -> IO (Maybe (BS.ByteString))
receiveBytes' h bufsize timeout len tries = do
  let currentRequestTime = 2 ^ (abs $ tries - 1) 
  let delay = 2 ^ tries - 1
  trace $ "receiveBytes " ++ show len ++ " char8s. Blocking " ++ show delay ++ 
          "ms. Try: " ++ show tries
  if len <= 0 || timeout >= currentRequestTime * 1000000 then return Nothing
  else do
    trace $ "Locking Thread for " ++ show delay ++ " ms"
    threadDelay delay
    fromIOMaybe
      (receiveBytes' h bufsize timeout len (tries + 1))
      (receive h bufsize) $ \x -> do
        if BS.length x >= len then return $ Just x
        else do
          trace $ "receiveBytes: " ++ show x ++ "(" ++ (show $ BS.length x) ++ ")"
          next <- receiveBytes' h bufsize timeout (len - BS.length x) 0 
          return $ liftM (BS.append x) next

send :: Handle -> MVar () -> BS.ByteString -> IO (Maybe ())
send h l bs = withMVar l (\_ -> tryIO $ BS.hPut h bs)

tryIO :: IO (a) -> IO (Maybe a)
tryIO f = try f >>= \o -> case o of
  Left e -> trace (show e) >> return Nothing
  Right ok -> return $ Just ok

bindServer :: Int -> String -> IO (Socket)
bindServer bindPort bindHost = do
  sock <- socket AF_INET Stream 0
  addr <- getHostAddr bindPort bindHost
  trace $ "Binding: " ++ show addr
  setSocketOption sock ReuseAddr 1
  bindSocket sock addr
  listen sock 150
  return sock

getHostAddr :: Int -> String -> IO (SockAddr)
getHostAddr p s = do
  h <- if s == "*" then return iNADDR_ANY else inet_addr s
  return $ SockAddrInet (fromIntegral p) h

