module Yawn.Network (
  receive,
  send
) where

import Control.Concurrent (MVar, withMVar)
import System.IO (Handle)
import System.IO.Error (try)
import Yawn.Logger (Level(LOG_ERROR), doLog')
import qualified Data.ByteString as BS (ByteString, hPut, hGetNonBlocking, null)

receive :: Handle -> Int -> IO (Maybe BS.ByteString)
receive h bufsize = do
  tryIO (BS.hGetNonBlocking h bufsize) >>= \r -> case r of
    Nothing -> return Nothing
    Just d -> if BS.null d then return Nothing else return $ Just d

send :: Handle -> MVar () -> BS.ByteString -> IO (Maybe ())
send h l bs = withMVar l (\_ -> tryIO $ BS.hPut h bs)

tryIO :: IO (a) -> IO (Maybe a)
tryIO f = try f >>= \o -> case o of
  Left e -> doLog' LOG_ERROR (show e) >> return Nothing
  Right ok -> return $ Just ok
