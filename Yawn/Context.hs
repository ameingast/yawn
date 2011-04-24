module Yawn.Context(
  Context,
  makeContext,
  configuration,
  get,
  put,
  putBin,
  close
) where

import Data.ByteString
import Control.Concurrent
import System.IO
import System.IO.Error

import Yawn.Data
import Yawn.Logger as Log

data Context = Context {
  configuration :: Configuration,
  get :: IO (String),
  put :: String -> IO (),
  putBin :: ByteString -> IO (),
  close :: IO ()
}

instance Show Context where
  show = show . configuration

makeContext :: Configuration -> MVar () -> Handle -> Context
makeContext c l h = Context c 
                    (contextGet c l h) 
                    (contextPut l h)
                    (contextPutBin l h) 
                    (hClose h) 

contextPut :: MVar () -> Handle -> String -> IO ()
contextPut l h s = withMVar l (\a -> System.IO.hPutStr h s >> return a)

contextGet :: Configuration -> MVar () -> Handle -> IO (String)
contextGet c l h = do
  tryInput <- System.IO.Error.try (System.IO.hGetLine h)
  case tryInput of
    Left e -> Log.err e >> return ""
    Right i -> if i == "\r" then return "\r\n"
    else contextGet c l h >>= \r -> return $ i ++ r

contextPutBin :: MVar () -> Handle -> ByteString -> IO ()
contextPutBin l h bs = withMVar l (\a -> Data.ByteString.hPut h bs >> return a)
