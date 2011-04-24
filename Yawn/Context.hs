module Yawn.Context(
  Context,
  makeContext,
  configuration,
  get,
  put,
  putBin,
  close
) where

import Data.ByteString (ByteString, hPut)
import Control.Concurrent (MVar, withMVar)
import System.IO (Handle, hGetLine, hPutStr, hClose, hWaitForInput, hGetChar)
import System.IO.Error (try)

import Yawn.Data
import qualified Yawn.Logger as Log

data Context = Context {
  configuration :: Configuration,
  get :: IO (String),
  put :: String -> IO (),
  putBin :: ByteString -> IO (),
  close :: IO ()
}

makeContext :: Configuration -> MVar () -> Handle -> Context
makeContext c l h = Context c 
                    (contextGet c l h) 
                    (contextPut l h)
                    (contextPutBin l h) 
                    (hClose h) 

contextPut :: MVar () -> Handle -> String -> IO ()
contextPut l h s = let put' = logErrorFor $ hPutStr h s
                   in withMVar l (\a -> put' >> return a)

-- FIXME: hGetLine is exploitable with long lines of input
contextGet :: Configuration -> MVar () -> Handle -> IO (String)
contextGet c l h = try (hGetLine h) >>= \input -> case input of 
  Left e -> Log.err e >> return ""
  Right i -> if i == "\r" then getMessageBody h >>= return . ("\r\n"++)
  else contextGet c l h >>= \r -> return $ i ++ r

-- FIXME: replace the timeout with a more reasonable number
getMessageBody :: Handle -> IO (String)
getMessageBody h = do
  hasBody <- hWaitForInput h 10
  if not hasBody then return ""
  else do
    try (hGetChar h) >>= \input -> case input of 
      Left _ -> return "" 
      Right c -> getMessageBody h >>= return . (c:)

contextPutBin :: MVar () -> Handle -> ByteString -> IO ()
contextPutBin l h bs = let put' = logErrorFor $ hPut h bs
                       in withMVar l (\a -> put' >> return a)  

logErrorFor :: IO () -> IO ()
logErrorFor f = try f >>= \output -> case output of
  Left e -> Log.err e >> return ()
  Right _ok -> return ()