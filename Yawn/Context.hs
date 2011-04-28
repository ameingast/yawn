module Yawn.Context (
  Context,
  makeContext,
  configuration,
  get,
  getBlocking,
  getBytes,
  putResponse,
  close,
  debug,
  info,
  error,
  failure,
  mimeTypes
) where

import Control.Concurrent (MVar)
import System.IO (Handle, hClose)
import Yawn.Configuration (Configuration, socketBufSize, requestTimeOut)
import Yawn.HTTP.Response
import Yawn.Logger (Level(LOG_DEBUG, LOG_INFO, LOG_ERROR), doLog)
import Yawn.Mime (MimeDictionary)
import Yawn.Util.Counter (Counter)
import Yawn.Util.Maybe
import qualified Data.ByteString as BS (ByteString)
import qualified Yawn.Network as Network (receive, receiveBlocking, receiveBytes, send)

-- TODO: rename get, put and close
data Context = Ctx {
  configuration :: Configuration,
  mimeTypes     :: MimeDictionary,
  get           :: IO (Maybe (BS.ByteString)),
  getBlocking   :: Counter -> IO (Maybe (BS.ByteString)),
  getBytes      :: Int -> IO (Maybe (BS.ByteString)),
  putResponse   :: Response -> IO (Maybe (Response)),
  close         :: IO (),
  debug         :: String -> IO (),
  info          :: String -> IO (),
  failure       :: String -> IO ()
}

makeContext :: Configuration -> MimeDictionary -> MVar () -> Handle -> Context
makeContext conf dict lock handle = 
  Ctx { configuration = conf,
        mimeTypes     = dict,
        get           = Network.receive handle bufSize,
        getBytes      = Network.receiveBytes handle bufSize timeOut,
        getBlocking   = Network.receiveBlocking handle bufSize timeOut,
        putResponse   = putR,
        close         = log' LOG_DEBUG "Closing connection." >> hClose handle,
        debug         = log' LOG_DEBUG, 
        info          = log' LOG_INFO, 
        failure       = log' LOG_ERROR }
  where 
    timeOut = requestTimeOut conf
    bufSize = socketBufSize conf
    log'    = doLog conf
    putR r  = do
      (response, bytes) <- packResponse conf r
      fromIOMaybe_ (Network.send handle lock bytes) $ \_ -> return $ Just response
