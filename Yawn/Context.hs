module Yawn.Context (
  Context,
  makeContext,
  configuration,
  get,
  getBlocking,
  getBytes,
  put,
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
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Yawn.Network as Network (receive, receiveBlocking, receiveBytes, send)

-- TODO: rename get, put and close
data Context = Ctx {
  configuration :: Configuration,
  mimeTypes     :: MimeDictionary,
  get           :: IO (Maybe (BS.ByteString)),
  getBlocking   :: Counter -> IO (Maybe (BS.ByteString)),
  getBytes      :: Int -> IO (Maybe (BS.ByteString)),
  put           :: BS.ByteString -> IO (Maybe ()),
  putResponse   :: Response -> IO (Maybe ()),
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
        put           = put',
        putResponse   = put' . BS8.pack . show,
        close         = hClose handle,
        debug         = log' LOG_DEBUG, 
        info          = log' LOG_INFO, 
        failure       = log' LOG_ERROR }
  where 
    timeOut = requestTimeOut conf
    bufSize = socketBufSize conf
    put'    = Network.send handle lock
    log'    = doLog conf
