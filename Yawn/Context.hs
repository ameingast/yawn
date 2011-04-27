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
  mimeTypes
) where

import Control.Concurrent (MVar)
import System.IO (Handle, hClose)
import Yawn.Configuration (Configuration, socketBufSize, requestTimeOut)
import Yawn.HTTP.Response
import Yawn.Util.Counter (Counter)
import Yawn.Mime (MimeDictionary)
import qualified Yawn.Network as Network (receive, receiveBlocking, receiveBytes, send)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack)

-- TODO: rename get, put and close
--       add logger method here so i can get rid of all the > let conf = configuration ctx < bloat
data Context = Ctx {
  configuration :: Configuration,
  mimeTypes     :: MimeDictionary,
  get           :: IO (Maybe (BS.ByteString)),
  getBlocking   :: Counter -> IO (Maybe (BS.ByteString)),
  getBytes      :: Int -> IO (Maybe (BS.ByteString)),
  put           :: BS.ByteString -> IO (Maybe ()),
  putResponse   :: Response -> IO (Maybe ()),
  close         :: IO ()
}

makeContext :: Configuration -> MimeDictionary -> MVar () -> Handle -> Context
makeContext c d l h = 
  Ctx { configuration = c,
        mimeTypes = d,
        get = Network.receive h bufSize,
        getBytes = Network.receiveBytes h bufSize timeOut,
        getBlocking = Network.receiveBlocking h bufSize timeOut,
        put = put',
        putResponse = put' . BS8.pack . show,
        close = hClose h }
  where timeOut = requestTimeOut c
        bufSize = socketBufSize c
        put' = Network.send h l
