module Yawn.Context(
  Context,
  makeContext,
  configuration,
  get,
  put,
  putResponse,
  close,
  mimeTypes
) where

import Control.Concurrent (MVar)
import System.IO (Handle, hClose)
import Yawn.Configuration (Configuration, socketBufSize)
import Yawn.HTTP.Response
import Yawn.Mime (MimeDictionary)
import Yawn.Network (receive, send)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (pack)

-- TODO: rename get, put and close
data Context = Ctx {
  configuration :: Configuration,
  mimeTypes     :: MimeDictionary,
  get           :: IO (Maybe (BS.ByteString)),
  put           :: BS.ByteString -> IO (Maybe ()),
  putResponse   :: Response -> IO (Maybe ()),
  close         :: IO ()
}

makeContext :: Configuration -> MimeDictionary -> MVar () -> Handle -> Context
makeContext c d l h = 
  let put' = send h l
  in Ctx c d (receive h $ socketBufSize c) put' (put' . BS8.pack . show) (hClose h)
