module Yawn.Server(
  start
) where

import Control.Concurrent (MVar, newMVar, forkIO)
import Control.Exception (bracket)
import Network
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import System.IO.Error(try)
import Yawn.Configuration (Configuration, port, publicRoot, defaultIndexFile)
import Yawn.Context
import Yawn.Logger
import Yawn.Mime (MimeDictionary, loadMimeTypes, mimeType)
import Yawn.Request
import Yawn.Response
import qualified Data.ByteString as BS
import qualified Yawn.Parser as Parser
import qualified Yawn.Util as Util

start :: Configuration -> IO ()
start conf = do
  loadMimeTypes conf >>= \d -> case d of
    Nothing -> return ()
    Just dict -> do
      let run = (listenOn . PortNumber . fromIntegral . port) conf
      bracket run sClose (startSocket conf dict)

startSocket :: Configuration -> MimeDictionary -> Socket -> IO ()
startSocket conf dict socket = do
  doLog conf LOG_DEBUG $ "Listening on port: " ++ (show $ port conf)
  lock <- newMVar ()
  loop conf dict socket lock

-- TODO: add First parameter. if first then less timeout else http-1.1 std timeout
loop :: Configuration -> MimeDictionary -> Socket -> MVar () -> IO ()
loop conf dict socket l = do
  (h, n, p) <- accept socket
  doLog conf LOG_INFO $ "Accepted connection from " ++ n ++ ":" ++ show p
  hSetBuffering h NoBuffering
  forkIO $ work $ makeContext conf dict l h
  loop conf dict socket l

-- TODO: only use close for http/1.0 connections and use a timeout for 1.1
work :: Context -> IO ()
work ctx  = do
  let conf = configuration ctx
  i <- get ctx
  doLog conf LOG_INFO $ "Received: " ++ i
  case Parser.parseRequest i of
    Left _e  -> dispatchError ctx BAD_REQUEST 
    Right r -> (doLog conf LOG_DEBUG $ "Parsed: " ++ show r) >> dispatchRequest ctx r
  close ctx

dispatchError :: Context -> StatusCode -> IO ()
dispatchError ctx sc = do
  doLog (configuration ctx) LOG_ERROR sc
  put ctx $ show $ Response sc [] ""

dispatchRequest :: Context -> Request -> IO ()
dispatchRequest ctx r = do
  let conf = configuration ctx
  let path = determinePath ctx r
  doLog conf LOG_DEBUG $ "Dispatching request: " ++ (show $ url r) ++ ", GET: " ++ 
                         show (getParams r) ++ ", POST: " ++ show (postParams r)
  doLog conf LOG_DEBUG $ "Serving: " ++ path
  deliverResource ctx path

determinePath :: Context -> Request -> String
determinePath ctx r = let base = (publicRoot . configuration) ctx
                      in base ++ "/" ++ addIdx ctx (requestPath r)

addIdx :: Context -> String -> String
addIdx ctx p = let idxFile = (defaultIndexFile . configuration) ctx
               in if p == "/" || p == "" then idxFile else p

deliverResource :: Context -> FilePath -> IO ()
deliverResource ctx path = 
  -- FIXME: this failsafe sucks ass
  if elem ".." (Util.split (=='/') path) then fileNotFound ctx
  else deliverFile ctx path

deliverFile :: Context -> FilePath -> IO ()
deliverFile ctx path = do
  try (BS.readFile path) >>= \f -> case f of
    Left _e -> fileNotFound ctx
    Right content -> do 
      put ctx $ show $ Response OK [CONTENT_TYPE (contentType ctx path)] []
      putBin ctx content

fileNotFound :: Context -> IO ()
fileNotFound ctx = dispatchError ctx NOT_FOUND

contentType :: Context -> FilePath -> String
contentType ctx path = case mimeType (mimeTypes ctx) path of
  Nothing -> ""
  Just ct -> ct
