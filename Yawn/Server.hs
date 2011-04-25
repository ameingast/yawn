module Yawn.Server(
  start
) where

import Control.Exception (bracket)
import Control.Concurrent (MVar, newMVar, forkIO)
import Network
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import System.IO.Error(try)
import qualified Data.ByteString as BS

import Yawn.Context
import Yawn.Data
import Yawn.Request
import Yawn.Mime
import qualified Yawn.Logger as Log
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
  Log.debug $ "Listening on port: " ++ (show $ port conf)
  lock <- newMVar ()
  loop conf dict socket lock

-- TODO: add First parameter. if first then less timeout else http-1.1 std timeout
loop :: Configuration -> MimeDictionary -> Socket -> MVar () -> IO ()
loop conf dict socket l = do
  (h, n, p) <- accept socket
  Log.info $ "Accepted connection from " ++ n ++ ":" ++ show p
  hSetBuffering h NoBuffering
  forkIO $ work $ makeContext conf dict l h
  loop conf dict socket l

-- TODO: only use close for http/1.0 connections and use a timeout for 1.1
work :: Context -> IO ()
work ctx  = do
  i <- get ctx
  Log.info $ "Received: " ++ i
  case Parser.parseRequest i of
    Left _e  -> dispatchError ctx BAD_REQUEST 
    Right r -> (Log.debug $ "Parsed: " ++ show r) >> dispatchRequest ctx r
  close ctx

dispatchError :: Context -> StatusCode -> IO ()
dispatchError ctx sc = Log.err sc >> (put ctx $ show $ Response sc [] "")

dispatchRequest :: Context -> Request -> IO ()
dispatchRequest ctx r = do
  Log.debug $ "Dispatching request: " ++ (uri r) ++ ", GET: " ++ 
              show (getParams r) ++ ", POST: " ++ show (postParams r)
  case determinePath ctx r of
    Nothing -> dispatchError ctx NOT_FOUND
    Just path -> (Log.debug $ "Serving: " ++ path) >> deliverResource ctx path

determinePath :: Context -> Request -> Maybe (String)
determinePath ctx u = let r = (root . configuration) ctx ++ "/public"
                      in requestPath u>>= \p -> return $ r ++ "/" ++ addIdx ctx p

addIdx :: Context -> String -> String
addIdx ctx p = let idxFile = defaultIndexFile $ configuration ctx
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
