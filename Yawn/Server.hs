module Yawn.Server(
  start
) where

import Control.Concurrent (MVar, newMVar, forkIO)
import Control.Exception (bracket)
import Network (Socket, PortID (PortNumber), listenOn, sClose, accept)
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import System.IO.Error (try)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import Yawn.Configuration (Configuration, port, publicRoot, defaultIndexFile, showIndex)
import Yawn.Context (Context, makeContext, configuration, get, close, put, putBin, mimeTypes)
import Yawn.HTTP.Request
import Yawn.HTTP.RequestParser (parseRequest)
import Yawn.HTTP.Response
import Yawn.Logger (Level (LOG_DEBUG, LOG_INFO, LOG_ERROR), doLog)
import Yawn.Mime (MimeDictionary, mimeType)
import Yawn.Util.List (split)
import qualified Data.ByteString as BS (readFile)

start :: Configuration -> MimeDictionary -> IO ()
start conf dict = let run = (listenOn . PortNumber . fromIntegral . port) conf
                  in bracket run sClose (startSocket conf dict)

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
  case parseRequest i of
    Left _e -> badRequest ctx
    Right r -> (doLog conf LOG_DEBUG $ "Parsed: " ++ show r) >> dispatchRequest ctx r
  -- if keep-alive && <= keep-alive timeout listen for more input
  close ctx

dispatchError :: Context -> StatusCode -> IO ()
dispatchError ctx sc = do
  doLog (configuration ctx) LOG_ERROR sc
  put ctx $ show $ Response sc [] ""

-- extract into Dispatcher.hs
dispatchRequest :: Context -> Request -> IO ()
dispatchRequest ctx r = do
  let conf = configuration ctx
  let path = determinePath ctx r
  let idxFile = addIdx ctx path
  idxFileExists <- doesFileExist idxFile
  pathDirectoryExists <- doesDirectoryExist path
  doLog conf LOG_DEBUG $ "Dispatching request: " ++ (show $ url r) ++ ", GET: " ++ 
                         show (getParams r) ++ ", POST: " ++ show (postParams r)
  if showIndex conf && pathDirectoryExists && idxFileExists then deliverResource ctx idxFile
  else if showIndex conf && pathDirectoryExists then deliverIndex ctx path
       else deliverResource ctx path

determinePath :: Context -> Request -> String
determinePath ctx r = let base = (publicRoot . configuration) ctx
                      in base ++ requestPath r

addIdx :: Context -> String -> String
addIdx ctx p = let idxFile = (defaultIndexFile . configuration) ctx
               in if last p == '/' then p ++ idxFile else p

-- TODO
deliverIndex :: Context -> FilePath -> IO ()
deliverIndex ctx path = do
  let conf = configuration ctx
  doLog conf LOG_DEBUG $ "Serving index: " ++ path
  try (getDirectoryContents path) >>= \c -> case c of
    Left e -> (doLog conf LOG_ERROR $ "Unable to create index " ++ show e) >> badRequest ctx
    Right list -> put ctx $ show $ Response OK [CONTENT_TYPE "text/html"] (show list)

deliverResource :: Context -> FilePath -> IO ()
deliverResource ctx path = do
  doLog (configuration ctx) LOG_DEBUG $ "Serving file: " ++ path
  -- FIXME: move this failsafe up so it also catches directory listings
  if elem ".." (split (=='/') path) then fileNotFound ctx
  else deliverFile ctx path

deliverFile :: Context -> FilePath -> IO ()
deliverFile ctx path = do
  try (BS.readFile path) >>= \f -> case f of
    Left _e -> fileNotFound ctx
    Right content -> do 
      put ctx $ show $ Response OK [CONTENT_TYPE (contentType ctx path)] []
      putBin ctx content

badRequest :: Context -> IO ()
badRequest ctx = dispatchError ctx BAD_REQUEST

fileNotFound :: Context -> IO ()
fileNotFound ctx = dispatchError ctx NOT_FOUND

contentType :: Context -> FilePath -> String
contentType ctx path = case mimeType (mimeTypes ctx) path of
  Nothing -> ""
  Just ct -> ct
