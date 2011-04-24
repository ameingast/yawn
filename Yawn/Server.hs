module Yawn.Server(
  start
) where

import Control.Exception (bracket)
import Control.Concurrent (MVar, newMVar, forkIO)
import qualified Data.ByteString as BS
import Network
import System.IO (BufferMode (NoBuffering), hSetBuffering)
import System.IO.Error(try)

import Yawn.Context
import Yawn.Data
import qualified Yawn.Logger as Log
import qualified Yawn.Parser as Parser
import Yawn.Request
import qualified Yawn.Util as Util

start :: Configuration -> IO ()
start conf = let run = (listenOn . PortNumber . fromIntegral . port) conf
             in bracket run sClose (startSocket conf)

startSocket :: Configuration -> Socket -> IO ()
startSocket conf socket = do
  Log.debug $ "Listening on port: " ++ (show $ port conf)
  lock <- newMVar ()
  loop socket conf lock

-- TODO: add First parameter. if first then less timeout else http-1.1 std timeout
loop :: Socket -> Configuration -> MVar () -> IO ()
loop socket conf l = do
  (h, n, p) <- accept socket
  Log.info $ "Accepted connection from " ++ n ++ ":" ++ show p
  hSetBuffering h NoBuffering
  forkIO $ work $ makeContext conf l h
  loop socket conf l

work :: Context -> IO ()
work ctx  = do
  i <- get ctx
  Log.info $ "Received: " ++ i
  case Parser.parseRequest i of
    Left _e  -> dispatchError ctx BAD_REQUEST "" 
    Right r -> (Log.debug $ "Parsed: " ++ show r) >> dispatchRequest ctx r
  close ctx

dispatchError :: Show a => Context -> StatusCode -> a -> IO ()
dispatchError ctx sc e = Log.err e >> (put ctx $ show $ Response sc [] $ show e)

dispatchRequest :: Context -> Request -> IO ()
dispatchRequest ctx r = do
  Log.debug $ "Dispatching request: " ++ (uri r) ++ ", GET: " ++ 
              show (getParams r) ++ ", POST: " ++ show (postParams r)
  case determinePath ctx r of
    Nothing -> dispatchError ctx NOT_FOUND ""
    Just path -> (Log.debug $ "Serving: " ++ path) >> deliverResource ctx path

determinePath :: Context -> Request -> Maybe (String)
determinePath ctx u = let r = (root . configuration) ctx
                      in requestPath u>>= \p -> return $ r ++ "/" ++ addIdx ctx p

addIdx :: Context -> String -> String
addIdx ctx p = let idxFile = defaultIndexFile $ configuration ctx
                     in if p == "/" || p == "" then idxFile else p

deliverResource :: Context -> FilePath -> IO ()
deliverResource ctx path = 
  -- FIXME: this failsafe sucks ass
  if elem ".." (Util.split (=='/') path) then fileNotFound ctx
  -- FIXME: determine content type using mime
  else if Util.endsWith path ["jpg","png"] then deliverBinFile ctx path
       else deliverTextFile ctx path

deliverTextFile :: Context -> FilePath -> IO ()
deliverTextFile ctx path = do
  tryContent <- try (readFile path)
  case tryContent of
    Left _e -> fileNotFound ctx
    Right content -> put ctx $ show $ Response OK [CONTENT_TYPE "text/html"] content

deliverBinFile :: Context -> FilePath -> IO ()
deliverBinFile ctx path = do
  tryImg <- try (BS.readFile path)
  case tryImg of
    Left _e -> fileNotFound ctx
    Right content -> do 
      put ctx $ show $ Response OK [CONTENT_TYPE "image/jpeg"] []
      putBin ctx content

fileNotFound :: Context -> IO ()
fileNotFound ctx = dispatchError ctx NOT_FOUND ""
