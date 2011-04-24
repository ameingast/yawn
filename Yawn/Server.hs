module Yawn.Server(
  start
) where

import Control.Exception
import Control.Concurrent
import Data.ByteString
import Network
import System.IO
import System.IO.Error

import Yawn.Context
import Yawn.Data
import Yawn.Logger as Log
import Yawn.Parser as Parser
import Yawn.Request
import Yawn.Util 

start :: Configuration -> IO ()
start c = let run = (listenOn . PortNumber . fromIntegral . port) c 
          in bracket run sClose (startSocket c)

startSocket :: Configuration -> Socket -> IO ()
startSocket conf socket = do
  Log.debug $ "Listening on port: " ++ (show $ port conf)
  lock <- newMVar ()
  loop socket conf lock

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
  let path = determinePath ctx (uri r)
  Log.debug $ "Delivering resource: " ++ path
  deliverResource ctx path 

determinePath :: Context -> String -> String
determinePath ctx u = (root $ configuration $ ctx) ++ path
  where path = if u == "/" then "/" ++ (defaultIndexFile $ configuration ctx) else u

deliverResource :: Context -> FilePath -> IO ()
deliverResource ctx path = 
  -- this failsafe sucks ass, but it prevents requests from breaking out of the root
  if Prelude.elem ".." (Yawn.Util.split (=='/') path) then 
    fileNotFound ctx
  -- determine content type using mime
  else if endsWith path ["jpg","png"] then deliverImage ctx path
       else deliverTextFile ctx path

deliverTextFile :: Context -> FilePath -> IO ()
deliverTextFile ctx path = do
  tryContent <- System.IO.Error.try (System.IO.readFile path)
  case tryContent of
    Left _e -> fileNotFound ctx
    Right content -> put ctx $ show $ Response OK [CONTENT_TYPE "text/html"] content

deliverImage :: Context -> FilePath -> IO ()
deliverImage ctx path = do
  tryImg <- System.IO.Error.try (Data.ByteString.readFile path)
  case tryImg of
    Left _e -> fileNotFound ctx
    Right content -> do
      put ctx $ show $ Response OK [CONTENT_TYPE "image/jpeg"] []
      putBin ctx content

fileNotFound :: Context -> IO ()
fileNotFound ctx = dispatchError ctx NOT_FOUND ""
