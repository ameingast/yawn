module Yawn.Server(
  run
) where

import Control.Exception
import Control.Concurrent
import Network
import System.IO
import System.IO.Error
import Yawn.Data
import Yawn.Logger as Log
import Yawn.Parser as Parser
import Yawn.Util 

run :: Configuration -> IO ()
run c = let p = (listenOn . PortNumber . fromIntegral . port) c 
        in bracket p sClose (\s -> startSocket s c)

makeContext :: Configuration -> MVar () -> Handle -> Context
makeContext c l h = Context c get' put' close'
  where put' s = do
                  Log.debug $ "Responding: " ++ s
                  withMVar l (\a -> hPutStr h s >> return a)
        close' = hClose h
        get' = do 
          tryInput <- System.IO.Error.try (hGetLine h)
          case tryInput of
            Left e -> Log.err e >> return ""
            Right i -> if i == "\r" then return "\r\n"
                       else get' >>= \r -> return $ i ++ r

startSocket :: Socket -> Configuration -> IO ()
startSocket socket conf = do
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
    Left e  -> dispatchError ctx BAD_REQUEST "" 
    Right r -> (Log.debug $ "Parsed: " ++ show r) >> dispatchRequest ctx r
  close ctx

dispatchError :: Show a => Context -> StatusCode -> a -> IO ()
dispatchError ctx sc e = Log.err e >> (put ctx $ show $ Response sc [] $ show e)

dispatchRequest :: Context -> Request -> IO ()
dispatchRequest ctx r = case method r of
                          GET -> getResource ctx r
                          _ -> dispatchError ctx METHOD_NOT_ALLOWED ""

getResource :: Context -> Request -> IO ()
getResource ctx r = do
  let path = determinePath ctx (uri r)
  Log.debug $ "Delivering resource: " ++ path
  deliverResource ctx path 

determinePath :: Context -> RequestUri -> String
determinePath ctx (RequestUri u) = (root $ configuration $ ctx) ++ path
  where path = if u == "/" then "/index.html" else u

deliverResource :: Context -> FilePath -> IO ()
deliverResource ctx path = 
  -- this failsafe sucks ass, but it prevents requests from breaking out of the root
  if elem ".." (split (=='/') path) then 
    fileNotFound ctx
  else do
    -- replace readFile with getLine so the handle can be flushed after N lines
    tryContent <- System.IO.Error.try (readFile path)
    case tryContent of
      Left _e -> fileNotFound ctx
      -- determine content type
      Right content -> put ctx $ show $ Response OK [CONTENT_TYPE "text/html"] content

fileNotFound :: Context -> IO ()
fileNotFound ctx = dispatchError ctx NOT_FOUND ""
