module Yawn.Server(
  run
) where

import Control.Exception
import Control.Concurrent
import Network
import System.IO
import Yawn.Data
import Yawn.Logger as Log
import Yawn.Parser as Parser

run :: Configuration -> IO ()
run c = let p = (listenOn . PortNumber . fromIntegral . port) c 
        in bracket p sClose (\s -> startSocket s c)

makeContext :: Configuration -> MVar () -> Handle -> Context
makeContext c l h = Context c get' put' close'
  where put' s = do
                  Log.debug $ "Responding: " ++ s
                  withMVar l (\a -> hPutStr h s >> return a)
        close' = hClose h
        -- TODO: handle hGetLine exceptions
        get' = hGetLine h >>= \i -> if i == "\r" then return "\r\n"
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
    Left e  -> Log.err e >> dispatchError ctx 400 e
    Right r -> (Log.debug $ "Parsed: " ++ show r) >> dispatchRequest ctx r
  close ctx

-- add fancy error message
dispatchError :: Show a => Context -> Int -> a -> IO ()
dispatchError ctx code e = put ctx $ show $ Response BAD_REQUEST (show e)

dispatchRequest :: Context -> Request -> IO ()
dispatchRequest ctx r = case method r of
                          GET -> getResource ctx r
                          _ -> Log.err $ "Unsupported request" ++ show r

getResource :: Context -> Request -> IO ()
getResource ctx r = readFile "Main.hs" >>= \f -> put ctx $ show $ Response OK f
