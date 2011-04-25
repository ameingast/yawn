module Yawn.Application (
  start
) where

import Yawn.Configuration (Configuration, loadConfig)
import Yawn.Mime as Mime (loadMimeTypes)
import Yawn.Logger (doLog, Level (LOG_DEBUG))
import qualified Yawn.Server as Server (start)

start :: FilePath -> IO ()
start path = putStrLn "Booting YAWS..." >> safeLoadConfiguration path

safeLoadConfiguration :: FilePath -> IO ()
safeLoadConfiguration path = loadConfig path >>= \c -> case c of
  Nothing -> return ()
  Just conf -> doLog conf LOG_DEBUG "Loaded configuration" >> safeLoadMimetypes conf
    
safeLoadMimetypes :: Configuration -> IO ()
safeLoadMimetypes conf = loadMimeTypes conf >>= \d -> case d of
  Nothing -> return ()
  Just dict -> doLog conf LOG_DEBUG "Loaded mimetypes" >> Server.start conf dict

