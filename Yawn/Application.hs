module Yawn.Application (
  start
) where

import Yawn.Configuration (Configuration, loadConfig)
import Yawn.Logger (Level (LOG_DEBUG), doLog)
import Yawn.Util.Maybe (liftIOMaybe_)
import qualified Yawn.Mime as Mime (loadMimeTypes)
import qualified Yawn.Server as Server (start)

start :: FilePath -> IO ()
start path = putStrLn "Booting YAWS..." >> loadConfiguration path

loadConfiguration :: FilePath -> IO ()
loadConfiguration path = liftIOMaybe_ hop $ loadConfig path
  where hop c = (doLog c LOG_DEBUG $ "Loaded configuration: " ++ show c) >> loadMimeTypes c

loadMimeTypes :: Configuration -> IO ()
loadMimeTypes conf = liftIOMaybe_ hop $ Mime.loadMimeTypes conf
  where hop dict = doLog conf LOG_DEBUG "Loaded mimetypes" >> Server.start conf dict 
