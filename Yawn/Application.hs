module Yawn.Application (
  start
) where

import Yawn.Configuration (Configuration, loadConfig)
import Yawn.Logger (system)
import Yawn.Util.Maybe (liftIOMaybe_)
import qualified Yawn.Mime as Mime (loadMimeTypes)
import qualified Yawn.Server as Server (start)

start :: FilePath -> IO ()
start path = system "Booting YAWS..." >> loadConfiguration path

loadConfiguration :: FilePath -> IO ()
loadConfiguration = liftIOMaybe_ hop . loadConfig
  where hop c = system ("Loaded configuration: " ++ show c) >> loadMimeTypes c

loadMimeTypes :: Configuration -> IO ()
loadMimeTypes conf = liftIOMaybe_ hop $ Mime.loadMimeTypes conf
  where hop dict = system "Loaded mimetypes" >> Server.start conf dict 
