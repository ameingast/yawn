module Yawn.Application (
  start
) where

import System.Exit (ExitCode(ExitFailure, ExitSuccess), exitWith)
import Yawn.Logger (system)
import Yawn.Util.Maybe
import qualified Yawn.Configuration as Configuration (loadConfig)
import qualified Yawn.Mime as Mime (loadMimeTypes)
import qualified Yawn.Server as Server (start)

start :: FilePath -> IO ()
start path = do 
  system "Booting YAWS..." 
  exitStatus <- fromIOMaybe_ (Configuration.loadConfig path) $ \config -> do
    system $ "Loaded configuration: " ++ show config
    fromIOMaybe_ (Mime.loadMimeTypes config) $ \dict -> do
      system $ "Loaded " ++ show (length dict) ++ " mimetypes"
      Server.start config dict >> return Nothing
  case exitStatus of
    Nothing -> exitWith (ExitFailure 1)
    Just () -> exitWith ExitSuccess