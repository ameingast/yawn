module Yawn.Configuration (
  Configuration,
  port,
  host,
  defaultIndexFile,
  requestTimeOut,
  keepAliveTimeOut,
  socketBufSize,
  maxClients,
  showIndex,
  defaultMimeType,
  serverName,
  serverVersion,
  mimeFile,
  logRoot,
  publicRoot,
  loadConfig
) where

import System.IO (hPutStrLn, stderr)
import Yawn.Util.DictionaryParser (Dictionary, parseDictionary)
import Yawn.Util.List (find)
import qualified System.IO.Error as IOError (try)

data Configuration = Configuration { 
  port              :: Integer,
  host              :: String,
  root              :: FilePath,
  defaultIndexFile  :: String,
  requestTimeOut    :: Int,
  keepAliveTimeOut  :: Int,
  socketBufSize     :: Int,
  maxClients        :: Int,
  showIndex         :: Bool,
  defaultMimeType   :: String,
  serverName        :: String,
  serverVersion     :: String
} deriving (Show, Eq)

mimeFile :: Configuration -> FilePath
mimeFile conf = root conf ++ "/conf/mime.types"

logRoot :: Configuration -> FilePath
logRoot conf = root conf ++ "/log/"

publicRoot :: Configuration -> FilePath
publicRoot conf = root conf ++ "/public/"

loadConfig :: FilePath -> IO (Maybe Configuration)
loadConfig appRoot = IOError.try (readFile (appRoot ++ "/conf/yawn.conf")) >>= \d -> case d of
  Left e -> printError e >> return Nothing
  Right content -> parseConfig content appRoot
    
parseConfig :: String -> String -> IO (Maybe Configuration)
parseConfig content appRoot = parseConfiguration appRoot content >>= \c -> case c of 
  Nothing -> return Nothing
  Just config -> return $ Just config

parseConfiguration :: FilePath -> String -> IO (Maybe Configuration)
parseConfiguration appRoot s = case parseDictionary "yawn.conf" s of
  Left e -> printError e >> return Nothing
  Right pairs -> return $ Just $ makeConfig appRoot pairs

makeConfig :: FilePath -> Dictionary -> Configuration
makeConfig appRoot xs = Configuration (read $ find "port" "9000" xs)
                                      (find "host" "localhost" xs)
                                      appRoot
                                      (find "defaultIndexFile" "index.html" xs)
                                      (read $ find "requestTimeout" "300" xs)
                                      (read $ find "keepAliveTimeout" "15" xs)
                                      (read $ find "socketBufSize" "2048" xs)
                                      (read $ find "maxClients" "100" xs)
                                      (read $ find "showIndex" "True" xs)
                                      (find "defaultMimeType" "text/html" xs)
                                      (find "serverName" "YAWN" xs)
                                      (find "serverVersion" "0.1" xs)

printError :: Show a => a -> IO ()
printError e = hPutStrLn stderr $ "Unable to load configuration file " ++ show e
