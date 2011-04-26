module Yawn.Configuration (
  Configuration,
  port,
  host,
  defaultIndexFile,
  requestTimeOut,
  keepAliveTimeOut,
  maxClients,
  showIndex,
  mimeFile,
  logRoot,
  publicRoot,
  loadConfig
) where

import System.IO (hPutStrLn, stderr)
import Yawn.Util.DictionaryParser (Dictionary, parseDictionary)
import qualified System.IO.Error as IOError (try)

data Configuration = Configuration { 
  port :: Integer,
  host :: String,
  root :: FilePath,
  defaultIndexFile :: String,
  requestTimeOut :: Int,
  keepAliveTimeOut :: Int,
  maxClients :: Int,
  showIndex :: Bool
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
parseConfig content appRoot = (parseConfiguration content) >>= \c -> case c of 
  Nothing -> return Nothing
  Just config -> return $ Just $ config { root = appRoot }

parseConfiguration :: String -> IO (Maybe Configuration)
parseConfiguration s = case parseDictionary "yawn.conf" s of
  Left e -> printError e >> return Nothing
  Right pairs -> case makeConfig pairs of
    Nothing -> printError "Configuration file incomplete" >> return Nothing
    Just x -> return $ Just x

makeConfig :: Dictionary -> Maybe Configuration
makeConfig xs = do
  aPort <- find "port" xs
  aHost <- find "host" xs
  anIndexFile <- find "defaultIndexFile" xs
  let aRequestTimeout = findOr "requestTimeout" "300" xs
  let aKeepAliveTimeout = findOr "keepAliveTimeout" "15" xs
  let aMaxClients = findOr "maxClients" "100" xs
  let aShowIndex = findOr "showIndex" "False" xs
  return $ Configuration (read aPort) 
                         aHost 
                         "" 
                         anIndexFile 
                         (read aRequestTimeout) 
                         (read aKeepAliveTimeout)
                         (read aMaxClients) 
                         (read aShowIndex)

find :: Eq a => a -> [(a, b)] -> Maybe b
find _ [] = Nothing
find s ((a,b):xs) = if s == a then Just b else find s xs

findOr :: Eq a => a -> b -> [(a, b)] -> b
findOr x y xs = case find x xs of
  Nothing -> y
  Just z -> z

printError :: Show a => a -> IO ()
printError e = hPutStrLn stderr $ "Unable to load configuration file " ++ show e
