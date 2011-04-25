module Yawn.Configuration (
  Configuration,
  port,
  host,
  defaultIndexFile,
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
  defaultIndexFile :: String
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
  anIndexFile <- find "indexFile" xs
  return $ Configuration (read aPort) aHost "" anIndexFile

find :: Eq a => a -> [(a, b)] -> Maybe b
find _ [] = Nothing
find s ((a,b):xs) = if s == a then Just b else find s xs

printError :: Show a => a -> IO ()
printError e = hPutStrLn stderr $ "Unable to load configuration file " ++ show e
