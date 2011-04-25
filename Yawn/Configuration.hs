module Yawn.Configuration where

import Text.ParserCombinators.Parsec
import System.IO (hPutStrLn, stderr)
import qualified System.IO.Error as IOError

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
parseConfiguration s = case parse file "yawn.conf" s of
  Left e -> printError e >> return Nothing 
  Right ok -> return $ Just ok

printError :: Show a => a -> IO ()
printError e = hPutStrLn stderr $ "Unable to load configuration file " ++ show e

file :: GenParser Char st Configuration
file = do
  aPort <- valueFor "port"
  aHost <- valueFor "host"
  aDefaultIndexFile <- valueFor "indexFile"
  return $ Configuration (read aPort) aHost "" aDefaultIndexFile

valueFor :: String -> GenParser Char st String
valueFor s = string (s ++ "=") >> many (noneOf "\n") >>= \r -> eol >> return r

eol :: GenParser Char st Char
eol = char '\n' <|> char '\r' <|> (eof >> return '\n')
