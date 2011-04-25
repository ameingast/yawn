module Yawn.Configuration where

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
