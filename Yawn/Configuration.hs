module Yawn.Configuration where

data Configuration = Configuration { 
  port :: Integer,
  host :: String,
  root :: FilePath,
  defaultIndexFile :: String
} deriving (Show, Eq)
