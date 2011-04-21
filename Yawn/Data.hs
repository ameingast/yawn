module Yawn.Data where

data Configuration = Configuration { 
  portNumber :: Integer,
  host :: String
} deriving Show
