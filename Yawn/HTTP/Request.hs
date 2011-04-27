module Yawn.HTTP.Request where

import Network.URL (URL, importParams, url_params, url_path)
import qualified Data.Map as M (Map, lookup)

data Request = Request {
  method :: RequestMethod,
  url :: URL,
  version :: HttpVersion,
  headers :: M.Map String String,
  body :: String
} deriving (Show, Eq)

data RequestMethod =  GET |
                      PUT |
                      POST |
                      DELETE |
                      HEAD |
                      OPTIONS |
                      CONNECT |
                      TRACE deriving (Show, Eq)

data HttpVersion = HTTP_1_0 | HTTP_1_1 deriving Eq 

instance Show HttpVersion where
  show HTTP_1_0 = "HTTP/1.0"
  show HTTP_1_1 = "HTTP/1.1"

getParams :: Request -> [(String, String)]
getParams = url_params . url

postParams :: Request -> Maybe [(String, String)]
postParams = importParams . body

requestPath :: Request -> String
requestPath = url_path . url

findHeader :: String -> Request -> Maybe String 
findHeader name = M.lookup name . headers

contentLength :: Request -> Int
contentLength r = case findHeader "Content-Length" r of
  Nothing -> 0 
  Just s -> read s

-- Under HTTP/1.0 all connections are closed unless Connection: Keep-Alive is supplied
-- Under HTTP/1.1 all connections are open unless Connection: close is supplied
isKeepAlive :: Request -> Bool
isKeepAlive r = case findHeader "Connection" r of 
  Nothing -> version r == HTTP_1_1
  Just con -> if version r == HTTP_1_0 then "Keep-Alive" == con
              else "close" /= con
