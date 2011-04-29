module Yawn.HTTP.Request where

import Data.Maybe (fromMaybe)
import Network.URL (URL, importParams, url_params, url_path)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (unpack)
import qualified Data.Map as M (Map, lookup)

data Request = Request {
  method :: RequestMethod,
  url :: URL,
  version :: HttpVersion,
  headers :: M.Map String String,
  body :: BS.ByteString
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
postParams = importParams . BS8.unpack . body

requestPath :: Request -> String
requestPath = url_path . url

findHeader :: String -> Request -> Maybe String 
findHeader name = M.lookup name . headers

hasHeader :: String -> Request -> Bool
hasHeader s r = findHeader s r /= Nothing

-- catch read exceptions
contentLength :: Request -> Int
contentLength = read . fromMaybe "0" . findHeader "Content-Length"

-- Under HTTP/1.0 all connections are closed unless Connection: Keep-Alive is supplied
-- Under HTTP/1.1 all connections are open unless Connection: close is supplied
isKeepAlive :: Request -> Bool
isKeepAlive r = case findHeader "Connection" r of 
  Nothing -> version r == HTTP_1_1
  Just con -> if version r == HTTP_1_0 then "Keep-Alive" == con
              else "close" /= con
