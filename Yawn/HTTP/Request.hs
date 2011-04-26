module Yawn.HTTP.Request where

import Network.URL (URL, importParams, url_params, url_path)
import qualified Data.Map as M

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
