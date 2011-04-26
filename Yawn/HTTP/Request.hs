module Yawn.HTTP.Request where

import Network.URL (URL, importParams, url_params, url_path)
import qualified Data.Map as M (Map, lookup)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (unpack)

data Request = Request {
  method :: RequestMethod,
  url :: URL,
  version :: HttpVersion,
  headers :: M.Map BS.ByteString BS.ByteString,
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

findHeader :: BS.ByteString -> Request -> Maybe BS.ByteString
findHeader name = M.lookup name . headers
