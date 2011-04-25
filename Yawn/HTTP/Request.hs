module Yawn.HTTP.Request where

import Network.URL (URL, importParams, url_params, url_path)

data Request = Request {
  method :: RequestMethod,
  url :: URL,
  version :: HttpVersion,
  headers :: [RequestHeader],
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

data RequestHeader =  ACCEPT String |
                      ACCEPT_CHARSET String|
                      ACCEPT_ENCODING String |
                      ACCEPT_LANGUAGE String |
                      AUTHORIZATION String |
                      EXPECT String |
                      FROM String |
                      HOST String |
                      IF_MATCH String |
                      IF_MODIFIED_SINCE String |
                      IF_NONE_MATCH String |
                      IF_RANGE String |
                      IF_UNMODIFIED_SINCE String |
                      MAX_FORWARDS String |
                      PROXY_AUTHORIZATION String |
                      RANGE String |
                      REFERER String |
                      TE String |
                      CONNECTION String |
                      USER_AGENT |
                      UNSUPPORTED deriving (Show, Eq)

data HttpVersion =  HTTP_1_0 | HTTP_1_1 deriving Eq 

instance Show HttpVersion where
  show HTTP_1_0 = "HTTP/1.0"
  show HTTP_1_1 = "HTTP/1.1"

getParams :: Request -> [(String, String)]
getParams = url_params . url

postParams :: Request -> Maybe [(String, String)]
postParams = importParams . body

requestPath :: Request -> String
requestPath = url_path . url
