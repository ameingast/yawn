module Yawn.Data where

data Configuration = Configuration { 
  portNumber :: Integer,
  host :: String
} deriving (Show, Eq)

data Request = Request {
  method :: RequestMethod,
  uri :: RequestUri,
  version :: HttpVersion,
  headers :: [RequestHeader]
} deriving (Show, Eq)

data RequestMethod =  GET |
                      PUT |
                      POST |
                      DELETE |
                      HEAD |
                      OPTIONS |
                      CONNECT |
                      TRACE deriving (Show, Eq)

data RequestUri = STAR | 
                  ABSOLUTE_URI String | 
                  ABSOLUTE_PATH String | 
                  AUTHORITY deriving (Show, Eq)

data HttpVersion =  HTTP_1_0 | 
                    HTTP_1_1 deriving (Show, Eq)

data RequestHeader =  ACCEPT |
                      ACCEPT_CHARSET |
                      ACCEPT_ENCODING |
                      ACCEPT_LANGUAGE |
                      AUTHORIZATION |
                      EXPECT |
                      FROM |
                      HOST |
                      IF_MATCH |
                      IF_MODIFIED_SINCE |
                      IF_NONE_MATCH |
                      IF_RANGE |
                      IF_UNMODIFIED_SINCE |
                      MAX_FORWARDS |
                      PROXY_AUTHORIZATION |
                      RANGE |
                      REFERER |
                      TE |
                      USER_AGENT deriving (Show, Eq)
