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
