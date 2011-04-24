module Yawn.Data where

import Yawn.Util

data Configuration = Configuration { 
  port :: Integer,
  host :: String,
  root :: FilePath,
  defaultIndexFile :: String
} deriving (Show, Eq)

data Request = Request {
  method :: RequestMethod,
  uri :: String,
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

data HttpVersion =  HTTP_1_0 | HTTP_1_1 deriving Eq 

instance Show HttpVersion where
  show HTTP_1_0 = "HTTP/1.0"
  show HTTP_1_1 = "HTTP/1.1"

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

data Response = Response {
  statusCode :: StatusCode,
  entityHeaders :: [ResponseHeader],
  body :: String
} deriving Eq

instance Show Response where
  show (Response sc hs b) = show HTTP_1_1 ++ " " ++ show sc ++ 
                            concatWith "\n" hs ++ "\r\n\r\n" ++ b 

data StatusCode = OK |
                  CREATED |
                  ACCEPTED |
                  BAD_REQUEST |
                  UNAUTHORIZED |
                  FORBIDDEN |
                  NOT_FOUND |
                  METHOD_NOT_ALLOWED deriving Eq

instance Show StatusCode where
  show OK = "200 Ok" 
  show CREATED = "201 Created"
  show ACCEPTED = "202 Accepted"
  show BAD_REQUEST = "400 Bad Request"
  show UNAUTHORIZED = "401 Unauthorized"
  show FORBIDDEN = "403 Forbidden"
  show NOT_FOUND = "404 Not Found"
  show METHOD_NOT_ALLOWED = "405 Method not allowed"

data ResponseHeader = CONTENT_TYPE String deriving Eq

instance Show ResponseHeader where
  show (CONTENT_TYPE s) = "Content-type: " ++ s
