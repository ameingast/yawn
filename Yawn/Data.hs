module Yawn.Data where

import Yawn.Util

data Configuration = Configuration { 
  port :: Integer,
  host :: String,
  root :: FilePath,
  defaultIndexFile :: String
} deriving (Show, Eq)

data Response = Response {
  statusCode :: StatusCode,
  entityHeaders :: [ResponseHeader],
  body :: String
} deriving Eq

instance Show Response where
  show (Response sc hs b) = "HTTP/1.1 " ++ show sc ++ 
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
