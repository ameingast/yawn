module Yawn.HTTP.Response where

import Data.Maybe (fromMaybe)
import Yawn.Util.List (concatWith)
import qualified Data.ByteString as BS (ByteString, empty, length, append, null)
import qualified Data.ByteString.Char8 as BS8 (pack)

data Response = Response {
  statusCode :: StatusCode,
  entityHeaders :: [ResponseHeader],
  responseBody :: Maybe (BS.ByteString)
} deriving Eq

instance Show Response where
  show (Response sc hs body) = "HTTP/1.1 " ++ show sc ++ 
                                concatWith "\n" hs ++ "\r\n\r\n<BINARY>" 

data StatusCode = OK |
                  CREATED |
                  ACCEPTED |
                  BAD_REQUEST |
                  UNAUTHORIZED |
                  FORBIDDEN |
                  NOT_FOUND |
                  METHOD_NOT_ALLOWED |
                  INTERNAL_ERROR deriving Eq

instance Show StatusCode where
  show OK = "200 Ok" 
  show CREATED = "201 Created"
  show ACCEPTED = "202 Accepted"
  show BAD_REQUEST = "400 Bad Request"
  show UNAUTHORIZED = "401 Unauthorized"
  show FORBIDDEN = "403 Forbidden"
  show NOT_FOUND = "404 Not Found"
  show METHOD_NOT_ALLOWED = "405 Method not allowed"
  show INTERNAL_ERROR = "500 Internal error"

data ResponseHeader = CONTENT_TYPE String 
                    | CONTENT_LENGTH Int deriving Eq

instance Show ResponseHeader where
  show (CONTENT_TYPE s) = "Content-type: " ++ s
  show (CONTENT_LENGTH l) = "Content-length: " ++ show l

packResponse :: Response -> BS.ByteString
packResponse (Response sc hs b) = 
  if BS.null body then header 
  else header `BS.append` body 
  where header = BS8.pack $ "HTTP/1.1" ++ show sc ++ concatWith "\r\n" headers ++ "\r\n\r\n"
        body = fromMaybe BS.empty b
        headers = CONTENT_LENGTH (BS.length body) : hs
