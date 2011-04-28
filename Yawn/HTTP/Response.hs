module Yawn.HTTP.Response where

import Data.Maybe (fromMaybe)
import Yawn.Util.List (concatWith)
import Yawn.Util.Time (getAscDate)
import qualified Data.ByteString as BS (ByteString, empty, length, append)
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
                    | CONTENT_LENGTH Int 
                    | RESPONSE_DATE String 
                    | SERVER_NAME String deriving Eq

instance Show ResponseHeader where
  show (CONTENT_TYPE s) = "Content-Type: " ++ s
  show (CONTENT_LENGTH l) = "Content-Length: " ++ show l
  show (RESPONSE_DATE s) = "Date: " ++ s
  show (SERVER_NAME s) = "Server: " ++ s

packResponse :: Response -> IO (BS.ByteString)
packResponse (Response sc hs b) = do
  date <- getAscDate
  let body = fromMaybe BS.empty b
  let headers = RESPONSE_DATE date : (CONTENT_LENGTH (BS.length body) : hs)
  let header = BS8.pack $ "HTTP/1.1 " ++ show sc ++ concatWith "\r\n" headers ++ "\r\n\r\n"
  return $ header `BS.append` body 
