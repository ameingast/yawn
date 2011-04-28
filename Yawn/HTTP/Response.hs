module Yawn.HTTP.Response where

import Data.Maybe (fromMaybe)
import Yawn.Configuration (Configuration, serverName, serverVersion)
import Yawn.Util.List (concatWith)
import qualified Data.ByteString as BS (ByteString, empty, length, append)
import qualified Data.ByteString.Char8 as BS8 (pack)
import qualified Yawn.Util.Time as Time (getAscDate)

data Response = Response {
  statusCode :: StatusCode,
  entityHeaders :: [ResponseHeader],
  responseBody :: Maybe (BS.ByteString)
} deriving Eq

instance Show Response where
  show (Response sc hs _) = "HTTP/1.1 " ++ show sc ++ concatWith "\n" hs ++ "\r\n\r\n" 

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
                    | SERVER_NAME String 
                    | LAST_MODIFIED String
                    | CONNECTION String deriving Eq

instance Show ResponseHeader where
  show (CONTENT_TYPE s)   = "Content-Type: " ++ s
  show (CONTENT_LENGTH l) = "Content-Length: " ++ show l
  show (RESPONSE_DATE s)  = "Date: " ++ s
  show (SERVER_NAME s)    = "Server: " ++ s
  show (LAST_MODIFIED s)  = "Last-Modified: " ++ s
  show (CONNECTION s)     = "Connection: " ++ s

packResponse :: Configuration -> Response -> IO ((Response, BS.ByteString))
packResponse conf r = do
  response <- addMetaInfo conf r
  let bytes = (BS8.pack $ show response) `BS.append` (fromMaybe BS.empty $ responseBody r)
  return (response, bytes)

addMetaInfo :: Configuration -> Response -> IO (Response)
addMetaInfo conf = 
  addTimeStamp . (addServer conf . addContentLength)

addContentLength :: Response -> Response
addContentLength r = 
  let content_length = BS.length $ fromMaybe BS.empty $ responseBody r 
  in addHeader r $ CONTENT_LENGTH content_length

addServer :: Configuration -> Response -> Response
addServer conf r = 
  let server = serverName conf ++ "/" ++ serverVersion conf
  in addHeader r $ SERVER_NAME server 

addTimeStamp :: Response -> IO (Response)
addTimeStamp r =
  Time.getAscDate >>= return . addHeader r . RESPONSE_DATE

addHeader :: Response -> ResponseHeader -> Response
addHeader r@(Response _ headers _) h = 
  r { entityHeaders = h:headers }
