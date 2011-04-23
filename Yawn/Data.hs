module Yawn.Data where

data Configuration = Configuration { 
  port :: Integer,
  host :: String,
  root :: FilePath
} deriving (Show, Eq)

data Context = Context {
  configuration :: Configuration,
  get :: IO (String),
  put :: String -> IO (),
  close :: IO ()
}

instance Show Context where
  show = show . configuration

instance Eq Context where
  a == b = configuration a == configuration b

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
                    HTTP_1_1 deriving Eq 

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
  body :: String
} deriving Eq

instance Show Response where
  show (Response sc b) = "HTTP/1.1" ++ show sc ++ "\r\n\r\n" ++ b 

data StatusCode = 
  -- 200
  OK |
  -- 201
  CREATED |
  -- 202
  ACCEPTED |
  -- 400
  BAD_REQUEST |
  -- 401
  UNAUTHORIZED |
  -- 403
  FORBIDDEN |
  -- 404
  NOT_FOUND |
  -- 405
  METHOD_NOT_ALLOWED deriving Eq

instance Show StatusCode where
  show sc = let n = show $ statusCodeToNum sc in case sc of
    OK -> n ++ " Ok"
    CREATED -> n ++ " Created"
    ACCEPTED -> n ++ " Accepted"
    BAD_REQUEST -> n ++ " Bad Request"
    UNAUTHORIZED -> n ++ " Unauthorized"
    FORBIDDEN -> n ++ " Forbidden"
    NOT_FOUND -> n ++ " Not Found"
    METHOD_NOT_ALLOWED -> n ++ "Method not allowed"

statusCodeToNum :: StatusCode -> Int
statusCodeToNum sc = case sc of
  OK -> 200
  CREATED -> 201
  ACCEPTED -> 202
  BAD_REQUEST -> 400
  UNAUTHORIZED -> 401
  FORBIDDEN -> 403
  NOT_FOUND -> 404
  METHOD_NOT_ALLOWED -> 405
