module Yawn.Parser(
  parseRequest
) where

import Text.ParserCombinators.Parsec
import Yawn.Request

eol :: GenParser Char st Char 
eol = (try $ char '\r') <|> (try $ char '\n') <|> (eof >> return '\n') 

requestMethod :: GenParser Char st RequestMethod
requestMethod = tryMethod GET <|> tryMethod PUT <|> tryMethod POST <|> 
                tryMethod DELETE <|> tryMethod HEAD <|> tryMethod OPTIONS <|>
                tryMethod CONNECT <|> tryMethod TRACE

tryMethod :: RequestMethod -> GenParser Char st RequestMethod
tryMethod name = try $ string (show name) >> return name

requestUri :: GenParser Char st String
requestUri = (noneOf " ") `manyTill` space >>= return 

parseHttpVersion :: GenParser Char st HttpVersion
parseHttpVersion = (try $ string "HTTP/1.0" >> return HTTP_1_0) <|> 
              (try $ string "HTTP/1.1" >> return HTTP_1_1)

tryHeader :: String -> (String -> RequestHeader) -> GenParser Char st RequestHeader
tryHeader name constr = try $ string name >> char ':' >> space >> (noneOf "\r") `manyTill` eol >>= return . constr

skipHeader :: GenParser Char st RequestHeader
skipHeader = (noneOf "\r") `manyTill` eol >> return UNSUPPORTED

compatibleHeaders :: GenParser Char st RequestHeader
compatibleHeaders = (tryHeader "Connection" CONNECTION) <|> 
                    (tryHeader "Host" HOST)

requestHeaders :: GenParser Char st [RequestHeader]
requestHeaders = (compatibleHeaders <|> skipHeader) `manyTill` string "\r\n" 

request :: GenParser Char st Request
request = do
  spaces
  requestmethod <- requestMethod
  spaces
  requesturi <- requestUri
  spaces
  httpversion <- parseHttpVersion
  char '\r'
  allHeaders <- requestHeaders
  let filteredHeaders = filter (UNSUPPORTED /=) allHeaders
  return $ Request requestmethod requesturi httpversion filteredHeaders

parseRequest :: String -> Either ParseError Request
parseRequest input = parse request "Parse error" input
