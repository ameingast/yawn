module Yawn.Parser(
  parseRequest
) where

import Text.ParserCombinators.Parsec
import Network.URL (URL, importURL)
import Yawn.Request

eol :: GenParser Char st Char 
eol = (char '\r') <|> (char '\n')

parseRequestMethod :: GenParser Char st RequestMethod
parseRequestMethod = tryMethod GET <|> tryMethod PUT <|> tryMethod POST <|> 
                     tryMethod DELETE <|> tryMethod HEAD <|> tryMethod OPTIONS <|>
                     tryMethod CONNECT <|> tryMethod TRACE

tryMethod :: RequestMethod -> GenParser Char st RequestMethod
tryMethod name = try $ string (show name) >> return name

parseRequestUrl :: GenParser Char st URL
parseRequestUrl = do
  (noneOf " ") `manyTill` space >>= \u -> case importURL u of
    Nothing -> fail "Invalid URL"
    Just u' -> return u'

parseHttpVersion :: GenParser Char st HttpVersion
parseHttpVersion = do
  string "HTTP/1."
  (char '0' >> return HTTP_1_0) <|> (char '1' >> return HTTP_1_1)

parseHeaders :: GenParser Char st [RequestHeader]
parseHeaders = (compatibleHeaders <|> unsupportedHeader) `manyTill` string "\r\n" 

tryHeader :: String -> (String -> RequestHeader) -> GenParser Char st RequestHeader
tryHeader name constr = do
  try $ string name
  char ':'
  spaces
  value <- (noneOf "\r") `manyTill` char '\r' 
  return $ constr value

unsupportedHeader :: GenParser Char st RequestHeader
unsupportedHeader = (noneOf "\r") `manyTill` eol >> return UNSUPPORTED

compatibleHeaders :: GenParser Char st RequestHeader
compatibleHeaders = (tryHeader "Connection" CONNECTION) <|> 
                    (tryHeader "Host" HOST)

parseRequestBody :: GenParser Char st String
parseRequestBody = anyChar `manyTill` eof

request :: GenParser Char st Request
request = do
  requestMethod <- parseRequestMethod
  space
  requestUrl <- parseRequestUrl
  httpVersion <- parseHttpVersion
  eol
  requestHeaders <- parseHeaders >>= return . filter (UNSUPPORTED /=)
  messageBody <- parseRequestBody
  return $ Request requestMethod requestUrl httpVersion requestHeaders messageBody

parseRequest :: String -> Either ParseError Request
parseRequest input = parse request "Parse error" input
