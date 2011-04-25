module Yawn.HTTP.RequestParser (
  parseRequest
) where

import Text.ParserCombinators.Parsec
import Network.URL (URL, importURL)
import Yawn.HTTP.Request

eol :: CharParser () Char 
eol = (char '\r') <|> (char '\n')

parseRequestMethod :: CharParser () RequestMethod
parseRequestMethod = tryMethod GET <|> tryMethod PUT <|> tryMethod POST <|> 
                     tryMethod DELETE <|> tryMethod HEAD <|> tryMethod OPTIONS <|>
                     tryMethod CONNECT <|> tryMethod TRACE

tryMethod :: RequestMethod -> CharParser () RequestMethod
tryMethod name = try $ string (show name) >> return name

parseRequestUrl :: CharParser () URL
parseRequestUrl = do
  (noneOf " ") `manyTill` space >>= \u -> case importURL u of
    Nothing -> fail "Invalid URL"
    Just u' -> return u'

parseHttpVersion :: CharParser () HttpVersion
parseHttpVersion = do
  string "HTTP/1."
  (char '0' >> return HTTP_1_0) <|> (char '1' >> return HTTP_1_1)

parseHeaders :: CharParser () [RequestHeader]
parseHeaders = (compatibleHeaders <|> unsupportedHeader) `manyTill` string "\r\n" 

parseHeader :: String -> (String -> RequestHeader) -> CharParser () RequestHeader
parseHeader name constr = do
  string name >> char ':' >> many1 space
  value <- (noneOf "\r") `manyTill` char '\r'
  return $ constr value

unsupportedHeader :: CharParser () RequestHeader
unsupportedHeader = (noneOf "\r") `manyTill` eol >> return UNSUPPORTED

compatibleHeaders :: CharParser () RequestHeader
compatibleHeaders = (try $ parseHeader "Accept" ACCEPT) <|>
                    (try $ parseHeader "Accept-Charset" ACCEPT_CHARSET) <|>
                    (try $ parseHeader "Accept-Encoding" ACCEPT_ENCODING) <|>
                    (try $ parseHeader "Accept-Language" ACCEPT_LANGUAGE) <|>
                    (try $ parseHeader "Authorization" AUTHORIZATION) <|>
                    (try $ parseHeader "Expect" EXPECT) <|>
                    (try $ parseHeader "From" FROM) <|>
                    (try $ parseHeader "Host" HOST) <|>
                    (try $ parseHeader "If-Match" IF_MATCH) <|>
                    (try $ parseHeader "If-Modified-Since" IF_MODIFIED_SINCE) <|> 
                    (try $ parseHeader "If-None-Match" IF_NONE_MATCH) <|> 
                    (try $ parseHeader "If-Range" IF_RANGE) <|> 
                    (try $ parseHeader "If-Unmodified-Since" IF_UNMODIFIED_SINCE) <|> 
                    (try $ parseHeader "Max-Forwards" MAX_FORWARDS) <|> 
                    (try $ parseHeader "Proxy-Authorization" PROXY_AUTHORIZATION) <|> 
                    (try $ parseHeader "Range" RANGE) <|> 
                    (try $ parseHeader "Referer" REFERER) <|> 
                    (try $ parseHeader "Connection" CONNECTION) <|> 
                    (try $ parseHeader "User-Agent" USER_AGENT)

parseRequestBody :: CharParser () String
parseRequestBody = anyChar `manyTill` eof

request :: CharParser () Request
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
