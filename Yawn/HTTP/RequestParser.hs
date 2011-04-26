module Yawn.HTTP.RequestParser (
  parseRequest
) where

import Text.ParserCombinators.Parsec
import Network.URL (URL, importURL)
import Yawn.HTTP.Request
import qualified Data.Map as M (fromList)
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Char8 as BS8 (unpack)

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

parseHeaders :: CharParser () [(String, String)]
parseHeaders = try parseHeader `manyTill` string "\r\n" 

parseHeader :: CharParser () (String, String)
parseHeader = do
  name <- many (noneOf ":")
  char ':' >> many1 space
  value <- anyChar `manyTill` string "\r\n"
  return (name, value)

parseRequestBody :: CharParser () String
parseRequestBody = anyChar `manyTill` eof

request :: CharParser () Request
request = do
  requestMethod <- parseRequestMethod
  space
  requestUrl <- parseRequestUrl
  httpVersion <- parseHttpVersion
  string "\r\n"
  requestHeaders <- parseHeaders >>= return . M.fromList
  messageBody <- parseRequestBody
  return $ Request requestMethod requestUrl httpVersion requestHeaders messageBody

parseRequest :: BS.ByteString -> Either ParseError Request
parseRequest input = parse request "Parse error" $ BS8.unpack input
