module Yawn.HTTP.RequestParser (
  parse
) where

import Control.Applicative hiding (many)
import Data.Word (Word8)
import Network.URL (URL, importURL)
import Yawn.HTTP.Request
import qualified Data.Attoparsec as P
import qualified Data.Attoparsec.Char8 as P8 (char8, endOfLine, isEndOfLine, isHorizontalSpace)
import qualified Data.ByteString as BS (ByteString, concat)
import qualified Data.ByteString.Char8 as BS8 (unpack, pack)
import qualified Data.Map as M (fromList)

parse :: (IO (BS.ByteString)) -> BS.ByteString -> IO (Either String Request)
parse supplier = parseMore supplier . P.parse parseRequest

parseMore :: (IO (BS.ByteString)) -> P.Result Request -> IO (Either String Request)
parseMore supplier result = do
  case result of
    P.Fail bs ctx err -> return $ Left $ show ctx ++ ": " ++ err ++ ". Unparsed: " ++ show bs
    P.Partial f -> supplier >>= \next -> parseMore supplier $ f next
    P.Done _bs r -> return $ Right r

parseRequest :: P.Parser (Request)
parseRequest = do
  aRequestMethod <- parseHttpMethod <* P8.char8 ' '
  aUrl <- parseURL <* P8.char8 ' '
  aVersion <- parseHttpVersion <* P8.endOfLine
  someHeaders <- (P.many parseHeader >>= return . M.fromList) <* P8.endOfLine
  aBody <- parseBody
  return $ Request aRequestMethod aUrl aVersion someHeaders aBody

parseURL :: P.Parser (URL)
parseURL =
  P.takeWhile1 (/= 32) >>= \u -> case (importURL . BS8.unpack) u of
    Nothing -> fail "Invalid URL"
    Just u' -> return u'

parseHttpMethod :: P.Parser (RequestMethod)
parseHttpMethod = do
  (packString "GET" >> return GET) <|>
    (packString "DELETE" >> return DELETE) <|>
    (packString "HEAD" >> return HEAD) <|>
    (packString "OPTIONS" >> return OPTIONS) <|>
    (packString "CONNECT" >> return CONNECT) <|>
    (packString "TRACE" >> return TRACE) <|>
    (P.try (packString "POST") >> return POST) <|>
    (packString "PUT" >> return PUT)

parseHttpVersion :: P.Parser (HttpVersion)
parseHttpVersion = do
  packString "HTTP/1."
  (P8.char8 '0' >> return HTTP_1_0) <|> (P8.char8 '1' >> return HTTP_1_1)

parseHeader :: P.Parser (BS.ByteString, BS.ByteString)
parseHeader = do
  name <- P.takeWhile1 letterOrDigit <* P8.char8 ':' 
  P.skipWhile P8.isHorizontalSpace
  value <- P.takeTill P8.isEndOfLine <* P8.endOfLine
  additionalValues <- P.many parseHeaderBody
  return (name, BS.concat $ value:additionalValues)

parseHeaderBody :: P.Parser (BS.ByteString)
parseHeaderBody = do 
  P.takeWhile1 P8.isHorizontalSpace
  P.takeTill P8.isEndOfLine <* P8.endOfLine

parseBody :: P.Parser (BS.ByteString)
parseBody = P.takeWhile symbol <* P.endOfInput

letterOrDigit :: Word8 -> Bool
letterOrDigit w = w <= 127 && P.notInClass tokens w

symbol :: Word8 -> Bool
symbol w = w <= 127 || P.inClass tokens w 

tokens :: String
tokens = "\0-\31()<>@,;:\\\"/[]?={} \t"

packString :: String -> P.Parser (BS.ByteString)
packString = P.string . BS8.pack
