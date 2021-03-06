module Yawn.HTTP.RequestParser (
  parse
) where

import Control.Applicative
import Control.Monad (liftM2, liftM5)
import Data.Word (Word8)
import Network.URL (URL, importURL)
import Yawn.HTTP.Request
import qualified Data.Attoparsec as P
import qualified Data.Attoparsec.Char8 as P8 (char8, endOfLine, isEndOfLine, isHorizontalSpace)
import qualified Data.ByteString as BS (ByteString, concat, empty)
import qualified Data.ByteString.Char8 as BS8 (unpack, pack)
import qualified Data.Map as M (fromList)

type ParserSupplier = IO (Maybe BS.ByteString)

parse :: ParserSupplier -> BS.ByteString -> IO (Either String (BS.ByteString, Request))
parse supplier = parseMore supplier . P.parse parseRequest

parseMore :: ParserSupplier -> P.Result a -> IO (Either String (BS.ByteString, a))
parseMore supplier result = case result of
  P.Fail bs ctx err -> return $ Left $ show ctx ++ ": " ++ err ++ ". Unparsed: " ++ show bs
  P.Done bs r -> return $ Right (bs, r)
  P.Partial f -> supplier >>= \next' -> case next' of
    Nothing -> parseMore supplier $ f BS.empty
    Just next -> parseMore supplier $ f next

parseRequest :: P.Parser (Request)
parseRequest = 
  liftM5 Request
    (parseHttpMethod <* P8.char8 ' ')
    (parseURL <* P8.char8 ' ')
    (parseHttpVersion <* P8.endOfLine)
    ((many parseHeader >>= return . M.fromList) <* P8.endOfLine)
    (return BS.empty)

parseURL :: P.Parser (URL)
parseURL = 
  P.takeWhile1 (/= 32) >>= \u -> case (importURL . BS8.unpack) u of
    Nothing -> fail "Invalid URL"
    Just u' -> return u'

parseHttpMethod :: P.Parser (RequestMethod)
parseHttpMethod =
  (GET <$ packString "GET")
  <|> (DELETE <$ packString "DELETE")
  <|> (HEAD <$ packString "HEAD")
  <|> (OPTIONS <$ packString "OPTIONS")
  <|> (CONNECT <$ packString "CONNECT")
  <|> (TRACE <$ packString "TRACE")
  <|> (POST <$ P.try (packString "POST"))
  <|> (PUT <$ packString "PUT")

parseHttpVersion :: P.Parser (HttpVersion)
parseHttpVersion = do
  packString "HTTP/1."
  (P8.char8 '0' >> return HTTP_1_0) <|> (P8.char8 '1' >> return HTTP_1_1)

parseHeader :: P.Parser (String, String)
parseHeader = 
  liftM2 (\x y -> (BS8.unpack x, BS8.unpack y))
    (P.takeWhile1 letterOrDigit <* P8.char8 ':' <* P.skipWhile P8.isHorizontalSpace)
    parseHeaderValue

parseHeaderValue :: P.Parser (BS.ByteString)
parseHeaderValue = do
  value <- P.takeTill P8.isEndOfLine <* P8.endOfLine
  many parseHeaderBody >>= return . (value:) >>= return . BS.concat

parseHeaderBody :: P.Parser (BS.ByteString)
parseHeaderBody =
  P.takeWhile1 P8.isHorizontalSpace *> P.takeTill P8.isEndOfLine <* P8.endOfLine

letterOrDigit :: Word8 -> Bool
letterOrDigit w = w <= 127 && P.notInClass tokens w

tokens :: String
tokens = "\0-\31()<>@,;:\\\"/[]?={} \t"

packString :: String -> P.Parser (BS.ByteString)
packString = P.string . BS8.pack
