module Yawn.Mime (
  MimeDictionary,
  loadMimeTypes,
  mimeType
) where

import System.FilePath (takeExtension) 
import Text.ParserCombinators.Parsec
import Yawn.Configuration (Configuration, mimeFile)
import Yawn.Logger (Level (LOG_ERROR), doLog)
import qualified System.IO.Error as IOError 

type MimeDictionary = [(String, [String])]

loadMimeTypes :: Configuration -> IO (Maybe MimeDictionary)
loadMimeTypes conf = do
  IOError.try (readFile $ mimeFile conf) >>= \c -> case c of
    Left e -> doLog conf LOG_ERROR e >> return Nothing
    Right content -> case parseMime content of
      Left e -> doLog conf LOG_ERROR e >> return Nothing
      Right ok -> return $ Just ok

mimeType :: MimeDictionary -> FilePath -> Maybe (String)
mimeType d p = let ext = tail $ takeExtension p
               in case filter (\(_, exts) -> elem ext exts) d of
                 [] -> Nothing
                 ((a,_):_) -> Just a

parseMime :: String -> Either ParseError [(String, [String])]
parseMime s = parse file "Mime Type Error" s 

file :: CharParser st [(String, [String])]
file = do
  many (comments <|> string "\n")
  many line

comments :: CharParser st String
comments = char '#' >> anyChar `manyTill` char '\n'

line :: CharParser st (String, [String])
line = do
  name <- many1 symbol
  exts <- extensions <|> noExtensions
  return (name, exts)

extensions :: CharParser st [String]
extensions = do
  many whiteSpace
  exts <- many1 symbol `sepBy` many1 whiteSpace
  eol
  return exts

noExtensions :: CharParser st [String]
noExtensions = eol >> return []

whiteSpace :: CharParser st Char
whiteSpace = char ' ' <|> char '\t'

eol :: CharParser st Char
eol = char '\n' <|> (eof >> return '\n')

symbol :: CharParser st Char
symbol = letter <|> digit <|> char '-' <|> char '+' <|> char '.' <|> char '/'
