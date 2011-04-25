module Yawn.Mime (
  MimeDictionary,
  loadMimeTypes,
  mimeType
) where

import System.FilePath (takeExtension) 
import Text.ParserCombinators.Parsec
import Yawn.Configuration (Configuration, root)
import qualified System.IO.Error as IOError 
import qualified Yawn.Logger as Log

type MimeDictionary = [(String, [String])]

dictLocation :: Configuration -> String
dictLocation conf = root conf ++ "/conf/mime.types"

loadMimeTypes :: Configuration -> IO (Maybe MimeDictionary)
loadMimeTypes conf = do
  IOError.try (readFile $ dictLocation conf) >>= \c -> case c of
    Left e -> Log.err e >> return Nothing
    Right content -> case parseMime content of
      Left e -> Log.err e >> return Nothing
      Right ok -> return $ Just ok

mimeType :: MimeDictionary -> FilePath -> Maybe (String)
mimeType d p = let ext = tail $ takeExtension p
               in case filter (\(_, exts) -> elem ext exts) d of
                 [] -> Nothing
                 ((a,_):_) -> Just a

parseMime :: String -> Either ParseError [(String, [String])]
parseMime s = parse file "Mime Type Error" s 

file :: GenParser Char st [(String, [String])]
file = do
  many (comments <|> string "\n")
  many line

comments :: GenParser Char st String
comments = char '#' >> anyChar `manyTill` char '\n'

line :: GenParser Char st (String, [String])
line = do
  name <- many1 symbol
  exts <- extensions <|> noExtensions
  return (name, exts)

extensions :: GenParser Char st [String]
extensions = do
  many whiteSpace
  exts <- many1 symbol `sepBy` many1 whiteSpace
  eol
  return exts

noExtensions :: GenParser Char st [String]
noExtensions = eol >> return []

whiteSpace :: GenParser Char st Char
whiteSpace = char ' ' <|> char '\t'

eol :: GenParser Char st Char
eol = char '\n' <|> (eof >> return '\n')

symbol :: GenParser Char st Char
symbol = letter <|> digit <|> char '-' <|> char '+' <|> char '.' <|> char '/'
