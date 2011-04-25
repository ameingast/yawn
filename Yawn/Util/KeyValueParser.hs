module Yawn.Util.KeyValueParser (
  parsePairs
) where

import Text.ParserCombinators.Parsec

parsePairs :: String -> String -> Either ParseError [(String, String)] 
parsePairs s name = parse file name s

file :: GenParser Char st [(String, String)]
file = keyValuePair `sepEndBy` many1 skipLine

skipLine = comment <|> eol 

comment :: GenParser Char st ()
comment = char '#' >> many (noneOf "\n") >> return ()

keyValuePair :: GenParser Char st (String, String)
keyValuePair = do
  key <- many1 symbol
  char '='
  value <- many1 symbol 
  return (key, value)

valueFor :: String -> GenParser Char st String
valueFor s = string (s ++ "=") >> many (noneOf "\n") >>= \r -> eol >> return r

eol :: GenParser Char st ()
eol = (char '\n' >> return ()) <|> (char '\r' >> return ())

symbol :: GenParser Char st Char
symbol = letter <|> digit <|> char '.' <|> char ':'
