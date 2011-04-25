module Yawn.Util.DictionaryParser (
  Dictionary,
  parseDictionary
) where

import Text.ParserCombinators.Parsec

type Dictionary = [(String, String)]

parseDictionary :: String -> String -> Either ParseError Dictionary 
parseDictionary = parse (many blank >> keyValuePair `sepEndBy` many1 blank)

keyValuePair :: CharParser () (String, String)
keyValuePair = do
  key <- many1 symbol
  many horizontalSpace >> char '=' >> many horizontalSpace
  value <- many1 symbol 
  return (key, value)

blank :: CharParser () ()
blank = comment <|> eol 

comment :: CharParser () ()
comment = char '#' >> many (noneOf "\n\r") >> return ()

eol :: CharParser () ()
eol = (char '\n' <|> char '\r') >> return ()

symbol :: CharParser () Char
symbol = letter <|> digit <|> char '.' <|> char ':'

horizontalSpace :: CharParser () Char
horizontalSpace = char ' ' <|> char '\t'
