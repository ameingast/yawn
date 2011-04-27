module Yawn.Util.DictionaryParser (
  Dictionary,
  parseDictionary
) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad (liftM2)
import Text.ParserCombinators.Parsec

type Dictionary = [(String, String)]

parseDictionary :: String -> String -> Either ParseError Dictionary 
parseDictionary = parse (many blank *> keyValuePair `sepEndBy` many1 blank <* eof)

keyValuePair :: CharParser () (String, String)
keyValuePair =
  liftM2 (,)
    (many1 symbol <* many horizontalSpace >> char '=' >> many horizontalSpace)
    (many1 symbol)

blank :: CharParser () ()
blank = comment <|> eol 

comment :: CharParser () ()
comment = char '#' *> many (noneOf "\n\r") *> return ()

eol :: CharParser () ()
eol = (char '\n' <|> char '\r') *> return ()

symbol :: CharParser () Char
symbol = letter <|> digit <|> char '.' <|> char ':' <|> char '/'

horizontalSpace :: CharParser () Char
horizontalSpace = char ' ' <|> char '\t'
