module Cow.Language.JavaScript (Value) where

import Control.Applicative ((<$>), (<*), (*>), (<*>), liftA2)

import Text.ParserCombinators.Parsec

type Tag  = Int
type Name = String

data Value = Var      (Maybe Tag) Name
           | Num      Double
           | Str      String
           | Regex    String
           | Keyword  String
           | Operator String
           | Array
           | Object
           | Loop 
           | Init -- The bit between parentheses in loops, if statements and so on...
           | Block deriving (Show, Eq)

keywords :: [String]
keywords = ["if", "else", "for", "while", "do", "in", "function", "void", "delete", "var",
            "instanceof", "typeof", "return", "this"]

keyword :: Parser Value
keyword = Keyword <$> (choice $ string <$> keywords) <* spaces

var :: Parser Value
var = Var Nothing <$> name 
  where name = liftA2 (:) (letter <|> char '_') $ many (letter <|> digit <|> char '_')
        
num :: Parser Value
num = Num . read <$> decimal
  where decimal = (++) <$> many1 digit <*> fractional
        fractional = ("." ++) <$> ((char '.' *> many1 digit) <|> return "0")

specChar :: CharParser st Char
specChar = spec <$> (oneOf "\"\\nt'" <?> "valid escape character (\", n, t, \\, or ')")
  where spec character = case character of
          'n'  -> '\n'
          't'  -> '\t'
          c    ->  c -- All other characters are just themselves when escaped.

str :: Parser Value
str = do opener   <- oneOf "\"'"
         contents <- many $ (char '\\' *> specChar) <|> char opener
         char opener <?> "end of string"
         return $ Str contents
         
regex :: Parser Value -- TODO: handle regexes with '/' characters inside (e.g. /\//).
regex = Regex <$> (char '/' *> many1 (noneOf "/") <* char '/')