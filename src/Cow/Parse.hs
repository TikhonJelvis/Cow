module Cow.Parse (parseFile) where

import Data.Functor ((<$>))

import Text.ParserCombinators.Parsec (ParseError, SourceName)

import Cow.Type

type ParseResult a = Either ParseError [AST a]
type Parser a = SourceName -> String -> ParseResult a

parseFile :: Parser a -> String -> IO (ParseResult a)
parseFile parse file = parse file <$> readFile file
