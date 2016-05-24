{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
-- | A basic JavaScript parser that produces a ParseTree with Token
-- values as leaves. The Token values keep track of whitespace
-- consumed during lexing which can be accessed to accurately print
-- the parsed input but does not factor into equality comparisons or
-- other operations.
--
-- The parser has not been deeply tested so do not expect 100%
-- conformance to actual JavaScript syntax.
module Cow.Language.JavaScript where

import           Control.Lens

import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Text.Lens

import           Text.Parsec
import           Text.Parsec.Text

-- * Tokens

-- | The name of an identifier or object key, kept abstract to
-- differentiate from strings. (Not 100% sure about this design
-- decision.)
newtype Name = Name { _name :: Text } deriving Eq

makeLenses ''Name

instance Show Name where show n = n ^. name . _Text

-- | The kinds of tokens we ultimately parse.
--
-- Note that this is not pure lexing data: differentiating between
-- some of these requires actual parsing.
data Value = Var Name
           | Num Double
           | Str Text
           | Regex Text         -- TODO: better type for regex?

             -- TODO: Exauhstive types for operators and keywords?
           | Keyword Text
           | Operator Text

           | Semicolon
           | LineEnd

             -- array literals ([1,f(2),"foo",[1]])
           | ArrayStart
           | ArrayEnd
           | ArraySep

             -- argument lists
           | ArgStart
           | ArgEnd
           | ArgSep

             -- object literals
           | ObjStart
           | ObjEnd
           | ObjKey Name
           | ObjSep
           | ObjColon -- TODO: better name?

             -- loop conditions (ie the (..) in for(..) and while(..)
           | LoopStart
           | LoopEnd
           | ForLoopSep

             -- blocks: function bodies and control flow
           | BlockStart
           | BlockEnd

             -- comments
           | LineComment Text   -- // ..
           | BlockComment Text  -- /* .. */
           deriving (Show, Eq)

makePrisms ''Value

-- | A single JavaScript token that preserves the whitespace consumed
-- in parsing it.
data Token = Token { _whitespace :: Text
                   -- ^ The whitespace *before* this token.
                   , _value      :: Value
                   -- ^ The semantic role of this token (ie list
                   -- separator, object key) with any relevant content
                   -- (ie the identifier itself).
                   }

makeLenses ''Token

instance Eq Token where
  t1 == t2 = t1 ^. value == t2 ^. value

-- * Parsing functions:

-- | Converts a parser that doesn't care about whitespace into one
-- that consumes and saves the whitespace *before* the token.
tokenize :: Parser Value -> Parser Token
tokenize value = Token <$> (Text.pack <$> many space) <*> value

-- | Parse a literal string of characters (ie a keyword) into a 'Text'
-- value.
literal :: Text -> Parser Text
literal str = Text.pack <$> (string $ Text.unpack str)


-- | The end of a line of code: a semicolon or a newline.
terminator :: Parser Token
terminator = tokenize $  Semicolon <$ char ';'
                     <|> LineEnd   <$ char '\n'

-- | All the legal JavaScript binary operators, ordered by precedence.
operators :: [[Text]]
operators = [["."], ["*", "/", "%"], ["+", "-"],
             [">>", ">>>", "<<"], ["<", "<=", ">", ">=", "in", "instanceof"],
             ["==", "===", "!=", "!=="], ["&"], ["^"], ["|"], ["&&"], ["||"],
             ["=", "*=", "/=", "%=", "+=", "-=", ">>=", "<<=", "&=", "|=", "^="]]


-- | All the legal JavaScript unary operators.
unaryOperators :: [[Text]]
unaryOperators = [["new"], ["++", "--"], ["+", "-", "~", "!", "typeof", "void", "delete"]]


-- | All the reserved words in JavaScript which aren't unary/binary
-- operators.
keywords :: [Text]
keywords = ["break", "catch", "const", "continue", "do", "export",
            "for", "function", "if", "import", "label",
            "let", "return", "switch", "throw", "try",
            "var", "while", "with", "yield"]

-- | A character that makes up a valid JavaScript identifier.
--
-- This might not be entirely complete with more obscure valid Unicode
-- characters like zero-width joiners.
idChar :: Parser Char
idChar = alphaNum <|> oneOf "$_"

-- | Parses a keyword, based on @reserved@ from @Text.Parse.Token@.
keyword :: Text -> Parser Token
keyword keyword = tokenize $ Keyword keyword <$ parseKeyword
  where parseKeyword =
          do literal keyword
             notFollowedBy idChar <?> ("end of " <> Text.unpack keyword)
