{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
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

import qualified Data.Char        as Char
import           Data.Maybe       (fromMaybe, listToMaybe)
import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Text.Lens

import           Text.Parsec
import           Text.Parsec.Text

import           Numeric          (readDec, readHex, readInt, readOct)

import           Cow.ParseTree

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
           | String Text
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

instance Show Token where show = show . _value

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
-- This might not be entirely complete with obscure but valid Unicode
-- characters like zero-width joiners.
idChar :: Parser Char
idChar = alphaNum <|> oneOf "$_"

-- | Parses a keyword, based on @reserved@ from @Text.Parse.Token@.
keyword :: Text -> Parser Token
keyword keyword = tokenize $ Keyword keyword <$ parseKeyword
  where parseKeyword =
          do literal keyword
             notFollowedBy idChar <?> ("end of " <> Text.unpack keyword)

-- | Parses a valid name which can start with a letter/$/_ followed by
-- any number of letters/numbers/$/_.
identifier :: Parser Name
identifier = Name <$> (Text.cons <$> startChar <*> rest) <?> "identifier"
  where startChar = letter <|> oneOf "$_"
        rest = Text.pack <$> many idChar

-- | Parses JavaScript string literals, including both normal escapes
-- and Unicode (both '\uXXXX' and '\u{XXXXXX}' formats).
stringLiteral :: Parser Token
stringLiteral = tokenize $ String <$> contents
  where contents = do start <- oneOf "'\""
                      contents <- many $ strChar start
                      char start <?> "end of string"
                      return $ Text.pack contents
        strChar start = satisfy (`notElem` [start, '\\']) <|> escape <?> "string character"

        escape = char '\\' *> (escapeChar <|> unicodeEscape)
        escapeChar = escaped <$> oneOf "0btnvfr'\"\\\n" <?> "escape character"
        escaped c = let Just res = lookup c escapes in res
        escapes = [('b', '\b'), ('t', '\t'), ('n', '\n'), ('v', '\v'), ('0', '\0'),
                   ('f', '\f'), ('r', '\r'), ('\'', '\''), ('\\', '\\')]

        -- parses \uXXXX and \u{X} … \u{XXXXXX} as Unicode characters
        unicodeEscape = char 'u' *> (fourDigit <|> other) <?> "unicode escape sequence"
          where toChar = Char.chr . read . ("0x" ++)
                fourDigit = toChar <$> count 4 hexDigit
                other = toChar <$> between (char '{') (char '}') (many1 hexDigit)

                -- TODO: Handle malformed literals like 0923, 0b1233, -0x1.2
number :: Parser Token
number = tokenize $ Num <$> (try intLit <|> floatLit)
  where -- an integer literal *not* in decimal (ie 0x10 but not 10)
        intLit = do sign   <- (negate <$ try (char '-') <|> id <$ optional (char '+'))
                    format <- parseFormat
                    num    <- many1 digit
                    return . sign . toFormat format $ num
        parseFormat = choice $ try . string <$> ["0x", "0X", "0b", "0B", "0"]
        toFormat format = fromMaybe read $ toReader <$> lookup format formats
        toReader f = fst . head . f
        formats = [("0", readOct), ("0x", readHex), ("0X", readHex),
                   ("0b", readBinary), ("0B", readBinary)]
        readBinary = readInt 2 (`elem` ['0','1']) (read . (:[]))

        floatLit = do sign <- (negate <$ try (char '-') <|> id <$ optional (char '+'))
                      num  <- try $ many1 digit
                      dec  <- optionMaybe $ char '.' *> many1 digit
                      exp  <- optionMaybe $ oneOf "eE" *> many1 digit
                      let dec' = fromMaybe "" $ ("." ++) <$> dec
                          exp' = fromMaybe "" $ ("e" ++) <$> exp
                      return . sign . read $ num <> dec' <> exp'
