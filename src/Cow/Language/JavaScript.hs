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
import           Control.Monad    (join)

import qualified Data.Char        as Char
import           Data.Maybe       (fromMaybe, listToMaybe, maybeToList)
import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as Text
import           Data.Text.Lens

import           Text.Parsec
import qualified Text.Parsec.Expr as Expr
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
data Value = Variable Name
           | Num Double
           | String Text
           | Regex Text         -- TODO: better type for regex?

             -- TODO: Exauhstive types for operators and keywords?
           | Keyword Text
           | Operator Text

           | Semicolon
           | LineEnd

             -- expressions in parentheses like (1 + 2), but *not*
             -- argument lists
           | ParenStart
           | ParenEnd

             -- array literals ([1,f(2),"foo",[1]])
           | ArrayStart
           | ArrayEnd
           | ArraySep

             -- array indexing (the [-] part of a[1])
           | IndexStart -- [
           | IndexEnd   -- ]

             -- argument lists (function definitions *and* calls)
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

-- * Parsing

-- ** Parsing helpers:

-- | Converts a parser that doesn't care about whitespace into one
-- that consumes and saves the whitespace *after* the token.
tokenize :: Parser Value -> Parser Token
tokenize value = do val    <- value
                    spaces <- Text.pack <$> many space
                    return $ Token spaces val

-- | Parses delimited lists like '1,2,3,4,' tokenizing all the pieces
-- (both values *and* delimiters).
--
-- Allows an optional trailing delimiter, as long as there is at least
-- one expression. (So just ',' won't parse!)
tokenizedList :: Parser Token         -- ^ The separator (usually a comma).
              -> Parser (Parse Token) -- ^ The expression between separators.
              -> Parser [Parse Token]
tokenizedList sep exp = do entries <- join <$> many (try entry)
                           final   <- optionMaybe exp
                           return $ entries <> maybeToList final
  where -- a single 'exp' followed by a separator
        entry = (\ a b -> [a, b]) <$> exp <*> (Leaf' <$> sep)

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

-- | Parse a literal string of characters (ie a keyword) into a 'Text'
-- value.
literal :: Text -> Parser Text
literal str = Text.pack <$> (string $ Text.unpack str)


-- ** Language Productions

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

variable :: Parser Token
variable = tokenize $ Variable <$> identifier

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
-- | Parses JavaScript numeric literals including other bases ('0x10')
-- and floating point numbers with exponents ('1.5e10').
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

        -- a float literal: 105, 10.5, 10e5, 10.1e5… etc
        floatLit = do sign <- (negate <$ try (char '-') <|> id <$ optional (char '+'))
                      num  <- try $ many1 digit
                      dec  <- optionMaybe $ char '.' *> many1 digit
                      exp  <- optionMaybe $ oneOf "eE" *> many1 digit
                      let dec' = fromMaybe "" $ ("." ++) <$> dec
                          exp' = fromMaybe "" $ ("e" ++) <$> exp
                      return . sign . read $ num <> dec' <> exp'

-- | The index part of an expression indexing into an array: ie the
-- '[f(x)]' from 'g(y)[f(x)]'.
indexExpression :: Parser (Parse Token)
indexExpression = do open  <- Leaf' <$> (tokenize $ IndexStart <$ literal "[")
                     index <- expression
                     close <- Leaf' <$> (tokenize $ IndexEnd <$ literal "]")
                     return $ Node' [open, index, close]

-- | The arguments of a function call expression: ie the '(1,2,3)' of
-- 'f(1,2,3)'.
callExpression :: Parser (Parse Token)
callExpression = do open  <- Leaf' <$> (tokenize $ ArgStart <$ literal "(")
                    args  <- tokenizedList (tokenize $ ArgSep <$ literal ",") expression
                    close <- Leaf' <$> (tokenize $ ArgEnd <$ literal ")")
                    return . Node' $ (open : args) ++ [close]

-- | An expression with a mix of function calls and array indexing
-- like 'f(1)[2](3)'.
--
-- Separated out to deal with left-recursion.
callsAndIndices :: Parser (Parse Token)
callsAndIndices = do base  <- simpleAtom
                     calls <- many1 (callExpression <|> indexExpression)
                     return . Node' $ base : calls

-- | A self-contained piece of a JavaScript expression.
--
-- This is split from @atom@ to avoid left-recursion.
simpleAtom :: Parser (Parse Token)
simpleAtom =  Leaf' <$> stringLiteral
          <|> Leaf' <$> try number
          <|> Leaf' <$> variable
          <|> parens
  where parens = do open  <- tokenize $ ParenStart <$ literal "("
                    exp   <- expression
                    close <- tokenize $ ParenEnd <$ literal ")"
                    return $ Node' [Leaf' open, exp, Leaf' close]

-- | Any JavaScript expression without any operators.
atom :: Parser (Parse Token)
atom = try callsAndIndices <|> simpleAtom

-- | Any JavaScript expression.
expression :: Parser (Parse Token)
expression = Expr.buildExpressionParser table atom <?> "expression"
  where table = [post "++", post "--"] :
                (map (map pref) unaryOperators ++
                 map (map bin) operators)

        op     op left right = Node' [left, op, right]
        unOp   op arg        = Node' [op, arg]
        postOp op arg        = Node' [arg, op]

        bin opStr  = Expr.Infix   (op     <$> operator opStr) Expr.AssocLeft
        pref opStr = Expr.Prefix  (unOp   <$> operator opStr)
        post opStr = Expr.Postfix (postOp <$> operator opStr)

        operator = try . fmap Leaf' . tokenize . fmap Operator . literal
