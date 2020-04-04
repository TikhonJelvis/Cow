{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE OverloadedLists #-}
-- | A basic JavaScript parser that produces a 'ParseTree' with
-- 'Token' values as leaves. The 'Token' values keep track of
-- whitespace consumed during lexing which can be accessed to
-- accurately print the parsed input but does not factor into equality
-- comparisons or other operations.
--
-- The parser has not been deeply tested so do not expect 100%
-- conformance to actual JavaScript syntax.
module Cow.Language.JavaScript where

import           Control.Lens                  hiding (noneOf)
import           Control.Monad                 (join)

import qualified Data.Char                     as Char
import           Data.List.NonEmpty            (NonEmpty ((:|)))
import qualified Data.List.NonEmpty            as NonEmpty
import           Data.Maybe                    (fromMaybe, maybeToList)
import           Data.Text                     (Text)
import qualified Data.Text                     as Text

import           Text.Parsec                   hiding (label)
import qualified Text.Parsec.Expr              as Expr
import           Text.Parsec.Text

import           Numeric                       (readHex, readInt, readOct)

import           Cow.Language.JavaScript.Value
import           Cow.Language.Token
import           Cow.ParseTree

-- * Parsing

-- ** Parsing helpers:

-- | Line comments.
-- @
-- // this is a comment
-- @
lineComment ∷ Parser Value
lineComment = do
  literal "//"
  contents <- many $ noneOf "\n"
  pure $ LineComment $ Text.pack contents

-- | Block comments that can span multiple lines (like '/* comment
-- */').
--
-- Note that block comments *do not nest* in JavaScript!
blockComment ∷ Parser Value
blockComment = do
  contents <- between (literal "/*") (literal "*/") anything
  pure $ BlockComment $ Text.pack contents
  where anything = many $  noneOf "*"
                       <|> (try $ char '*' <* notFollowedBy (char '/'))

-- | Either kind of comment.
comment ∷ Parser Value
comment = try lineComment <|> blockComment

-- | Converts a parser that doesn't care about whitespace into one
-- that consumes and saves the whitespace *after* the token, as well
-- as handling comments.
tokenize ∷ Parser Value → Parser Term
tokenize value = do
  val     <- value
  spaces  <- Text.pack <$> many spaceChar

  let token = Leaf' $ Token spaces val

  optionMaybe (try $ tokenize comment) <&> \case
    Just comment -> Node' [token, comment]
    Nothing      -> token
  where spaceChar = satisfy $ \ x -> Char.isSpace x && x /= '\n'

-- | Tokenizes a parser to *also* consume newlines—used for tokens
-- that prevent automatic semicolon insertion (ASI).
noASI ∷ Parser Value → Parser Term
noASI value = do
  val    <- value
  spaces <- Text.pack <$> many space
  let token = Leaf' $ Token spaces val
  optionMaybe (try $ noASI comment) <&> \case
    Just comment -> Node' [token, comment]
    Nothing      -> token

-- | Parses delimited lists like '1,2,3,4,' tokenizing all the pieces
-- (both values *and* delimiters).
--
-- Allows an optional trailing delimiter, as long as there is at least
-- one expression. (So just ',' won't parse!)
tokenizedList ∷ Parser Term
              -- ^ The separator (usually a comma).
              → Parser Term
              -- ^ The expression between separators.
              → Parser [Term]
tokenizedList sep exp = do
  entries <- join <$> many (try entry)
  final   <- optionMaybe exp
  pure $ entries <> maybeToList final
  where entry = [[a, b] | a <- exp, b <- sep]

-- | A character that makes up a valid JavaScript identifier.
--
-- This might not be entirely complete with obscure but valid Unicode
-- characters like zero-width joiners.
idChar ∷ Parser Char
idChar = alphaNum <|> oneOf "$_"

-- | Parses a keyword, based on @reserved@ from @Text.Parse.Token@.
keyword ∷ Text → Parser Term
keyword keyword = tokenize $ Keyword keyword <$ parseKeyword
  where parseKeyword = do
          literal keyword
          notFollowedBy idChar <?> ("end of " <> Text.unpack keyword)

-- | Parses a valid name which can start with a letter/$/_ followed by
-- any number of letters/numbers/$/_.
identifier ∷ Parser Name
identifier = Name <$> (Text.cons <$> startChar <*> rest) <?> "identifier"
  where startChar = letter <|> oneOf "$_"
        rest = Text.pack <$> many idChar

-- | Parse a literal string of characters (ie a keyword) into a 'Text'
-- value.
literal ∷ Text → Parser Text
literal str = Text.pack <$> (string $ Text.unpack str)

-- | Parses some punctuation into a token. As far as I know,
-- semicolons should never be inserted after punctuation.
punct ∷ Value → Text → Parser Term
punct value mark = noASI $ value <$ literal mark

-- | Parses a condition (the '(..)' in 'if (..)', 'while (..)',
-- 'switch (..)' and so on).
condition ∷ Parser Term → Parser Term
condition contents = do
  open <- punct CondStart "("
  stuff <- contents
  close <- punct CondEnd ")"
  pure $ Node' [open, stuff, close]
  
-- ** Language Productions

-- | The end of a line of code: a semicolon or a newline.
terminator ∷ Parser Term
terminator = noASI $  Semicolon <$ char ';'
                  <|> LineEnd   <$ char '\n'
                  <|> LineEnd   <$ eof

-- TODO: Figure out how to handle ternary operator properly?
-- | All the legal JavaScript binary operators, ordered by precedence.
operators ∷ [[Text]]
operators = [["."], ["*", "/", "%"], ["+", "-"],
             [">>>", ">>", "<<"], ["<=", "<", ">=", ">", "instanceof", "in"],
             ["===", "==", "!==", "!="], ["&"], ["^"], ["|"], ["&&"], ["||"],
             ["=", "*=", "/=", "%=", "+=", "-=", ">>=", "<<=", "&=", "|=", "^="]]


-- | All the legal JavaScript unary operators.
unaryOperators ∷ [[Text]]
unaryOperators = [["new"], ["++", "--"], ["+", "-", "~", "!", "typeof", "void", "delete"]]


-- | All the reserved words in JavaScript which aren't unary/binary
-- operators.
keywords ∷ [Text]
keywords = ["break", "catch", "const", "continue", "do", "export",
            "for", "function", "if", "import", "label", "case", "default",
            "let", "return", "switch", "throw", "try",
            "var", "while", "with", "yield"]

-- | Any valid identifier that is not already a reserved keyword (list
-- in @keywords@).
variable ∷ Parser Term
variable = tokenize $ identifier >>= notKeyword
  where notKeyword (Name name)
          | name `elem` keywords = unexpected ("reserved keyword " <> Text.unpack name)
          | otherwise            = return . Variable $ Name name

-- | Labels are identifiers which can be used by 'break' and
-- 'continue' to jump out of nested loops.
label ∷ Parser Term
label = tokenize $ Label <$> identifier

-- | Parses JavaScript string literals, including both normal escapes
-- and Unicode (both '\uXXXX' and '\u{XXXXXX}' formats).
stringLiteral ∷ Parser Term
stringLiteral = tokenize $ String <$> contents
  where contents = do
          start    <- oneOf "'\""
          contents <- many $ strChar start

          char start <?> "end of string"

          pure $ Text.pack contents

        strChar start = satisfy notSpecial
                     <|> escape <?> "string character"
          where notSpecial x = x /= start && x /= '\\'

        escape = char '\\' *> (escapeChar <|> unicodeEscape)
        escapeChar = escaped <$> oneOf "0btnvfr'\"\\\n" <?> "escape character"
        escaped c = case lookup c escapes of
                      Just res -> res
                      Nothing  -> error "Error in stringLiteral/escapeChar: \
                                        \parsed an invalid escape character."
        escapes = [('b', '\b'), ('t', '\t'), ('n', '\n'), ('v', '\v'), ('0', '\0'),
                   ('f', '\f'), ('r', '\r'), ('\'', '\''), ('\\', '\\')]

        -- parses \uXXXX and \u{X} … \u{XXXXXX} as Unicode characters
        unicodeEscape = char 'u' *> (fourDigit <|> other) <?> "unicode escape sequence"
          where toChar = Char.chr . read . ("0x" <>)
                fourDigit = toChar <$> count 4 hexDigit
                other = toChar <$> between (char '{') (char '}') (many1 hexDigit)

-- | Parses JavaScript numeric literals including other bases (@0x10@)
-- and floating point numbers with exponents (@1.5e10@).
--
-- This parses some technically invalid literals like @0923@,
-- @0b1233@, @-0x1.2@, but that is okay for the purposes for diffing
-- and merging.
number ∷ Parser Term
number = tokenize $ Num <$> (try intLit <|> floatLit)
  where -- an integer literal *not* in decimal (ie 0x10 but not 10)
        intLit = do
          sign   <- (negate <$ try (char '-') <|> id <$ optional (char '+'))
          format <- parseFormat
          num    <- many1 digit
          pure $ sign $ toFormat format $ num

        parseFormat = choice $ try . string <$> ["0x", "0X", "0b", "0B", "0"]
        toFormat format = fromMaybe read $ toReader <$> lookup format formats
        toReader f = fst . head . f
        formats = [("0", readOct), ("0x", readHex), ("0X", readHex),
                   ("0b", readBinary), ("0B", readBinary)]
        readBinary = readInt 2 isBinary (read . (:[]))
          where isBinary x = x == '0' || x == '1'

        -- a float literal: 105, 10.5, 10e5, 10.1e5… etc
        floatLit = do
          sign <- (negate <$ try (char '-')
              <|> id <$ optional (char '+'))

          num  <- try $ many1 digit
          dec  <- optionMaybe $ char '.' *> many1 digit
          exp  <- optionMaybe $ oneOf "eE" *> many1 digit

          let dec' = fromMaybe "" $ ("." <>) <$> dec
              exp' = fromMaybe "" $ ("e" <>) <$> exp

          pure $ sign $ read $ num <> dec' <> exp'

-- | The index part of an expression indexing into an array: ie the
-- '[f(x)]' from 'g(y)[f(x)]'.
indexExpression ∷ Parser Term
indexExpression = do
  open  <- punct IndexStart "["
  index <- expression
  close <- punct IndexEnd "]"
  pure $ Node' [open, index, close]

-- | The arguments of a function call expression: ie the '(1,2,3)' of
-- 'f(1,2,3)'.
callExpression ∷ Parser Term
callExpression = do
  open  <- punct CallStart "("
  args  <- tokenizedList (punct CallSep ",") expression
  close <- punct CallEnd ")"
  pure $ Node' $ open :| (args <> [close])

-- | An expression with a mix of function calls and array indexing
-- like 'f(1)[2](3)'.
--
-- Separated out to deal with left-recursion.
callsAndIndices ∷ Parser Term
callsAndIndices = do
  base  <- simpleAtom
  calls <- many1 (callExpression <|> indexExpression)
  pure $ Node' $ base :| calls

-- | Parses array literals like '[1,f(x), g(x)[3] + z]'.
arrayLiteral ∷ Parser Term
arrayLiteral = do
  open  <- punct ArrayStart "["
  items <- tokenizedList (punct ArraySep ",") expression
  close <- punct ArrayEnd "]"
  pure $ Node' $ open :| (items <> [close])

-- | Parses JavaScript object literals like '{a : 10, b : 11}'.
objectLiteral ∷ Parser Term
objectLiteral = do
  open  <- punct ObjStart "{"
  pairs <- tokenizedList (punct ObjSep ",") pair
  close <- punct ObjEnd "}"
  pure $ Node' $ open :| (pairs <> [close])
  where -- a key value pair in an object, like 'a : 10'
        pair = do
          field <- try stringLiteral <|> variable
          sep   <- punct ObjColon ":"
          val   <- expression
          pure $ Node' [field, sep, val]

-- | A block is a series of statements surrounded by '{' and '}'. The
-- braces are not optional! (The optional case for conditionals and
-- loops is handled separately.)
block ∷ Parser Term
block = do
  open  <- punct BlockStart "{"
  body  <- many statement
  close <- punct BlockEnd "}"
  pure $ Node' $ open :| (body <> [close])

-- | A function expression. The function can *optionally* have a name:
-- this covers both 'function () {}' and 'function foo() {}'.
functionLiteral ∷ Parser Term
functionLiteral = do
  func  <- keyword "function"
  name  <- optionMaybe variable
  open  <- punct ArgStart "("
  args  <- tokenizedList (punct ArgSep ",") variable
  close <- punct ArgEnd ")"
  body  <- block

  let argList = Node' $ open :| (args <> [close])
  pure $ Node' $ func :| (maybeToList name <> [argList, body])

-- | A self-contained piece of a JavaScript expression.
--
-- This is split from @atom@ to avoid left-recursion.
simpleAtom ∷ Parser Term
simpleAtom =  stringLiteral
          <|> try functionLiteral
          <|> try arrayLiteral
          <|> try objectLiteral
          <|> try number
          <|> try variable
          <|> parens
  where parens = do
          open  <- punct ParenStart "("
          exp   <- expression
          close <- punct ParenEnd ")"
          pure $ Node' [open, exp, close]

-- | Any JavaScript expression without any operators.
atom ∷ Parser Term
atom = try callsAndIndices <|> simpleAtom

-- | Any JavaScript expression except for the ternary operator ('a ? b : c').
expression' ∷ Parser Term
expression' = Expr.buildExpressionParser table atom <?> "expression"
  where table = [post "++", post "--"] :
                (map (map pref) unaryOperators <>
                 map (map bin) operators)

        op     op left right = Node' [left, op, right]
        unOp   op arg        = Node' [op, arg]
        postOp op arg        = Node' [arg, op]

        bin opStr  = Expr.Infix   (op     <$> operator opStr) Expr.AssocLeft
        pref opStr = Expr.Prefix  (unOp   <$> operator opStr)
        post opStr = Expr.Postfix (postOp <$> asiOp opStr)

        operator = try . noASI . fmap Operator . opLiteral
        asiOp    = try . tokenize . fmap Operator . opLiteral

        opLiteral str = literal str <* notFollowedBy (oneOf ".*/%+-><=!&^|")


-- | Parses the ternary operator ('a ? b : c').
ternary ∷ Parser Term
ternary = do
  cond  <- expression'
  start <- noASI $ Operator <$> literal "?"
  then_ <- expression
  end   <- noASI $ Operator <$> literal ":"
  else_ <- expression
  pure $ Node' [cond, start, then_, end, else_]

-- | Any JavaScript expression.
expression ∷ Parser Term
expression = try ternary <|> expression'

-- | Declaring a variable with 'var', 'let' or 'const'. Currently does
-- not support destructuring assignment.
declaration ∷ Parser Term
declaration = do
  start <- keyword "var" <|> keyword "let" <|> keyword "const"
  defs  <- tokenizedList (punct DeclSep ",") varDef
  pure $ Node' $ start :| defs
  where varDef  = do
          name <- variable
          val  <- optionMaybe setting
          pure $ Node' $ name :| maybeToList val
        setting = do
          op  <- tokenize $ Operator <$> literal "="
          val <- expression
          pure $ Node' [op, val]

-- | An 'if' statement with an optional 'else' clause afterwards.
ifElse ∷ Parser Term
ifElse = do
  if'   <- keyword "if"
  cond  <- condition expression
  body  <- statement
  else' <- maybeToList <$> optionMaybe elseClause
  pure $ Node' $ if' :| [cond, body] <> else'
  where elseClause = do
          else' <- keyword "else"
          body  <- statement
          pure $ Node' [else', body]

-- | A 'try { ... }' statement with any number of 'catch' clauses and
-- an optional 'finally' clause.
--
-- This will parse if there are no 'catch' *or* 'finally' clauses even
-- though that isn't strictly valid JavaScript.
tryCatch ∷ Parser Term
tryCatch = do
  try     <- keyword "try"
  body    <- block
  catches <- many catch
  finally <- maybeToList <$> optionMaybe finally
  pure $ Node' $ try :| [body] <> catches <> finally
  where catch = do
          start <- keyword "catch"
          open  <- punct CatchStart "("
          eName <- variable
          close <- punct CatchEnd ")"
          body  <- block
          pure $ Node' [start, open, eName, close, body]
        finally = do
          start <- keyword "finally"
          body  <- block
          pure $ Node' [start, body]

-- | Parses a switch statement ('switch (foo) { case 'a': ... default: ... }').
switch ∷ Parser Term
switch = do
  start <- keyword "switch"
  cond  <- condition expression
  body  <- caseBlock
  pure $ Node' [start, cond, body]
  where caseBlock = do
          open  <- punct BlockStart "{"
          body  <- many (case_ <|> default_)
          close <- punct BlockEnd "}"
          pure $ Node' $ open :| body <> [close]
        case_ = do
          start <- keyword "case"
          val   <- expression
          end   <- punct CaseColon ":"
          body  <- many (try statement)
          pure $ Node' $ start :| [val, end] <> body
        default_ = do
          start <- keyword "default"
          end   <- punct CaseColon  ":"
          body  <- many statement
          pure $ Node' $ start :| [end] <> body

-- | Simple 'while' loops.
while ∷ Parser Term
while = do
  start <- keyword "while"
  cond  <- condition expression
  body  <- statement
  pure $ Node' [start, cond, body]

-- | do-while loops. Do people even use these any more?
doWhile ∷ Parser Term
doWhile = do
  start <- keyword "do"
  body  <- block
  while <- keyword "while"
  cond  <- condition expression
  pure $ Node' [start, body, while, cond]

-- | For loops of the form 'for (;;)'.
for ∷ Parser Term
for = do
  start <- keyword "for"
  cond  <- condition forCondition
  body  <- statement
  pure $ Node' [start, cond, body]
  where forCondition = do
          init <- expression <|> declaration
          sep1 <- punct ForSep ";"
          cond <- expression
          sep2 <- punct ForSep ";"
          end  <- expression
          pure $ Node' [init, sep1, cond, sep2, end]

-- | A 'for (x in ls)' or 'for (x of ls)' loop.
for' ∷ Text → Parser Term
for' sep = do
  start <- keyword "for"
  cond  <- condition for'Condition
  body  <- statement
  pure $ Node' [start, cond, body]
  where for'Condition = do
          var  <- optionMaybe $ keyword "var"
          name <- variable
          in_  <- keyword sep
          ls   <- expression
          let var' = maybeToList var
          pure $ Node' $ NonEmpty.fromList $ var' <> [name, in_, ls]

-- | A 'with' statement which you probably *shouldn't* use any
-- more—but I'm sure people do.
with ∷ Parser Term
with = do
  start <- keyword "with"
  cond  <- condition expression
  body  <- statement
  pure $ Node' [start, cond, body]

-- | A JavaScript statement, including its terminator (ie explicit or
-- implicit semicolon).
statement ∷ Parser Term
statement = do
  contents <- statement'
  -- terminator is optional to handle end of block (ie '{
  -- 1 + 2 }' on one line)
  end      <- optionMaybe terminator
  pure $ Node' $ contents :| maybeToList end
 where statement' =  try block
                 <|> try expression
                 <|> try declaration
                 <|> keyworded "return" expression
                 <|> keyworded "throw" expression
                 <|> keyworded "break" label
                 <|> keyworded "continue" label
                 <|> keyword "debugger"
                 <|> try labelDecl
                 <|> try ifElse
                 <|> try tryCatch
                 <|> try switch
                 <|> try while
                 <|> try doWhile
                 <|> try for
                 <|> try (for' "in")
                 <|> try (for' "of")
                 <|> try with
                 <|> tokenize comment
       keyworded word val = try $ do
         start <- keyword word
         end   <- optionMaybe val
         pure $ Node' $ start :| maybeToList end
       labelDecl = do
         l <- label
         c <- punct LabelStart ":"
         pure $ Node' [l, c]

-- | Parses a whole JavaScript program.
program ∷ Parser Term
program = many space *> (Node' . NonEmpty.fromList <$> many1 statement) <* eof
