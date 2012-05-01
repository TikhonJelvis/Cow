module Cow.Language.JavaScript (Value(..), parser) where

import Control.Applicative ((<$), (<$>), (<*), (*>), (<*>), liftA2)

import Data.List  (nub)
import Data.Maybe (maybeToList)

import Text.ParserCombinators.Parsec 
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Expr as E
import Text.ParserCombinators.Parsec.Language (javaStyle)

import Cow.Type

data Value = Root
           | Empty
           | Var      String
           | Num      Double
           | Str      String
           | Regex    String
           | Keyword  String
           | Operator String
           | Function
           | Call
           | Array
           | Object
           | Init -- The bit between parentheses in loops, if statements and so on...
           | Args
           | Block deriving (Eq)
                            
instance Show Value where
  show Root  = "\\uppercase{root}"
  show Empty = "$\\epsilon$"
  show (Var n) = n
  show (Num n) = "$" ++ take (length (show n) - 2) (show n) ++"$"
  show (Str s) = show s
  show (Regex r) = "/" ++ r ++ "/"
  show (Keyword k) = "\\textsc{" ++ k ++ "}"
  show (Operator o) = if o == "." then "dot" else"$" ++ o ++ "$"
  show Function = "\\uppercase{function}"
  show Call = "\\uppercase{call}"
  show Array = "\\uppercase{array}"
  show Object = "\\uppercase{object}"
  show Init = "\\uppercase{init}"
  show Args = "\\uppercase{args}"
  show Block = "\\uppercase{block}"

parser :: SourceName -> String -> Either ParseError [AST Value]
parser = parse program

ε :: Parser String
ε = string ""

terminator :: Parser ()
terminator = optional (oneOf ";\n") <|> eof

operators :: [[String]]
operators = [["."], ["*", "/", "%"], ["+", "-"],
             ["==", "===", "!=", "!==", "<", ">", "<=", ">="],
             ["&", "&&", "|", "||", "^"],
             [">>", ">>>", "<<"], ["=", "*=", "/=", "%="], ["+=", "-="],
             [">>=", "<<=", "&=", "|=", "^="]]
            
unaryOperators :: [String]
unaryOperators = ["+", "++", "-", "--", "~", "!"]

lexer :: T.TokenParser ()
lexer = T.makeTokenParser $ javaStyle {
  T.reservedOpNames = nub $ concat operators ++ unaryOperators ++ ["?", ":"],
  T.reservedNames = ["break", "catch", "const", "continue", "delete", "do", "export",
                     "for", "function", "if", "import", "in", "instanceof", "label",
                     "let", "new", "return", "switch", "this", "throw", "try", "typeof",
                     "var", "void", "while", "with", "yield"]
  }
            
keyword :: String -> Parser Value
keyword word = Keyword word <$ T.reserved lexer word <?> word

program :: Parser [AST Value]
program = T.whiteSpace lexer *> many (statement <* spaces)

funDef :: Parser (AST Value)
funDef = T.reserved lexer "function" *>
         (func <$> var <*> parameterList <*> block) <?> "function declaration"
  where func name args body = Node Function [name, args, body]

parameterList :: Parser (AST Value)
parameterList = Node Init . map (leaf . Var) <$> argList
  where argList = T.parens lexer . T.commaSep lexer $ T.identifier lexer
        
compoundBlock :: String -> Parser (AST Value)
compoundBlock word = try $ compound <$> keyword word
                                    <*> initExp
                                    <*> (block <|> statement)
  where compound key initVal body = Node key [initVal, body]
        initExp = Node Init <$> T.parens lexer (return <$> expression)

wordBlock :: String -> Parser (AST Value)
wordBlock word = Node <$> try (keyword word) <*> (return <$> (block <|> statementBlock))
  where statementBlock = do content <- statement
                            return $ Node Block [content]
block :: Parser (AST Value)
block = Node Block <$> T.braces lexer program <?> "block"
        
ifElse :: Parser (AST Value)
ifElse = do Node _ [initVal, blockVal] <- compoundBlock "if" <* spaces
            Node _ [elsePart]          <- wordBlock "else" <?> "else clause"
            return $ Node (Keyword "if") [initVal, blockVal, elsePart]

compoundBlocks :: Parser (AST Value)
compoundBlocks = choice $ try . compoundBlock <$> ["if", "while", "for", "with"]

returnStmt :: Parser (AST Value)
returnStmt = (Node <$> (Keyword "return" <$ T.reserved lexer "return")
                   <*> (maybeToList <$> optionMaybe statement)) <?> "return statement"

terminatedStatement :: Parser (AST Value)
terminatedStatement = (compoundBlocks
                   <|> leaf <$> keyword "break"
                   <|> leaf <$> keyword "continue"
                   <|> returnStmt
                   <|> varDecl
                   <|> expression) <* terminator <* spaces

statement :: Parser (AST Value)
statement = (try ifElse
         <|> try funDef
         <|> terminatedStatement
         <|> block) <?> "statement"
         
var :: Parser (AST Value)
var = leaf . Var <$> T.identifier lexer <?> "identifier"

str :: Parser (AST Value)
str = leaf . Str <$> T.stringLiteral lexer <?> "string literal"
        
varDecl :: Parser (AST Value)
varDecl = do keyword "var" *> spaces
             assignments <- T.commaSep1 lexer $ try assignment <|> var
             (return $ Node (Keyword "var") assignments) <?> "variable declaration"
             
assignment :: Parser (AST Value)
assignment = Node (Operator "=") <$> liftA2 (:)
               (var <* spaces <* string "=" <* spaces) (return <$> expression) <?> "assignment"

op :: String -> (AST Value) -> (AST Value) -> (AST Value)
op opStr left right = Node (Operator opStr) [left, right]

funLit :: Parser (AST Value)
funLit = T.reserved lexer "function" *> (func <$> parameterList <*> block)
  where func args body = Node Function [args, body]

bin :: String -> E.Operator Char () (AST Value)
bin opStr = E.Infix (op opStr <$ T.reservedOp lexer opStr) E.AssocLeft

table :: E.OperatorTable Char () (AST Value)
table = map (map bin) operators

expression :: Parser (AST Value)
expression = E.buildExpressionParser table atom <?> "expression"
                  
arguments :: Parser [AST Value]
arguments =  [] <$ try (T.parens lexer ε)
         <|> T.parens lexer (T.commaSep lexer expression)

indexOrCall :: Parser (AST Value)
indexOrCall = do base  <- simpleAtom
                 calls <- many $ arguments <|> return <$> T.squares lexer expression
                 return $ foldl (\ fn args -> Node Call [fn, Node Args args]) base calls

objLit :: Parser (AST Value)
objLit = Node Object <$> bindings
  where bindings = T.braces lexer . T.commaSep lexer $
                   pair <$> (str <|> var) <*> (T.colon lexer *> expression)
        pair name value = Node (Operator ":") [name, value]

simpleAtom :: Parser (AST Value)
simpleAtom =  str
          <|> (leaf . Num <$> try (T.float lexer) <?> "floating point literal")
          <|> (leaf . Num . fromIntegral <$> try (T.hexadecimal lexer) <?> "hex literal")
          <|> (leaf . Num . fromIntegral <$> T.integer lexer <?> "integer literal")
          <|> (T.parens lexer expression <?> "parenthesized expression")
          <|> try funLit
          <|> try objLit
          <|> var
              
atom :: Parser (AST Value)
atom =  try indexOrCall
    <|> simpleAtom
