module Cow.Language.JavaScript (Value(..), parser, program) where

import Control.Applicative ((<$), (<$>), (<*), (*>), (<*>), liftA2)
import Control.DeepSeq

import Data.List  (nub, intercalate)
import Data.Maybe (maybeToList)

import Text.ParserCombinators.Parsec 
import qualified Text.ParserCombinators.Parsec.Token as T
import qualified Text.ParserCombinators.Parsec.Expr as E
import Text.ParserCombinators.Parsec.Language (javaStyle)

import Cow.Scope
import Cow.Type

data Value = Root
           | Empty
           | Var      String
           | Key      String -- The name of an object's field
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
           | Parameters
           | Assign
           | Block deriving (Eq)
                            
instance Show Value where
  show Root         = "\\uppercase{root}"
  show Empty        = "$\\epsilon$"
  show (Var v)      = v
  show (Num n)      = "$" ++ take (length (show n) - 2) (show n) ++"$"
  show (Str s)      = show s
  show (Key k)      = "\\textbf{" ++ k ++ "}"
  show (Regex r)    = "/" ++ r ++ "/"
  show (Keyword k)  = "\\textsc{" ++ k ++ "}"
  show (Operator o) = if o == "." then "dot" else "$" ++ o ++ "$"
  show Function     = "\\uppercase{function}"
  show Call         = "\\uppercase{call}"
  show Array        = "\\uppercase{array}"
  show Object       = "\\uppercase{object}"
  show Init         = "\\uppercase{init}"
  show Args         = "\\uppercase{args}"
  show Block        = "\\uppercase{block}"
  show Parameters   = "\\uppercase{parameters}"
  show Assign       = "\\uppercase{assignment}"

instance NFData Value

instance Scopable Value where
  bindings (Node (Keyword "var") children) = children >>= getBindings
  bindings (Node Parameters children)      = children >>= getBindings
  bindings _                               = []

  globalBindings n@(Node (Operator "=") _) = getBindings n
  globalBindings _                         = []

  newEnv Function = True
  newEnv _        = False

  bound Var{} = True 
  bound _     = False
  
getBindings :: AST Value -> [Value]
getBindings (Node Assign (v:_))         = [val v]
getBindings (Node Parameters vs)        = val <$> vs
getBindings (Node v@Var{} [])           = [v]
getBindings (Node (Operator "=") (v:_)) = [val v]
getBindings _                           = []

ε :: Parser String
ε = string ""

terminator :: Parser ()
terminator = optional (oneOf ";\n") <|> eof

separators :: Parser ()
separators = skipMany (space <|> char ';') 

operators :: [[String]]
operators = [["."], ["*", "/", "%"], ["+", "-"],
             [">>", ">>>", "<<"], ["<", "<=", ">", ">=", "in", "instanceof"],
             ["==", "===", "!=", "!=="], ["&"], ["^"], ["|"], ["&&"], ["||"],
             ["=", "*=", "/=", "%=", "+=", "-=", ">>=", "<<=", "&=", "|=", "^="]]
            
unaryOperators :: [[String]]
unaryOperators = [["new"], ["++", "--"], ["+", "-", "~", "!", "typeof", "void", "delete"]]

lexer :: T.TokenParser ()
lexer = T.makeTokenParser $ javaStyle {
  T.reservedOpNames = nub $ concat operators ++ concat unaryOperators ++ ["?", ":"],
  T.reservedNames = ["break", "catch", "const", "continue", "do", "export",
                     "for", "function", "if", "import", "label",
                     "let", "return", "switch", "throw", "try", 
                     "var", "while", "with", "yield"]
  }
            
keyword :: String -> Parser Value
keyword word = Keyword word <$ T.reserved lexer word <?> word

parser :: Parser (AST Value)
parser = Node Root <$> program

program :: Parser [AST Value]
program = T.whiteSpace lexer *> many (statement <* separators)

funDef :: Parser (AST Value)
funDef = T.reserved lexer "function" *>
         (func <$> var <*> parameterList <*> block) <?> "function declaration"
  where func name args body = Node Function [name, args, body]

parameterList :: Parser (AST Value)
parameterList = Node Parameters . map (leaf . Var) <$> argList
  where argList = T.parens lexer . T.commaSep lexer $ T.identifier lexer
        
compoundBlock :: String -> Parser (AST Value)
compoundBlock word = try $ compound <$> keyword word
                                    <*> initExp
                                    <*> optBlock
  where compound key initVal body = Node key [initVal, body]
 
initExp :: Parser (AST Value)
initExp = Node Init <$> T.parens lexer (return <$> expression)

wordBlock :: String -> Parser (AST Value)
wordBlock word = Node <$> try (keyword word) <*> (return <$> optBlock)

block :: Parser (AST Value)
block = Node Block <$> T.braces lexer program <?> "block"
        
optBlock :: Parser (AST Value)
optBlock = block <|> toBlock <$> statement
  where toBlock n = Node Block [n]

ifElse :: Parser (AST Value)
ifElse = do Node _ [initVal, blockVal] <- compoundBlock "if" <* T.whiteSpace lexer
            Node _ [elsePart]          <- wordBlock "else" <?> "else clause"
            return $ Node (Keyword "if") [initVal, blockVal, elsePart]

doWhile :: Parser (AST Value)
doWhile = do Node _ [doPart] <- wordBlock "do"
             cond            <- T.reserved lexer "while" *> initExp
             return $ Node (Keyword "do") [doPart, cond]

compoundBlocks :: Parser (AST Value)
compoundBlocks = choice $ try . compoundBlock <$> ["if", "while", "for", "with"]

returnStmt :: Parser (AST Value)
returnStmt = (Node <$> (Keyword "return" <$ T.reserved lexer "return")
                   <*> (maybeToList <$> optionMaybe statement)) <?> "return statement"
             
forLoop :: Parser (AST Value)
forLoop = do keyword "for"
             (i, c, a) <- T.parens lexer $ (,,) <$> statement <*> statement <*> statement
             let initVal = Node Init [i, c, a]
             body      <- optBlock
             return $ Node (Keyword "for") [initVal, body]

terminatedStatement :: Parser (AST Value)
terminatedStatement = (compoundBlocks
                   <|> leaf <$> keyword "break"
                   <|> leaf <$> keyword "continue"
                   <|> returnStmt
                   <|> varDecl
                   <|> expression) <* terminator <* separators

statement :: Parser (AST Value)
statement = (try ifElse
         <|> try doWhile
         <|> try forLoop
         <|> try funDef
         <|> terminatedStatement
         <|> block) <?> "statement"
         
var :: Parser (AST Value)
var = leaf . Var <$> T.identifier lexer <?> "identifier"

str :: Parser (AST Value)
str = leaf . Str <$> T.stringLiteral lexer <?> "string literal"
        
varDecl :: Parser (AST Value)
varDecl = do keyword "var"
             assignments <- T.commaSep1 lexer $ try assignment <|> var
             (return $ Node (Keyword "var") assignments) <?> "variable declaration"
             
assignment :: Parser (AST Value)
assignment = Node Assign <$> liftA2 (:) (var <* T.reservedOp lexer "=") (return <$> expression) <?> "assignment"

funLit :: Parser (AST Value)
funLit = T.reserved lexer "function" *> (func <$> parameterList <*> block)
  where func args body = Node Function [args, body]

table :: E.OperatorTable Char () (AST Value)
table = [post "++", post "--"]  :
        (map (map pref) unaryOperators ++
         map (map bin) operators)
  where op opStr left right = Node (Operator opStr) [left, right]
        unOp opStr arg = Node (Operator opStr) [arg]
        postOp opStr arg = Node (Operator $ "post" ++ opStr) [arg]
        bin opStr  = E.Infix (op opStr <$ T.reservedOp lexer opStr) E.AssocLeft
        pref opStr = E.Prefix (unOp opStr <$ T.reservedOp lexer opStr)
        post opStr = E.Postfix (postOp opStr <$ T.reservedOp lexer opStr)

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
objLit = Node Object <$> properties
  where properties = T.braces lexer . T.commaSep lexer $
                     pair <$> (str <|> var) <*> (T.colon lexer *> expression)
        pair name value = Node (Operator ":") [toKey name, value]
        toKey (Node (Var v) []) = Node (Key v) []
        toKey (Node (Str v) []) = Node (Key v) []
        toKey k                 = k                
        
arrayLit :: Parser (AST Value)
arrayLit = Node Array <$> vals
  where vals = T.squares lexer $ T.commaSep lexer expression

simpleAtom :: Parser (AST Value)
simpleAtom =  str
          <|> (leaf . Num <$> try (T.float lexer) <?> "floating point literal")
          <|> (leaf . Num . fromIntegral <$> try (T.hexadecimal lexer) <?> "hex literal")
          <|> (leaf . Num . fromIntegral <$> T.integer lexer <?> "integer literal")
          <|> (T.parens lexer expression <?> "parenthesized expression")
          <|> try funLit
          <|> try objLit
          <|> try arrayLit
          <|> var
              
atom :: Parser (AST Value)
atom =  try indexOrCall
    <|> simpleAtom
