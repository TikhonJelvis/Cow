module Cow.Language.JavaScript (Value) where

import Prelude hiding (init)

import Control.Applicative ((<$), (<$>), (<*), (*>), (<*>), liftA2)

import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as T
import Text.ParserCombinators.Parsec.Language (javaStyle)

import Cow.Type

type Tag  = Int
type Name = String

data Value = Root
           | Var      Name
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
                            
lexer :: T.TokenParser ()
lexer = T.makeTokenParser $ javaStyle {
  T.reservedOpNames = ["+", "++", "-", "--", "=", "==", "===", "!", "!=", "!==", "~",
                         "&", "&&", "|", "||", "^", "+=", "-=", "*", "*=", "/", "/=", "%",
                         ">>", ">>>", "<<", ">>=", "<<=", "&=", "|=", "^=", ",",
                         "%=", ",", ".", "?", ":"],
  T.reservedNames = ["break", "catch", "const", "continue", "delete", "do", "export",
                     "for", "function", "if", "import", "in", "instanceof", "label",
                     "let", "new", "return", "switch", "this", "throw", "try", "typeof",
                     "var", "void", "while", "with", "yield"]
  }
            
keyword :: String -> Parser Value
keyword word = Keyword word <$ T.reserved lexer word

program :: Parser [AST Value]
program = T.semiSep lexer statement

funDef :: Parser (AST Value)
funDef = do T.reserved lexer "function"
            name <- var
            body <- block
            return $ Node (Keyword "function") [name, body]
        
compoundBlock :: String -> Parser (AST Value)
compoundBlock word = try $ Node <$> keyword word <*> content
  where content = do initVal  <- init <* spaces
                     blockVal <- block <|> statement
                     return $ [initVal, blockVal]
                     
wordBlock :: String -> Parser (AST Value)
wordBlock word = Node <$> try (keyword word) <*> (return <$> (block <|> statement))
                               
init :: Parser (AST Value)
init = Node Init <$> T.parens lexer (return <$> statement)

block :: Parser (AST Value)
block = Node Block <$> T.braces lexer program
        
ifElse :: Parser (AST Value)
ifElse = do Node _ [initVal, blockVal] <- compoundBlock "if" <* spaces
            Node _ [elsePart]          <- wordBlock "else" 
            return $ Node (Keyword "if") [initVal, blockVal, elsePart]

wordBlocks :: Parser (AST Value)
wordBlocks = choice $ wordBlock <$> ["function", "if", "while", "for", "with"]

returnStmt :: Parser (AST Value)
returnStmt = Node <$> (Keyword <$> string "return")
                  <*> (return <$> statement)

statement :: Parser (AST Value)
statement =  ifElse
         <|> funDef
         <|> wordBlocks
         <|> leaf <$> keyword "break"
         <|> leaf <$> keyword "continue"
         <|> returnStmt 
         <|> block
         <|> varDecl
         
var :: Parser (AST Value)
var = leaf . Var <$> T.identifier lexer
        
varDecl :: Parser (AST Value)
varDecl = do keyword "var" *> spaces
             assignments <- T.commaSep1 lexer $ assignment <|> var
             return $ Node (Keyword "var") assignments
             
assignment :: Parser (AST Value)
assignment = Node (Operator "=") <$> liftA2 (:)
               (var <* spaces <* string "=" <* spaces) (return <$> expression)
                
expression = undefined
