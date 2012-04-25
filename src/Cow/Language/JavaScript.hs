module Cow.Language.JavaScript (Value) where

import Prelude hiding (init)

import Control.Applicative ((<$), (<$>), (<*), (*>), (<*>), liftA2)

import Text.ParserCombinators.Parsec

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

terminator :: Parser Char
terminator = oneOf "\n;"

program :: Parser [AST Value]
program = statement `sepBy` (terminator <* spaces)

funDef :: Parser (AST Value)
funDef = do string "function"
            name <- var
            body <- block
            return $ Node (Keyword "function") [name, body]
        
compoundBlock :: String -> Parser (AST Value)
compoundBlock keyword = try $ Node <$> start <*> content
  where start = Keyword <$> (string keyword <* spaces)
        content = do initVal  <- init <* spaces
                     blockVal <- block
                     return $ [initVal, blockVal]
                     
wordBlock :: String -> Parser (AST Value)
wordBlock keyword = Node <$> try start <*> (return <$> block)
  where start = Keyword <$> (string keyword <* spaces)
                               
init :: Parser (AST Value)
init = Node Init <$> between (char '(' *> spaces) (char ')') (return <$> statement)

block :: Parser (AST Value)
block = statement <|> Node Block <$> between (char '{' *> spaces) (char '}') program
        
ifElse :: Parser (AST Value)
ifElse = do Node _ [initVal, blockVal] <- compoundBlock "if" <* spaces
            Node _ [elsePart]          <- wordBlock "else" 
            return $ Node (Keyword "if") [initVal, blockVal, elsePart]
            
keyword :: String -> Parser (AST Value)
keyword word = leaf . Keyword <$> string word <* spaces

wordBlocks :: Parser (AST Value)
wordBlocks = choice $ wordBlock <$> ["function", "if", "while", "for", "with"]

returnStmt :: Parser (AST Value)
returnStmt = Node <$> (Keyword <$> string "return")
                  <*> (return <$> statement)

statement :: Parser (AST Value)
statement =  ifElse
         <|> funDef
         <|> wordBlocks
         <|> keyword "break"
         <|> keyword "continue"
         <|> returnStmt 
         <|> block
         <|> varDecl
         
var :: Parser (AST Value)
var = leaf . Var <$> liftA2 (:) (letter <|> digit) (many idChar)
  where idChar = letter <|> digit <|> char '_'
        
varDecl :: Parser (AST Value)
varDecl = do keyword "var" *> spaces
             assignments <- assignment `sepBy1` (char ',' <* spaces)
             return $ Node (Keyword "var") assignments
             
assignment :: Parser (AST Value)
assignment = do name <- var <* spaces
                string "=" *> spaces
                val <- expression
                return $ Node (Operator "=") [name, val]
                
expression = undefined
