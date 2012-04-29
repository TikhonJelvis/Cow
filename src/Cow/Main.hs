module Main where

import Control.Applicative ((*>), (<*), (<$>), (<*>))

import Text.ParserCombinators.Parsec

import Cow.Diff
import Cow.Parse
import Cow.Type

import qualified Cow.Language.JavaScript as JS

toLaTeX left right result = writeFile "out.ltx" out
  where out = unlines ["\\documentclass[12pt]{article}",
                       "\\usepackage{change}",
                       "\\begin{document}",
                       "\\synttree" ++ left,
                       "\\synttree" ++ right ++ "\\\\ \\\\ \\\\ \\\\",
                       "\\synttree" ++ show result,
                       "\\end{document}"]
        

nums :: Parser (AST Integer)
nums = char '[' *> spaces *> (Node <$> value <*> children) <* char ']'
  where value    = read <$> many1 digit <* spaces
        children = many (nums <* spaces)

draw :: String -> String -> IO ()
draw inp1 inp2 = case diff <$> parse nums "left" inp1 <*> parse nums "right" inp2 of
  Right val -> toLaTeX inp1 inp2 val
  Left  err -> putStrLn $ "Error: " ++ show err
  
testParse :: IO ()
testParse = parseFile JS.parse "test.js" >>= printRes
  where  printRes (Right res) = print res
         printRes (Left err)  = print err