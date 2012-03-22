module Main where

import Control.Applicative ((*>), (<*), (<$>), (<*>))

import Text.ParserCombinators.Parsec

import Cow.Diff
import Cow.Type

toLaTeX :: Show a => a -> IO ()
toLaTeX result = writeFile "out.ltx" out
  where out = unlines ["\\documentclass{article}",
                       "\\usepackage{change}",
                       "\\begin{document}",
                       "\\synttree" ++ show result,
                       "\\end{document}"]
        

nums :: Parser (AST Integer)
nums = char '[' *> spaces *> (Node <$> value <*> children) <* char ']'
  where value    = read <$> many1 digit <* spaces
        children = many (nums <* spaces)

draw :: String -> String -> IO ()
draw inp1 inp2 = case diff <$> parse nums "left" inp1 <*> parse nums "right" inp2 of
  Right val -> toLaTeX val
  Left  err -> putStrLn $ "Error: " ++ show err