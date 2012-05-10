module Main where

import Control.Applicative ((*>), (<*), (<$>), (<*>))

import Text.ParserCombinators.Parsec

import Cow.Diff
import Cow.Scope
import Cow.Substructure
import Cow.Type

import qualified Cow.Language.JavaScript as JS

toLaTeX :: Show a => String -> String -> a -> IO ()
toLaTeX left right result = writeFile "out.ltx" out
  where out = unlines ["\\documentclass[12pt]{article}",
                       "\\usepackage{change}",
                       "\\begin{document}",
                       "\\synttree" ++ left,
                       "\\synttree" ++ right ++ "\\\\ \\\\ \\\\ \\\\",
                       "\\synttree" ++ show result,
                       "\\end{document}"]

toTreeLaTeX :: (Show error) => Either error [AST JS.Value] -> IO ()
toTreeLaTeX inp = case inp of
  Right tree -> writeFile "out.ltx" (out tree)
  Left  err  -> print err
  where out tree = unlines ["\\documentclass[10pt]{article}",
                            "\\usepackage[margin=1in, paperwidth=100in, textwidth=100in, paperheight=10in]{geometry}",
                            "\\usepackage{change}",
                            "\\begin{document}",
                            "\\synttree" ++ (show . tag $ Node JS.Root tree),
                            "\\end{document}"]

nums :: Parser (AST Integer)
nums = char '[' *> spaces *> (Node <$> value <*> children) <* char ']'
  where value    = read <$> many1 digit <* spaces
        children = many (nums <* spaces)

-- draw :: String -> String -> IO ()
-- draw inp1 inp2 = case diff <$> parse nums "left" inp1 <*> parse nums "right" inp2 of
--   Right val -> toLaTeX inp1 inp2 val
--   Left  err -> putStrLn $ "Error: " ++ show err
  
testParse :: IO ()
testParse = parseFromFile JS.program "test.js" >>= toTreeLaTeX
  where  printRes (Right res) = print $ Node JS.Root res
         printRes (Left err)  = print err
         
testDiff :: String -> String -> IO ()
testDiff inp1 inp2 = case tagDiff <$> parse nums "left" inp1 <*> parse nums "right" inp2 of
  Right val -> toLaTeX inp1 inp2 val
  Left err  -> putStrLn $ "Error: " ++ show err
         
main :: IO ()
main = testDiff "[0[14[15[16[12[13[1[5[11][8[10][9]]][6][7]]]]]]]]" "[0[14[11][8[10][9]][15[16[12[13[1[5][6][7]]]]]]]]"
