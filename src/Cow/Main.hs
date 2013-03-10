module Main where

import           Control.Applicative           ((*>), (<$>), (<*), (<*>))

import           System.Environment            (getArgs)

import           Text.ParserCombinators.Parsec

import           Cow.Diff
import           Cow.Merge
import           Cow.Scope
import           Cow.Substructure
import           Cow.Type
import           Cow.UI

import qualified Cow.Language.JavaScript       as JS

displayAST :: Show a => AST a -> String
displayAST (Node value [])       = "[" ++ show value ++ "]"
displayAST (Node value children) = "[" ++ show value ++ unwords (show <$> children) ++ "]"

toLaTeX' :: Show a => String -> String -> AST a -> IO ()
toLaTeX' left right result = writeFile "out.ltx" out
  where out = unlines ["\\documentclass[12pt]{article}",
                       "\\usepackage{change}",
                       "\\usepackage[margin=1in, paperwidth=100in, textwidth=100in, paperheight=10in]{geometry}",
                       "\\begin{document}",
                       "\\synttree" ++ left,
                       "\\synttree" ++ right ++ "\\\\ \\\\ \\\\ \\\\",
                       "\\synttree" ++ displayAST result,
                       "\\end{document}"]


toLaTeX :: Show a => String -> String -> String -> String -> String -> String -> String -> AST a -> IO ()
toLaTeX outFile base left right d1 d2 d3 result = writeFile (outFile ++ ".ltx") out
  where out = unlines ["\\documentclass[12pt]{article}",
                       "\\usepackage[margin=1in, paperwidth=40in, textwidth=40in, paperheight=10in]{geometry}",
                       "\\usepackage{change}",
                       "\\begin{document}",
                       "\\synttree" ++ base,
                       "\\synttree" ++ left,
                       "\\synttree" ++ right ++ "\\\\ \\\\ \\\\ \\\\",
                       "\\synttree" ++ d1,
                       "\\synttree" ++ d2 ++ "\\\\ \\\\ \\\\ \\\\",
                       "\\synttree" ++ d3 ++ "\\\\ \\\\ \\\\ \\\\",
                       "\\synttree" ++ displayAST result,
                       "\\end{document}"]

toTreeLaTeX :: (Show error) => Either error [AST JS.Value] -> IO ()
toTreeLaTeX inp = case inp of
  Right tree -> writeFile "out.ltx" (out tree)
  Left  err  -> print err
  where out tree = unlines ["\\documentclass[10pt]{article}",
                            "\\usepackage[margin=1in, paperwidth=100in, textwidth=100in, paperheight=10in]{geometry}",
                            "\\usepackage{change}",
                            "\\begin{document}",
                            "\\synttree" ++ (displayAST . tag $ Node JS.Root tree),
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

testDiff :: String -> String -> IO ()
testDiff inp1 inp2 = case diff <$> parse nums "left" inp1 <*> parse nums "right" inp2 of
  Right value -> toLaTeX' inp1 inp2 value
  Left err    -> putStrLn $ "Error: " ++ show err

testJSDiff :: String -> String -> IO ()
testJSDiff inp1 inp2 = case tagDiff <$> parse JS.parser "left" inp1 <*> parse JS.parser "right" inp2 of
  Right value -> toLaTeX' inp1 inp2 value
  Left err    -> putStrLn $ "Error: " ++ show err

testMerge :: String -> String -> String -> String -> IO ()
testMerge b l r out = case resolveConflicts <$> get b <*> get l <*> get r of
  Right (value, d1, d2, d3) ->
    toLaTeX out "" "" "" (displayAST d1) (displayAST d2) (displayAST d3) value
  Left err                  -> print err
  where get a = parse JS.parser "js" a

testFileMerge :: String -> String -> String -> String -> IO ()
testFileMerge b l r out = do b' <- readFile b
                             l' <- readFile l
                             r' <- readFile r
                             testMerge b' l' r' out

testDisplay :: String -> String -> String -> IO ()
testDisplay lFile rFile out = do l <- parseFromFile JS.parser lFile
                                 r <- parseFromFile JS.parser rFile
                                 toOutput l r
  where toOutput (Right l) (Right r) = do writeFile (out ++ ".html") . toHTML . displayDiff $ tagDiff l r
                                          writeFile (out ++ ".ltx") . outLaTex  $ tagDiff l r
        toOutput _ _                 = putStrLn "Parser error!"
        outLaTex result =  unlines ["\\documentclass[12pt]{article}",
                                    "\\usepackage{change}",
                                    "\\usepackage[margin=1in, paperwidth=100in, textwidth=100in, paperheight=10in]{geometry}",
                                    "\\begin{document}",
                                    "\\synttree" ++ displayAST result,
                                    "\\end{document}"]

usage :: String
usage = "Moo"

main :: IO ()
main = do args <- getArgs
          case args of
            []                      -> putStrLn usage
            ["diff", l, r, out]     -> testDisplay l r out
            ["merge", b, l, r, out] -> testFileMerge b l r out
            _                       -> putStrLn usage
