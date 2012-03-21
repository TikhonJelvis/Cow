module Main where

import Cow.Diff
import Cow.Type

main :: IO ()
main = writeFile "out.ltx" out
  where out = unlines ["\\documentclass{article}",
                       "\\usepackage{synttree}",
                       "\\begin{document}",
                       "\\synttree" ++ show result,
                       "\\end{document}"]
        result = diff (Node (0 :: Integer) [Node 1 []]) (Node 0 [Node 0 []])
        