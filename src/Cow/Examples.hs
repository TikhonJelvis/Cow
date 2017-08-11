-- | This modules contains example trees useful for interactively
-- experimenting with the system. Later on, it will also contain
-- example uses of other parts of the framework.
module Cow.Examples where

import           Data.Array                    (Array)
import qualified Data.Array                    as Array
import           Data.Foldable
import           Data.List                     (intercalate)
import qualified Data.Text                     as Text

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

import           Text.Parsec                   (parse)
import           Text.Printf                   (printf)

import           Cow.Diff
import qualified Cow.Language.JavaScript       as JS
import qualified Cow.Language.JavaScript.Value as JS
import           Cow.Language.Token
import           Cow.ParseTree
import           Cow.ParseTree.Read
import           Cow.ParseTree.Viz

-- | A reasonably small tree that has multiple levels and is
-- relatively unblaanced.
base :: Parse' String
base = show <$> readTree' "[[1][2[3 4]][5[6][7[10[8 9 10 11 12 25]10][13 14 15]]]]"

-- | The base tree with some leaves deleted.
deleted :: Parse' String
deleted = show <$> readTree' "[[1][2[3 4]][5[6][20[10[8 9 10 12 25 27 29 2]12][13 14 15]]]]"

base', deleted' :: Parse Action' String
(base', deleted') = annotateTrees (expDiscount (const 1) 0.1) base deleted

big :: Parse' String
big = show <$> readTree' "[1 [2 [3 4 5 [6 7 8 [9 10 11 [12 13 14 15] 16 17] 18 [19 [20 21 [22 23 24] 25 [26 27 29] 30] 31 32] 33] 34 [35 36 [37 38 39] 40 41 42]] 43 44 45 [46 47 48] 49 50] 51 [52 53 [54 55 56] 57 [58 59 60] 61 62]]"

big' :: Parse' String
big' = show <$> readTree' "[1 [2 [3 4 5 [6 7 8 [9 10 11 [1 2 3 4] 16 17] 18 [64 [20 [22  24] 25 [26 27 29] 30]] 33] 34 [35 36 [37] 42]] 43 45 [46 47 48 49 50] 50 50] 51 [52 53 [54 55 56] 57 62]]"

-- | Diffs and renders a pair of trees side by side for easy comparison.
renderDiffTrees :: (Eq a, Show a) => (a -> Weight) -> Parse' a -> Parse' a -> Diagram SVG
renderDiffTrees weigh input output = trees & translateY 5 & pad 1.1 & bg white
  where trees =  renderDiffTree (input' & traversed %~ show)
             ||| strutX 10
             ||| renderDiffTree (output' & traversed %~ show)
        (input', output') = annotateTrees (expDiscount weigh 0.1) input output

-- | Renders the diff trees for two JavaScript files.
renderFileDiff :: FilePath -> FilePath -> IO (Diagram SVG)
renderFileDiff pathIn pathOut = do in_  <- readFile pathIn
                                   out_ <- readFile pathOut
                                   let treeIn  = parse JS.program pathIn $ Text.pack in_
                                       treeOut = parse JS.program pathIn $ Text.pack out_
                                   case (,) <$> treeIn <*> treeOut of
                                     Left err    -> print err >> return undefined
                                     Right trees -> return $ render trees

  where render = uncurry (renderDiffTrees $ JS.weigh . _value)

-- ** Helper functions

-- | Print a distTable in a reasonably readable format.
printTable :: Array (Int, Int) Double -> IO ()
printTable table = forM_ (rows $ toList table) $ \ row ->
  putStrLn $ intercalate " " $ map (printf "%.2f") row
  where ((_, _), (width', _)) = Array.bounds table

        rows [] = []
        rows ls = take (width' + 1) ls : rows (drop (width' + 1) ls)
