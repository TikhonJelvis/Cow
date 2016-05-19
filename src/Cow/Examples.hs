-- | This modules contains example trees useful for interactively
-- experimenting with the system. Later on, it will also contain
-- example uses of other parts of the framework.
module Cow.Examples where

import           Data.Array         (Array)
import qualified Data.Array         as Array
import           Data.Foldable
import           Data.List          (intercalate)

import           Text.Printf        (printf)

import           Cow.Diff
import           Cow.ParseTree
import           Cow.ParseTree.Read

-- | A reasonably small tree that has multiple levels and is
-- relatively unblaanced.
base :: Parse String
base = show <$> readTree' "[[1][2[3 4]][5[6][7[[8 9 10 11 12]][13 14 15]]]]"

-- | The base tree with some leaves deleted.
deletedLeaves :: Parse String
deletedLeaves = show <$> readTree' "[[1][2[3 4]][5[6][7[[8 9 12]][13 14 15]]]]"

big :: Parse String
big = show <$> readTree' "[1 [2 [3 4 5 [6 7 8 [9 10 11 [12 13 14 15] 16 17] 18 [19 [20 21 [22 23 24] 25 [26 27 29] 30] 31 32] 33] 34 [35 36 [37 38 39] 40 41 42]] 43 44 45 [46 47 48] 49 50] 51 [52 53 [54 55 56] 57 [58 59 60] 61 62]]"

big' :: Parse String
big' = show <$> readTree' "[1 [2 [3 4 5 [6 7 8 [9 10 11 [1 2 3 4] 16 17] 18 [64 [20 [22  24] 25 [26 27 29] 30]] 33] 34 [35 36 [37] 42]] 43 45 [46 47 48 49 50] 50 50] 51 [52 53 [54 55 56] 57 62]]"

-- ** Helper functions

-- | Print a distTable in a reasonably readable format.
printTable :: Array (Int, Int) Double -> IO ()
printTable table = forM_ (rows $ toList table) $ \ row ->
  putStrLn $ intercalate " " $ map (printf "%.2f") row
  where ((_, _), (width', _)) = Array.bounds table

        rows [] = []
        rows ls = take (width' + 1) ls : rows (drop (width' + 1) ls)
