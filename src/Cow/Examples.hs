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


-- ** Helper functions

-- | Print a distTable in a reasonably readable format.
printTable :: Array (Int, Int) Double -> IO ()
printTable table = forM_ (rows $ toList table) $ \ row ->
  putStrLn $ intercalate " " $ map (printf "%.2f") row
  where ((_, _), (width', _)) = Array.bounds table

        rows [] = []
        rows ls = take (width' + 1) ls : rows (drop (width' + 1) ls)
