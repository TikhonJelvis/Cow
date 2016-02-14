module Cow.Diff where

import           Data.Array    (Array, (!))
import qualified Data.Array    as Array

import           Cow.ParseTree (Parse, ParseTree (..))
import qualified Cow.ParseTree as Tree

-- | Produce the dynamic programming array for comparing two parse
-- trees using our modified string edit distance algorithm.
diffTable :: Eq a => Parse a -> Parse a -> Array (Int, Int) Int
diffTable left right = Array.listArray bounds [d i j | (i, j) <- Array.range bounds]
  where bounds = ((0, 0), (Tree.size left, Tree.size right))

        (jumpsL, jumpsR) = (Tree.jumps left, Tree.jumps right)
        (nodesL, nodesR) = (Tree.nodeTable left, Tree.nodeTable right)

        d i 0 = i
        d 0 j = j
        d i j = _
          -- Idea: Check if leaf or node. For leaves, treat as if
          -- normal string edit distance. For nodes, always try *both*
          -- keeping it unchanged (and stepping into its children) and
          -- removing it (and skipping to its next node).

          -- I think that this, combined with a heuristic weight
          -- function, should get the behavior I need. In particular:
          --  * if a node is unchanged, the minimum should always end up
          --    with not deleting it
          --  * if a node needs to be changed, one of the two cases may be
          --    the minimum one to choose

          -- TODO: What to do if I'm comparing a node to a leaf? Is
          -- preorder really a good way to do this? Maybe some sort of
          -- level-by-level comparison?

          -- Thought: the problem is that the two trees might get
          -- offset compared to each other in small
          -- ways.

          -- Counterpoint: doesn't exactly this happen with the normal
          -- edit distance algorithm if the strings are offset?  Does
          -- adding an extra dimension change this?
