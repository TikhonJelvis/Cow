{-# LANGUAGE PatternGuards #-}
module Cow.Diff where

import           Data.Array    (Array, (!))
import qualified Data.Array    as Array

import           Cow.ParseTree (Parse, ParseTree (..), NodeType (..))
import qualified Cow.ParseTree as Tree

type Weight = Double

  -- TODO: Figure out how this function behaves and whether
  -- exponential actually makes sense!
-- | Calculate the weight of a tree exponentially discounting each
-- additional level.
weigh :: Weight -> Parse leaf -> ParseTree Weight leaf
weigh α (Leaf _ leaf)     = Leaf α leaf
weigh α (Node _ children) = Node (sum $ Tree.topAnnot <$> result) result
  where result = Tree.mapAnnot (α *) . weigh α <$> children

α :: Weight
α = 0.1

-- | A few arrays representing the structure of a tree for fast
-- indexing.
data Tables leaf = Tables {
  -- | The next non-child node in a preorder traversal.
  jumps :: Array Int Int,
  -- | The contents of the node itself, indexed in preorder.
  nodes :: Array Int (NodeType leaf),
  -- | The weight of each node—how much removing it should cost.
  weights :: Array Int Weight
  }

tables :: Parse leaf -> Tables leaf
tables tree = Tables { jumps   = Tree.preorderTable $ Tree.jumps tree
                     , nodes   = Tree.nodeTable tree
                     , weights = Tree.preorderTable $ weigh α tree
                     }

-- | Calculate the distance between two trees using our metric.
dist :: Eq a => Parse a -> Parse a -> Double
dist left right = ds ! (endL, endR)
  where ds = distTable left right
        (endL, endR) = (Tree.size left, Tree.size right)

-- | Produce the dynamic programming array for comparing two parse
-- trees using our modified string edit distance algorithm.
distTable :: Eq a => Parse a -> Parse a -> Array (Int, Int) Double
distTable left right = ds
  where (endL, endR) = (Tree.size left, Tree.size right)
        bounds = ((0, 0), (endL, endR))

        (tableL, tableR) = (tables left, tables right)

        ds = Array.listArray bounds [d i j | (i, j) <- Array.range bounds]


          -- TODO: Verify your "fixed" indexing actually works!
        d i j | i == endL = sum [weights tableR ! x | x <- [i..endR]]
        d i j | j == endR = sum [weights tableL ! x | x <- [j..endL]]
        d i j = case (l, r) of
          (Leaf' l, Leaf' r) | l == r -> ds ! (i + 1, j + 1)
          _                           -> minimum $ lefts ++ rights
          where (l, r) = (nodes tableL ! i, nodes tableR ! j)
                lefts | Leaf'{} <- l = [ ds ! (i + 1, j) + weights tableL ! i ]
                      | Node'   <- l = [ ds ! (i + 1, j)
                                       , ds ! (jumps tableL ! i, j) + weights tableL ! i
                                       ]
                rights | Leaf'{} <- r = [ ds ! (i, j + 1) + weights tableR ! j]
                       | Node'   <- r = [ ds ! (i, j + 1)
                                        , ds ! (i, jumps tableR ! j) + weights tableR ! j
                                        ]

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
