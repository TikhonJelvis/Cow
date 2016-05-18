{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE TemplateHaskell #-}
module Cow.Diff where

import           Control.Lens

import           Data.Array    (Array, (!))
import qualified Data.Array    as Array
import           Data.List     (minimumBy)
import           Data.Ord      (comparing)

import           Cow.ParseTree (NodeType (..), Parse, ParseTree (..))
import qualified Cow.ParseTree as Tree

type Weight = Double

  -- TODO: Figure out how this function behaves and whether
  -- exponential actually makes sense!
-- | Calculate the weight of a tree exponentially discounting each
-- additional level.
weigh :: Weight -> Parse leaf -> ParseTree Weight leaf
weigh α (Leaf _ leaf)     = Leaf α leaf
weigh α (Node _ children) = Node (sumOf (each . Tree.annots) result) result
  where result = (Tree.annots *~ α) . weigh α <$> children

α :: Weight
α = 1.1

-- | A few arrays representing the structure of a tree for fast
-- indexing.
data Tables leaf = Tables {
  -- | The next non-child node in a preorder traversal.
  jumps   :: Array Int Int,
  -- | The contents of the node itself, indexed in preorder.
  nodes   :: Array Int (NodeType leaf),
  -- | The weight of each node—how much removing it should cost.
  weights :: Array Int Weight
  }

-- | Given a tree, creates arrays that specify its structure in a way
-- that's fast to index.
tables :: Parse leaf -> Tables leaf
tables tree = Tables { jumps   = Tree.preorderTable $ Tree.jumps tree
                     , nodes   = Tree.nodeTable tree
                     , weights = Tree.preorderTable $ weigh α tree
                     }

-- | Specifies what to do with ranges of nodes in the tree, given as
-- an inclusive range of indices in preorder.
--
-- Note how the problem is *symmetric*: removing corresponds to
-- removing a node from the input tree and adding corresponds to
-- removing a node in the output tree.
data Action = Add Int Int | Remove Int Int deriving (Show, Eq)

-- | This data type contains both a numeric distance *and* the edit
-- action it corresponds to.
data Dist = Dist { _dist  :: Double
                 , _edits :: [Action]
                 }

makeLenses ''Dist

type DistTable = Array (Int, Int) Dist

-- | Produce the dynamic programming array for comparing two parse
-- trees using our modified string edit distance algorithm.
distTable :: Eq a => Parse a -> Parse a -> DistTable
distTable input output = ds
  where (endIn, endOut) = (Tree.size input, Tree.size output)
        bounds = ((0, 0), (endIn, endOut))

        (tableIn, tableOut) = (tables input, tables output)

        ds = Array.listArray bounds [d i o | (i, o) <- Array.range bounds]

          -- TODO: Fix base cases and verify "fixed" indexing.
        d i o | i == endIn && o == endOut = Dist 0 []
        d i o | i == endIn  = Dist weight [Add o (endOut - 1)]
          where weight = sum [weights tableOut ! x | x <- [o..(endOut - 1)]]
        d i o | o == endOut = Dist weight [Remove i (endIn - 1)]
          where weight = sum [weights tableIn ! x | x <- [i..(endIn - 1)]]
        d i o = case (in_, out) of
          (Leaf' in_, Leaf' out) | in_ == out ->
            ds ! (i + 1, o + 1)
          _                                   ->
            minimumBy (comparing _dist) $ inputs ++ outputs
          where (in_, out) = (nodes tableIn ! i, nodes tableOut ! o)
                inputs | Leaf'{} <- in_ =
                         [ ds ! (i + 1, o) & dist +~ weights tableIn ! i
                                           & edits %~ (Remove i i :)
                         ]
                       | Node'   <- in_ =
                         [ ds ! (i + 1, o)
                         , let next = jumps tableIn ! i in
                           ds ! (next, o) & dist +~ weights tableIn ! i
                                          & edits %~ (Remove i (next - 1) :)
                         ]
                outputs | Leaf'{} <- out =
                          [ ds ! (i, o + 1) & dist +~ weights tableOut ! o
                                            & edits %~ (Add o o :)
                          ]
                        | Node'   <- out =
                          [ ds ! (i, o + 1)
                          , let next = jumps tableOut ! o in
                            ds ! (i, next) & dist +~ weights tableOut ! o
                                           & edits %~ (Add o (next - 1) :)
                          ]

-- | Calculate the distance between two trees using our metric.
diff :: Eq a => Parse a -> Parse a -> Dist
diff left right = ds ! (0, 0)
  where ds = distTable left right
