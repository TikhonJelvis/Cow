{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Cow.Diff where

import           Control.Lens
import           Control.Monad.State (evalState)

import           Data.Array          (Array, (!))
import qualified Data.Array          as Array
import           Data.List           (minimumBy)
import           Data.Maybe          (mapMaybe)
import           Data.Ord            (comparing)

import           Cow.ParseTree       (NodeType (..), Parse, ParseTree (..))
import qualified Cow.ParseTree       as Tree

type Weight = Double

-- | A function which assigns every subtree a weight.
type Weigh leaf = Parse leaf -> ParseTree Weight leaf

  -- TODO: Figure out how this function behaves and whether it
  -- actually makes any sense!
-- | Calculate the weight of a tree exponentially discounting each
-- additional level.
expDiscount :: Weight -> Weigh leaf
expDiscount α (Leaf _ leaf)     = Leaf 1 leaf
expDiscount α (Node _ children) = Node (1 + α * sumOf (each . Tree.annots) result) result
  where result = expDiscount α <$> children

          -- TODO: Clarify and verify!
-- | The weight of a node is n% of its possible range.
--
-- The minimum weight of a node is the largest(?) weight of any one
-- child (otherwise we do not get a valid distance metric). The
-- maximum for the heuristic to be useful is the sum of all its
-- children.
--
-- This heuristic is equal to minimum + n * (maximum - minimum).
--
-- Leaves have weight 1 by fiat.
percentage :: forall leaf. Weight -> Weigh leaf
percentage n (Leaf _ leaf)     = Leaf 1 leaf
percentage n (Node _ children) = Node (min + n * (max - min)) children'
  where -- TODO: make children NonEmpty?
        Just min = minimumOf (each . Tree.topAnnot) children'
        max = sumOf (each . Tree.topAnnot) children'

        children' :: [ParseTree Weight leaf]
        children' = percentage n <$> children

-- | A few arrays representing the structure of a tree for fast
-- indexing.
data Tables leaf = Tables {
  -- | The next non-child node in a preorder traversal.
  _jumps   :: Array Int Int,
  -- | The contents of the node itself, indexed in preorder.
  _nodes   :: Array Int (NodeType leaf),
  -- | The weight of each node—how much removing it should cost.
  _weights :: Array Int Weight
  }

makeLenses ''Tables

-- | Given a tree, creates arrays that specify its structure in a way
-- that's fast to index.
tables :: Weigh leaf -> Parse leaf -> Tables leaf
tables weigh tree = Tables { _jumps   = Tree.preorderTable $ Tree.jumps tree
                           , _nodes   = Tree.nodeTable tree
                           , _weights = Tree.preorderTable $ weigh tree
                           }

-- | Specifies what to do with a preorder index into the tree. If a
-- subtree is added or removed, all of its children are also added or
-- removed.
--
-- Note how the problem is *symmetric*: removing corresponds to
-- removing a node from the input tree and adding corresponds to
-- removing a node in the output tree.
data Action = Add    { _loc :: Int }
            | Remove { _loc :: Int } deriving (Show, Eq)

makeLenses ''Action

makePrisms ''Action

-- | An edit script is all the actions needed to go from the input
-- tree to the output tree.
type Script = [Action]

-- | This data type contains both a numeric distance *and* a (lazy)
-- edit script that led to it.
data Dist = Dist { _dist  :: Double
                 , _edits :: Script
                 } deriving (Show, Eq)

makeLenses ''Dist

type DistTable = Array (Int, Int) Dist

-- | Produce the dynamic programming array for comparing two parse
-- trees using our modified string edit distance algorithm.
distTable :: Eq a => (Parse a -> ParseTree Weight a) -> Parse a -> Parse a -> DistTable
distTable weigh input output = ds
  where (endIn, endOut) = (Tree.size input, Tree.size output)
        bounds = ((0, 0), (endIn, endOut))

        (tableIn, tableOut) = (tables weigh input, tables weigh output)

        ds = Array.listArray bounds [d i o | (i, o) <- Array.range bounds]

        -- Traverses the parts of the structure at indices [a-b).
        between a b = traversed . indices (`elem` [a..b - 1])

        d i o | i == endIn && o == endOut = Dist 0 []
        d i o | i == endIn  = Dist (sumOf (weights . between o endOut) tableOut) [Add o]
        d i o | o == endOut = Dist (sumOf (weights . between i endIn) tableIn)   [Remove i]
        d i o = case (in_, out) of
          (TypeLeaf in_, TypeLeaf out) | in_ == out ->
            ds ! (i + 1, o + 1)
          _                                   ->
            minimumBy (comparing _dist) $ inputs ++ outputs
          where (in_, out) = (_nodes tableIn ! i, _nodes tableOut ! o)
                inputs | TypeLeaf{} <- in_ =
                         [ ds ! (i + 1, o) & dist +~ _weights tableIn ! i
                                           & edits %~ (Remove i :)
                         ]
                       | TypeNode   <- in_ =
                         [ ds ! (i + 1, o)
                         , let next = _jumps tableIn ! i in
                           ds ! (next, o) & dist +~ _weights tableIn ! i
                                          & edits %~ (Remove i :)
                         ]
                outputs | TypeLeaf{} <- out =
                          [ ds ! (i, o + 1) & dist +~ _weights tableOut ! o
                                            & edits %~ (Add o :)
                          ]
                        | TypeNode   <- out =
                          [ ds ! (i, o + 1)
                          , let next = _jumps tableOut ! o in
                            ds ! (i, next) & dist +~ _weights tableOut ! o
                                           & edits %~ (Add o :)
                          ]

-- | Calculate the distance between two trees using our metric.
diff :: Eq a => Weigh a -> Parse a -> Parse a -> Dist
diff weigh left right = distTable weigh left right ! (0, 0)

-- | Annotates a node in a tree with whether it was added, removed or
-- left unchanged by an edit script.
data Action' = Add' | Remove' | None' deriving (Show, Eq)

-- | Compares two trees and returns all the added and removed
-- subtrees.
toSubTrees :: Eq a => Weigh a -> Parse a -> Parse a -> [ParseTree Action' a]
toSubTrees weigh input output = mapMaybe go actions
  where actions = diff weigh input output ^. edits

        go (Remove i) = Tree.getSubTree i input  <&> Tree.annots .~ Remove'
        go (Add o)    = Tree.getSubTree o output <&> Tree.annots .~ Add'

annotateTrees :: forall a. Eq a => Weigh a -> Parse a -> Parse a ->
                 (ParseTree Action' a, ParseTree Action' a)
annotateTrees weigh input output = (input', output')
  where input'  = go Remove' removed $ Tree.preorder input
        output' = go Add' added $ Tree.preorder output

        go to targets (Leaf n leaf)
          | n `elem` targets = Leaf to leaf
          | otherwise        = Leaf None' leaf
        go to targets node@(Node n children)
          | n `elem` targets = node & Tree.annots .~ to
          | otherwise        = Node None' $ go to targets <$> children

        actions = diff weigh input output ^. edits
        removed = actions ^.. each . _Remove
        added   = actions ^.. each . _Add
