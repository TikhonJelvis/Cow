{-# LANGUAGE TemplateHaskell #-}
module Cow.Diff where

import           Control.Lens

import           Data.Array           (Array, (!))
import qualified Data.Array           as Array
import           Data.Generics.Labels ()
import           Data.List            (minimumBy)
import           Data.Maybe           (mapMaybe)
import           Data.Ord             (comparing)

import           GHC.Generics         (Generic)

import           Cow.ParseTree        (NodeType (..), Parse (..), Parse')
import qualified Cow.ParseTree        as Tree

type Weight = Double

-- | A function which assigns every subtree a weight.
type Weigh leaf = Parse' leaf → Parse Weight leaf

  -- TODO: Figure out how this function behaves and whether it
  -- actually makes any sense!
-- | Calculate the weight of a tree exponentially discounting each
-- additional level by a factor of @α@.
expDiscount ∷ (leaf → Weight)
            -- ^ A function that assigns weights to tokens at the
            -- leaves of the tree.
            → Weight
            -- ^ α, the factor by which to discount each level. This
            -- is nested, so we discount by 1 then α then α²… etc.
            → Weigh leaf
expDiscount ℓ _ (Leaf _ leaf)     = Leaf (ℓ leaf) leaf
expDiscount ℓ α (Node _ children) = Node (1 + α * sumOf (each . Tree.annots) result) result
  where result = expDiscount ℓ α <$> children

          -- TODO: Clarify and verify!
-- | The weight of a node is α% of its possible range.
--
-- The minimum weight of a node is the largest weight of any one child
-- (otherwise we do not get a valid distance metric). The maximum for
-- the heuristic to be useful is the sum of all its children
-- (otherwise there would be cases where we would remove the elements
-- of a subtree individually rather than removing the whole subtree in
-- a single action).
--
-- This heuristic is equal to @minimum + α * (maximum - minimum)@,
-- where:
--
--  * @minimum@ is the *largest* weight of a single child
--  * @maximum@ is the *sum* of the weights of all the children
percentage ∷ (leaf → Weight)
           -- ^ A function that assigns weights to tokens at the
           -- leaves of the tree.
           → Weight
           -- ^ The factor by which we discount the *range* of the
           -- tree, where the range is difference between the minimum
           -- is the *largest* weight of a single child and the
           -- maximum is the sum of the weights of all the children.
           → Weigh leaf
percentage ℓ _ (Leaf _ leaf)     = Leaf (ℓ leaf) leaf
percentage ℓ α (Node _ children) = Node (minWeight + α * weightDifference) children'
  where -- minimum is safe here because children' is NonEmpty
        minWeight = maximum $ children' ^.. each . Tree.topAnnot
        maxWeight = sumOf (each . Tree.topAnnot) children'

        weightDifference = maxWeight - minWeight

        children' = percentage ℓ α <$> children

-- | A few arrays representing the structure of a tree for fast
-- indexing.
data Tables leaf = Tables
    { jumps   :: Array Int Int
    -- | The contents of the node itself, indexed in preorder.
    , nodes   :: Array Int (NodeType leaf)
    -- | The weight of each node—how much removing it should cost.
    , weights :: Array Int Weight
    } deriving (Show, Eq, Generic)

-- | Given a tree, creates arrays that specify its structure in a way
-- that's fast to index.
tables ∷ Weigh leaf → Parse' leaf → Tables leaf
tables weigh tree = Tables
  { jumps   = Tree.preorderTable $ Tree.jumps tree
  , nodes   = Tree.nodeTable tree
  , weights = Tree.preorderTable $ weigh tree
  }

-- | Specifies what to do with a preorder index into the tree. If a
-- subtree is added or removed, all of its children are also added or
-- removed.
--
-- Note how the problem is *symmetric*: removing corresponds to
-- removing a node from the input tree and adding corresponds to
-- removing a node in the output tree.
data Action = Add    { loc :: Int }
            | Remove { loc :: Int }
    deriving (Show, Eq, Generic)

makePrisms ''Action

-- | An edit script is all the actions needed to go from the input
-- tree to the output tree.
type Script = [Action]

-- | This data type contains both a numeric distance *and* a (lazy)
-- edit script that led to it.
data Dist = Dist
    { dist  :: Double
    , edits :: Script
    }
    deriving (Show, Eq, Generic)

type DistTable = Array (Int, Int) Dist

-- | Produce the dynamic programming array for comparing two parse
-- trees using our modified string edit distance algorithm.
distTable ∷ Eq a ⇒ (Parse' a → Parse Weight a) → Parse' a → Parse' a → DistTable
distTable weigh input output = ds
  where endIn  = Tree.size input
        endOut = Tree.size output

        bounds = ((0, 0), (endIn, endOut))

        (tableIn, tableOut) = (tables weigh input, tables weigh output)

        ds = Array.listArray bounds [d i o | (i, o) <- Array.range bounds]

        -- Traverses the parts of the structure at indices [a-b).
        between a b = traversed . indices (`elem` [a..b - 1])

        d i o
          -- done with *both* strings
          | i == endIn && o == endOut =
            Dist 0 []

          -- done with input string—keep remaining output
          | i == endIn  =
            Dist (sumOf (#weights . between o endOut) tableOut) [Add o]

          -- done with output string—keep remaining input
          | o == endOut =
            Dist (sumOf (#weights . between i endIn) tableIn)   [Remove i]

          -- not done with either string
          | otherwise = case (in_, out) of
              -- identical tokens: keep token, move on
              (TypeLeaf in_, TypeLeaf out)
                | in_ == out ->
                    ds ! (i + 1, o + 1)
              -- different tokens and/or nodes: find cheapest possible
              -- action (see list of possible actions later on)
              _ ->
                minimumBy (comparing $ view #dist) $ inputs <> outputs

          where in_ = (tableIn ^. #nodes) ! i
                out = (tableOut ^. #nodes) ! o

                -- available actions for nodes and tokens:
                --  * leaf
                --    * add/remove leaf (remove if on input, add
                --      if on output)
                --  * node
                --    * add/remove whole subtree (remove if on
                --      input, add if on output)
                --    * move *into* subtree (potentially modifying
                --      *parts* of the subtree)

                -- if we add/remove a whole subtree, we need to jump
                -- to the first index *after* the subtree, which is
                -- stored in the #jumps table

                inputs = case in_ of
                  TypeLeaf{} ->
                    [ ds ! (i + 1, o) & #dist +~ inputWeight
                                      & #edits %~ (Remove i :)
                    ]
                  TypeNode   ->
                    [ ds ! (i + 1, o)
                    , let next = (tableIn ^. #jumps) ! i
                      in ds ! (next, o) & #dist +~ inputWeight
                                        & #edits %~ (Remove i :)
                    ]
                inputWeight = (tableIn ^. #weights) ! i

                outputs = case out of
                  TypeLeaf{} ->
                    [ ds ! (i, o + 1) & #dist +~ outputWeight
                                      & #edits %~ (Add o :)
                    ]
                  TypeNode   ->
                    [ ds ! (i, o + 1)
                    , let next = (tableOut ^. #jumps) ! o
                      in ds ! (i, next) & #dist +~ outputWeight
                                        & #edits %~ (Add o :)
                    ]
                outputWeight = (tableOut ^. #weights) ! o

-- | Calculate the distance between two trees using our metric.
diff ∷ Eq a ⇒ Weigh a → Parse' a → Parse' a → Dist
diff weigh left right = distTable weigh left right ! (0, 0)

-- | Annotates a node in a tree with whether it was added, removed or
-- left unchanged by an edit script.
data Action' = Add'
    | Remove'
    | None'
    deriving (Show, Eq)

-- | Compares two trees and returns all the added and removed
-- subtrees.
toSubTrees ∷ Eq a ⇒ Weigh a → Parse' a → Parse' a → [Parse Action' a]
toSubTrees weigh input output = mapMaybe go actions
  where actions = diff weigh input output ^. #edits

        go (Remove i) = Tree.getSubTree i input  <&> Tree.annots .~ Remove'
        go (Add o)    = Tree.getSubTree o output <&> Tree.annots .~ Add'

annotateTrees ∷ forall a. Eq a ⇒ Weigh a → Parse' a → Parse' a →
                 (Parse Action' a, Parse Action' a)
annotateTrees weigh input output = (input', output')
  where input'  = go Remove' removed $ Tree.preorder input
        output' = go Add' added $ Tree.preorder output

        go to targets (Leaf n leaf)
          | n `elem` targets = Leaf to leaf
          | otherwise        = Leaf None' leaf
        go to targets node@(Node n children)
          | n `elem` targets = node & Tree.annots .~ to
          | otherwise        = Node None' $ go to targets <$> children

        actions = diff weigh input output ^. #edits
        removed = actions ^.. each . _Remove
        added   = actions ^.. each . _Add
