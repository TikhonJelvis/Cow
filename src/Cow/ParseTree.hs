{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MonadComprehensions #-}
module Cow.ParseTree where

import           Control.Monad.State (evalState, get, modify)

import           Data.Array          (Array)
import qualified Data.Array          as Array
import           Data.Foldable
import           Data.Maybe          (listToMaybe, mapMaybe)
import           Data.Traversable
import qualified Data.Tree           as Rose

import           Control.Lens

-- | A parse tree that carries the parsed tokens in its leaves, along
-- with arbitrary annotations at each node.
data ParseTree annot leaf = Node annot [ParseTree annot leaf]
                          | Leaf annot leaf
                          deriving (Show, Eq, Functor)

-- | A parse tree where we don't care about the annotations at each
-- node, only the leaves.
type Parse leaf = ParseTree () leaf

  -- TODO: This is probably woefully inefficient!
instance Foldable (ParseTree annot) where
  foldr f z = foldr f z . toListOf leaves

instance Traversable (ParseTree annot) where
  traverse = traverseOf leaves

-- | A lens which accesses the top-level annotation of a tree, but not
-- the annotations of the children of the node (if any).
topAnnot :: Lens (ParseTree annot leaf) (ParseTree annot leaf) annot annot
topAnnot f (Leaf annot leaf)     = (\ a -> Leaf a leaf) <$> f annot
topAnnot f (Node annot children) = (\ a -> Node a children) <$> f annot

-- | A traversal that targets every annotation in the tree in preorder.
annots :: Traversal (ParseTree annot leaf) (ParseTree annot' leaf) annot annot'
annots f (Leaf annot leaf)     = (\ a -> Leaf a leaf) <$> f annot
annots f (Node annot children) = Node <$> f annot <*> traverse (annots f) children

-- | A traversal that targets every leaf in order.
leaves :: Traversal (ParseTree annot leaf) (ParseTree annot leaf') leaf leaf'
leaves f (Leaf annot leaf)     = Leaf annot <$> f leaf
leaves f (Node annot children) = Node annot <$> traverse (leaves f) children

-- | The size of the tree, counting *all* of the nodes, not just the
-- leaves.
size :: ParseTree annot leaf -> Int
size (Node _ children) = 1 + sum (map size children)
size (Leaf _ _)        = 1

-- | Turns the parse tree into a rose tree, discarding all the
-- annotations. The internal nodes are labeled with 'Nothing' and the
-- leaves with 'Just leaf'.
toRoseTree :: ParseTree annot leaf -> Rose.Tree (Maybe leaf)
toRoseTree (Node _ children) = Rose.Node Nothing $ map toRoseTree children
toRoseTree (Leaf _ leaf)     = Rose.Node (Just leaf) []

-- | Turns the parse tree into a rose tree, keeping the annotations as
-- well as the values at the leaves.
toRoseTreeAnnot :: ParseTree annot leaf -> Rose.Tree (annot, Maybe leaf)
toRoseTreeAnnot (Node annot children) =
  Rose.Node (annot, Nothing) $ map toRoseTreeAnnot children
toRoseTreeAnnot (Leaf annot leaf)     = Rose.Node (annot, Just leaf) []

-- | A preorder traversal of a tree, annotating each node with its
-- position in the traversal, starting with 0.
preorder :: ParseTree annot leaf -> ParseTree Int leaf
preorder = iset (indexing annots) id

-- | A preorder traversal, keeping the old annotations rather than
-- replacing them.
preorder' :: ParseTree annot leaf -> ParseTree (Int, annot) leaf
preorder' = iover (indexing annots) (,)

-- | Gets the subtree rooted at the given index in a preorder
-- traversal.
getSubTree :: Int -> ParseTree annot leaf -> Maybe (ParseTree annot leaf)
getSubTree n = go . preorder'
  where go (Leaf (i, annot) leaf) = [Leaf annot leaf | i == n]
        go (Node (i, annot) children)
          | i == n    = Just $ Node annot $ children <&> annots %~ snd
          | otherwise = listToMaybe $ mapMaybe go children

                                   -- TODO: Is this actually right?
-- | Compiles the next non-child node for each node in a preorder
-- traversal. (The next non-child node may not exist, but it will
-- still be compiled.)
jumps :: ParseTree annot leaf -> ParseTree Int leaf
jumps tree = go $ preorder tree
  where go node@(Node n children) = Node (n + size node) (map go children)
        go (Leaf n leaf)          = Leaf (n + 1) leaf

-- | Compiles an array of each annotation in the tree in a preorder
-- traversal.
preorderTable :: ParseTree annot leaf -> Array Int annot
preorderTable tree = Array.listArray (0, size tree - 1) $ tree ^.. annots

data NodeType leaf = Node' | Leaf' leaf deriving (Show, Eq, Functor)

-- | Takes a preorder traversal of a tree and turns it into an array
-- which specifies whether each node was a node or a leaf (with its
-- contents).
nodeTable :: Parse leaf -> Array Int (NodeType leaf)
nodeTable tree = Array.listArray (0, size tree - 1) $ go tree
  where go (Node _ children) = Node' : (children >>= go)
        go (Leaf _ leaf)     = [Leaf' leaf]
