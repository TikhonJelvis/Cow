{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}
module Cow.ParseTree where

import           Control.Monad.State (evalState, get, modify)

import           Data.Array          (Array)
import qualified Data.Array          as Array
import qualified Data.Tree           as Rose

-- | A parse tree that carries the parsed tokens in its leaves, along
-- with arbitrary annotations at each node.
data ParseTree annot leaf = Node annot [ParseTree annot leaf]
                          | Leaf annot leaf
                          deriving (Show, Eq, Functor)

-- | A parse tree where we don't care about the annotations at each
-- node, only the leaves.
type Parse leaf = ParseTree () leaf

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

  -- TODO: Lensify this?
-- | Gets the annotation for the top-level node in the tree.
topAnnot :: ParseTree annot leaf -> annot
topAnnot (Node annot _) = annot
topAnnot (Leaf annot _) = annot

-- | Modifies the annotation for the top-level node in the tree.
modifyTopAnnot :: (annot -> annot) -> ParseTree annot leaf -> ParseTree annot leaf
modifyTopAnnot f (Node annot children) = Node (f annot) children
modifyTopAnnot f (Leaf annot leaf)     = Leaf (f annot) leaf

-- | Sets the annotation for the top-level node in the tree.
setTopAnnot :: annot -> ParseTree annot leaf -> ParseTree annot leaf
setTopAnnot annot = modifyTopAnnot $ const annot

-- | Replaces all the annotations in a tree with ().
noAnnot :: ParseTree annot leaf -> Parse leaf
noAnnot (Node _ children) = Node () $ map noAnnot children
noAnnot (Leaf _ leaf)     = Leaf () leaf

mapAnnot :: (a -> b) -> ParseTree a leaf -> ParseTree b leaf
mapAnnot f (Node annot children) = Node (f annot) $ mapAnnot f <$> children
mapAnnot f (Leaf annot leaf)     = Leaf (f annot) leaf

-- | A preorder traversal of a tree, annotating each node with its
-- position in the traversal, starting with 0.
preorder :: Parse leaf -> ParseTree Int leaf
preorder tree = evalState (go tree) 0
  where go (Node () children) = Node <$> get <*> (modify (+ 1) *> mapM go children)
        go (Leaf () leaf)     = do n <- get; modify (+ 1); return $ Leaf n leaf

                                   -- TODO: Is this actually right?
-- | Compiles the next non-child node for each node in a preorder
-- traversal. (The next non-child node may not exist, but it will
-- still be compiled.)
jumps :: Parse leaf -> ParseTree Int leaf
jumps tree = go $ preorder tree
  where go node@(Node n children) = Node (n + size node) (map go children)
        go (Leaf n leaf)          = Leaf (n + 1) leaf

-- | Compiles an array of each annotation in the tree in a preorder
-- traversal.
preorderTable :: ParseTree annot leaf -> Array Int annot
preorderTable tree = Array.listArray (0, size tree - 1) $ go tree
  where go (Node annot children) = annot : (children >>= go)
        go (Leaf annot _)        = [annot]

data NodeType leaf = Node' | Leaf' leaf deriving (Show, Eq, Functor)

-- | Takes a preorder traversal of a tree and turns it into an array
-- which specifies whether each node was a node or a leaf (with its
-- contents).
nodeTable :: Parse leaf -> Array Int (NodeType leaf)
nodeTable tree = Array.listArray (0, size tree - 1) $ go tree
  where go (Node _ children) = Node' : (children >>= go)
        go (Leaf _ leaf)     = [Leaf' leaf]
