{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE ViewPatterns              #-}
module Cow.ParseTree.Viz where

import           Control.Monad                (foldM)
import           Control.Monad.State          (evalState, get, put)

import           Data.Functor                 ((<$>))
import           Data.Maybe                   (fromMaybe, listToMaybe)
import qualified Data.Tree                    as Rose

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree

import           Cow.ParseTree
import           Cow.ParseTree.Read

-- | Draws a parse tree, discarding annotations. Internal nodes are
-- drawn as dots, while leaves have their text on a circle.
renderParseTree parseTree = renderTree renderNode curve tree
  where tree = clusterLayoutTree parseTree

        -- Render leaves as white circles and nodes as black dots
        renderNode Nothing    = circle 0.2 # fc black
        renderNode (Just str) = text str <> circle 1 # fc white

        -- Connects nodes with a Bézier curve to help avoid
        -- overlapping edges and make connections easy to follow
        curve start@(unp2 -> (x1, y1)) end@(unp2 -> (x2, y2)) =
          fromLocSegments $ [ bezier3 (r2 (0, 0))
                                      (r2 (dx, dy/4))
                                      (r2 (dx, dy))
                            ] `at` p2 (x1, y1)
          where (dx, dy) = (x2 - x1, y2 - y1)

          -- TODO: abstract over this!
nodeSpacing :: (Floating n, Ord n) => n
nodeSpacing = 4

-- | Calculates the y coordinate of each node in a tree, counting up
-- from the leaves which are all at y = 0.
nodeY :: (Floating n, Ord n) => ParseTree annot leaf -> ParseTree (n, annot) leaf
nodeY (Leaf annot leaf)     = Leaf (0, annot) leaf
nodeY (Node annot children) = Node (maximum depths + nodeSpacing, annot) children'
  where children' = nodeY <$> children
        depths    = fst . topAnnot <$> children'

-- | Calculates the x coordinate for each node in a tree. The leaves
-- are all evenly arranged at the bottom of the tree, with each
-- internal node centered *relative to its leaves* (not necessarily
-- its direct sub-nodes).
nodeX :: (Floating n, Ord n) => ParseTree annot leaf -> ParseTree (n, annot) leaf
nodeX tree = offsetAndCenter $ nodeWidth tree
  where -- Calculate the x offset of every node in a tree to center
        -- the root node and move the subtrees relative to each other
        -- as needed.
        offsetAndCenter (Leaf (width, annot) leaf) = Leaf (width / 2, annot) leaf
        offsetAndCenter (Node (width, annot) children) =
          Node (width / 2, annot) $ reverse offsetChildren
          where offsetChildren = evalState (foldM go [] $ offsetAndCenter <$> children) 0
                go children' node = do
                  offset <- get
                  let node' = mapAnnot (\ (n, annot) -> (offset + n, annot)) node
                  put $ offset + 2 * fst (topAnnot node)
                  return $ node' : children'

        -- Calculate how many leaves are under each node (any number
        -- of levels down). Leaves have a width of 'nodeSpacing'.
        nodeWidth (Leaf annot leaf)     = Leaf (nodeSpacing, annot) leaf
        nodeWidth (Node annot children) = Node (sum widths, annot) children'
          where children' = nodeWidth <$> children
                widths    = fst . topAnnot <$> children'

clusterLayoutTree :: (Floating n, Ord n) => ParseTree annot leaf -> Rose.Tree (Maybe leaf, P2 n)
clusterLayoutTree = fmap swap . toRoseTreeAnnot . mapAnnot toP2 . nodeX . nodeY
  where swap (a, b) = (b, a)
        toP2 (width, (depth, _)) = p2 (width, depth)

exampleTree :: ParseTree () String
exampleTree = show <$> readTree' "[[1][2[3 4]][5[6][7[[8 9 10 11 12]][13 14 15]]]]"

exampleTreeDiagram = renderParseTree exampleTree # translateY 10 # pad 1.1 # bg white
