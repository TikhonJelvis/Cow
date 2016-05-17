{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Cow.ParseTree.Viz where

import           Data.Functor                 ((<$>))
import qualified Data.Tree                    as Rose

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree

import           Cow.ParseTree
import           Cow.ParseTree.Read

-- | Render a node label as a string on a white circle of radius 1.
renderNode :: String -> Diagram B
renderNode str = text str <> circle 1 # fc white

-- | Draws a parse tree, discarding annotations. Internal nodes are
-- drawn as dots, while leaves have their text on a circle.
renderParseTree :: ParseTree annot String -> Diagram B
renderParseTree parseTree = renderTree renderNode curve tree
  where tree = clusterLayoutTree parseTree

        renderNode Nothing    = circle 0.2 # fc black
        renderNode (Just str) = text str <> circle 1 # fc white

        -- connects two points with an angle
        angle (unp2 -> (x1, y1)) (unp2 -> (x2, y2)) =
          fromVertices [p2 (x2, y2), p2 (x', y'), p2 (x1, y1)]
          where x' = x2 - 0.5 * (x2 - x1)
                y' = y1 - 0.4 * (y1 - y2)

        -- connects two points with a Bezier curve
        curve start@(unp2 -> (x1, y1)) end@(unp2 -> (x2, y2)) =
          fromLocSegments ([ bezier3 (r2 (0, 0)) (r2 (dx, dy/4)) (r2 (dx, dy))
                           ] `at` p2 (x1, y1))
          where (dx, dy) = (x2 - x1, y2 - y1)

          -- TODO: abstract over this!
nodeSpacing :: (Floating n, Ord n) => n
nodeSpacing = 4

-- | Calculates the maximum number of levels between each node and a
-- leaf. The depth for leaves is 0. This is how far above the baseline
-- the node should appear.
nodeY :: (Floating n, Ord n) => ParseTree annot leaf -> ParseTree (n, annot) leaf
nodeY (Leaf annot leaf)     = Leaf (0, annot) leaf
nodeY (Node annot children) = Node (maximum depths + nodeSpacing, annot) children'
  where children' = nodeY <$> children
        depths    = map (fst . topAnnot) children'

-- | Calculates the x coordinate for each node in a tree. The root of
-- each subtree is centered relative to its *leaves*, with each
-- subtree offset a bit from its neighbor.
nodeX :: (Floating n, Ord n) => ParseTree annot leaf -> ParseTree (n, annot) leaf
nodeX tree = offsetAndCenter $ nodeWidth tree
  where -- Calculate the x offset of every node in a tree to center
        -- the root node and move the subtrees relative to each other
        -- as needed.
        offsetAndCenter (Leaf (width, annot) leaf) = Leaf (width / 2, annot) leaf
        offsetAndCenter (Node (width, annot) children) =
          Node (width / 2, annot) $ reverse offsetChildren
          where (_, offsetChildren) = foldl go (0, []) $ map offsetAndCenter children
                go (offset, children') (Leaf (nodeOffset, annot) leaf) =
                  (offset + nodeOffset * 2, leaf' : children')
                  where leaf' = Leaf (offset + nodeOffset, annot) leaf
                go (offset, children') (Node (nodeOffset, annot) children) =
                  (offset + nodeOffset * 2, node' : children')
                  where node' = Node (offset + nodeOffset, annot) offsetChildren
                        offsetChildren = mapAnnot addOffset <$> children
                        addOffset (oldOffset, annot) = (offset + oldOffset, annot)

        -- Calculate how many leaves are under each node (any number
        -- of levels down). Leaves have a width of 1.
        nodeWidth :: (Floating n, Ord n) => ParseTree annot leaf -> ParseTree (n, annot) leaf
        nodeWidth (Leaf annot leaf)     = Leaf (nodeSpacing, annot) leaf
        nodeWidth (Node annot children) = Node (sum widths, annot) children'
          where children' = nodeWidth <$> children
                widths    = map (fst . topAnnot) children'

clusterLayoutTree :: (Floating n, Ord n) => ParseTree annot leaf -> Rose.Tree (Maybe leaf, P2 n)
clusterLayoutTree = fmap swap . toRoseTreeAnnot . mapAnnot toP2 . nodeX . nodeY
  where swap (a, b) = (b, a)
        toP2 (width, (depth, _)) = p2 (width, depth)

exampleTree :: ParseTree () String
exampleTree = show <$> readTree' "[[1][2[3 4]][5[6[7 8 9][10 11]][12]]]"

exampleTreeDiagram :: Diagram B
exampleTreeDiagram = renderParseTree exampleTree <> square 50 # fc white
