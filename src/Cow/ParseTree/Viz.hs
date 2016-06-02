{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
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

import           Cow.Diff
import           Cow.ParseTree
import           Cow.ParseTree.Read

-- | Render a tree with functions to control edge colors (based on the
-- the nodes they're connecting) and node shapes.
renderAnnotTree edgeColor renderNode parseTree = renderTree' renderNode renderEdge tree
  where tree = clusterLayoutTree parseTree
        renderEdge (annot1, p1) (annot2, p2) = curve p1 p2 # lc (edgeColor annot1 annot2) # lw 5

        -- Connects nodes with a Bézier curve to help avoid
        -- overlapping edges and make connections easy to follow
        curve start@(unp2 -> (x1, y1)) end@(unp2 -> (x2, y2)) =
          fromLocSegments $ [ bezier3 (r2 (0, 0))
                                      (r2 (dx, dy/4))
                                      (r2 (dx, dy))
                            ] `at` p2 (x1, y1)
          where (dx, dy) = (x2 - x1, y2 - y1)

-- | Renders trees annotated with add/remove/unchanged actions. If an
-- internal is added or removed, all its children and edges are
-- colored.
renderDiffTree = renderAnnotTree edgeColor $ \case
  (action, Nothing)  -> circle 0.5 # fc (colorOf action) # lw 5
  (action, Just str) -> (strutY 0.5 === text str) <> rect (w str) 1 # fc (bgOf action)
                                                                    # lc (bgOf action)
  where colorOf Add'    = green
        colorOf Remove' = red
        colorOf None'   = black

        bgOf action = blend 0.6 (colorOf action) white

        w label = fromIntegral (length label) * charWidth + nodeSpacing - 1

        edgeColor (action, _)   _  = colorOf action

-- | Renders a parse tree ignoring its annotations.
renderParseTree = renderAnnotTree (\ a b -> black) renderNode
  where -- Render leaves as white circles and nodes as black dots
        renderNode (_, Nothing) = circle 0.2 # fc black
        renderNode (_, Just str) = text str <> rect (w str) 1 # fc white

        w label = fromIntegral (length label) * charWidth + nodeSpacing - 1

          -- TODO: abstract over this!
nodeSpacing, charWidth :: (Floating n, Ord n) => n
nodeSpacing = 4
charWidth   = 0.25

-- | Calculates the y coordinate of each node in a tree, counting up
-- from the leaves which are all at y = 0.
nodeY :: (Floating n, Ord n) => ParseTree annot leaf -> ParseTree (n, annot) leaf
nodeY (Leaf annot leaf)     = Leaf (0, annot) leaf
nodeY (Node annot children) = Node (maximum depths + nodeSpacing, annot) children'
  where children' = nodeY <$> children
        depths    = children' ^.. each . topAnnot . _1

-- | Calculates the x coordinate for each node in a tree. The leaves
-- are all evenly arranged at the bottom of the tree, with each
-- internal node centered *relative to its leaves* (not necessarily
-- its direct sub-nodes).
nodeX :: (Floating n, Ord n) => ParseTree annot String -> ParseTree (n, annot) String
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
                  let node' = node & annots . _1 +~ offset
                  put $ offset + 2 * (node ^. topAnnot . _1)
                  return $ node' : children'

        -- Calculate how many leaves are under each node (any number
        -- of levels down). Leaves have a width that depends on the
        -- length of their label, with 'nodeSpacing' padding.
        nodeWidth (Leaf annot leaf)     = Leaf (nodeSpacing + textWidth leaf, annot) leaf
          where textWidth str = fromIntegral (length str) * charWidth
        nodeWidth (Node annot children) = Node (sum widths, annot) children'
          where children' = nodeWidth <$> children
                widths    = children' ^.. each . topAnnot . _1

-- | Lays a whole tree out with everything aligned from the leaves up.
clusterLayoutTree :: (Floating n, Ord n) => ParseTree annot String ->
                                            Rose.Tree ((annot, Maybe String), P2 n)
clusterLayoutTree = fmap go . toRoseTreeAnnot . (annots %~ toP2) . nodeX . nodeY
  where go ((pos, annot), leaf) = ((annot, leaf), pos)
        toP2 (width, (depth, annot)) = (p2 (width, depth), annot)

exampleTree :: ParseTree () String
exampleTree = show <$> readTree' "[[1][2[3 4]][5[6][7[[8 9 10 11 12]][13 14 15]]]]"

exampleTreeDiagram = renderParseTree exampleTree # translateY 10 # pad 1.1 # bg white
