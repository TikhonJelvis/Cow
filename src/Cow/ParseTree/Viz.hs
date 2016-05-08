module Cow.ParseTree.Viz where

import           Data.Tree

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD.Layout.Tree

t1 = Node 'A' [Node 'B' (map lf "CDE"), Node 'F' [Node 'G' (map lf "HIJ")]]
  where lf x = Node x []

tree :: Diagram B
tree = exampleSymmTree <> (square 30 # fc white)

exampleSymmTree :: Diagram B
exampleSymmTree =
  renderTree ((<> circle 1 # fc white) . text . (:[]))
             (~~)
             (symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) t1)
  # centerXY # pad 1.1


