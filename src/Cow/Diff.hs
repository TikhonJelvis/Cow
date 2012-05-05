module Cow.Diff (diff) where

import Data.Functor            ((<$>))
import Data.List.Extras.Argmax (argmin)

import Cow.Type

diff :: Eq a => AST a -> AST a -> Diff a
diff left right = head $ diffForest [left] [right]

diffForest :: Eq a => [AST a] -> [AST a] -> [Diff a]
diffForest [] []          = []
diffForest ls []          = map (Del <$>) ls
diffForest [] ls          = map (Ins <$>) ls
diffForest (l@(Node lRoot lChildren):ls) (r@(Node rRoot rChildren):rs) =
  argmin (weigh 0.9 <$>) [removed, added, modified]
  where removed              = (Del <$> l) : diffForest ls (r:rs)
        added                = (Ins <$> r) : diffForest (l:ls) rs
        modified | lRoot     == rRoot = Node (Non lRoot) childrenDiff : rest
                 | otherwise = Node (Mod lRoot rRoot) childrenDiff : rest
        childrenDiff         = diffForest lChildren rChildren
        rest                 = diffForest ls rs
            
weigh :: Double -> Diff a -> Double
weigh α (Node val children) = weight val + sum (weigh (α**2) <$> children)
  where weight (Non _) = 0
        weight _       = α