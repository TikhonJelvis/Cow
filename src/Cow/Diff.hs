module Cow.Diff (diff, weighDiff) where

import Data.Functor            ((<$>))
import Data.List.Extras.Argmax (argmin)

import Cow.Type

diff :: Eq a => AST a -> AST a -> Diff a
diff (Node left lchildren) (Node right rchildren)
  | left /= right = Node (Mod left right) $ diffForest lchildren rchildren
  | otherwise    = Node (Non left) $ diffForest lchildren rchildren

diffForest :: Eq a => [AST a] -> [AST a] -> [Diff a]
diffForest [] [] = []
diffForest ls [] = map (Del <$>) ls
diffForest [] ls = map (Ins <$>) ls
diffForest (l@(Node lRoot lChildren):ls) (r@(Node rRoot rChildren):rs) =
  argmin (weighDiff α <$>) [modified, removed, added]
  where α = 0.1
        removed                  = (Del <$> l) : diffForest ls (r:rs)
        added                    = (Ins <$> r) : diffForest (l:ls) rs
        modified | lRoot == rRoot = Node (Non lRoot) childrenDiff : rest
                 | otherwise     = Node (Mod lRoot rRoot) childrenDiff : rest
        childrenDiff             = diffForest lChildren rChildren
        rest                     = diffForest ls rs
            
weighDiff :: Double -> Diff a -> Double
weighDiff α (Node (Ins _) _)    = α
weighDiff α (Node (Del _) _)    = α
weighDiff α (Node value children) = weight value + α * (sum $ weighDiff α <$> children)
  where weight Non{} = 0
        weight Mod{} = 0.1 * α
        weight _     = α