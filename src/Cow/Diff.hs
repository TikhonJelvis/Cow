module Cow.Diff  where

import Data.Array
import Data.Functor            ((<$>))
import Data.List               (foldl')
import Data.List.Extras.Argmax (argmin)

import Debug.Trace

import Cow.Type

α :: Double
α = 0.1 

diff :: Eq a => AST a -> AST a -> Diff a
diff l@(Node left lchildren) r@(Node right rchildren)
  | left /= right = Node (Mod left right) $ result ! (0, 0)
  | otherwise    = Node (Non left) $ result ! (0, 0)
  where result = diffed l r

diff' :: Eq a => AST a -> AST a -> Diff a
diff' (Node left lchildren) (Node right rchildren)
  | left /= right = Node (Mod left right) $ diffForest lchildren rchildren
  | otherwise    = Node (Non left) $ diffForest lchildren rchildren

diffed :: Eq a => AST a -> AST a -> Array (Int, Int) [Diff a]
diffed left@(Node lRoot lChildren) right@(Node rRoot rChildren) = result
  where result = array ((0, 0), (lmax, rmax)) $
                 [((a, b), diffFrom a b) | a <- [0..lmax], b <- [0..rmax]]
        (lmax, rmax) = (treeLen left, treeLen right)
        diffFrom li ri = case (lefts, rights) of
          ([], []) -> []
          (ls, []) -> map (Del <$>) ls
          ([], ls) -> map (Ins <$>) ls
          (l@(Node lf lfChildren):ls, r@(Node rf rfChildren):rs) ->
            argmin (weighDiff α <$>) [modified, removed, added]
            where removed  = (Del <$> l) : result ! (li + treeLen l, ri)
                  added    = (Ins <$> r) : result ! (li, ri + treeLen r)
                  modified | lf == rf   = Node (Non lf) childrenDiff : rest
                           | otherwise = Node (Mod lf rf) childrenDiff : rest
                  childrenDiff = result ! (li + 1, ri + 1)
                  rest = result ! (li + treeLen l, ri + treeLen r)
          where lefts  = findForest li lChildren
                rights = findForest ri rChildren
                         
                
-- This function probably includes extra cases, but I'm too sleepy to fix it properly.
findForest :: Int -> [AST a] -> [AST a]
findForest li forest = snd $ go (li, forest)
  where go (0, res) = (0, res)
        go (n, []) = (n, [])
        go (n, (Node _ children):rest) = case go (n - 1, children) of
          (0, res) -> (0, res)
          (n, _)   -> go (n - 1, rest)
          
treeLen :: AST a -> Int
treeLen (Node _ children) = 2 + forestLen children

forestLen :: [AST a] -> Int
forestLen = foldl' (\ n tree -> n + treeLen tree) 0
        
diffForest :: Eq a => [AST a] -> [AST a] -> [Diff a]
diffForest [] [] = []
diffForest ls [] = map (Del <$>) ls
diffForest [] ls = map (Ins <$>) ls
diffForest (l@(Node lRoot lChildren):ls) (r@(Node rRoot rChildren):rs) =
  argmin (weighDiff α <$>) [modified, removed, added]
  where removed                  = (Del <$> l) : diffForest ls (r:rs)
        added                    = (Ins <$> r) : diffForest (l:ls) rs
        modified | lRoot == rRoot = Node (Non lRoot) childrenDiff : rest
                 | otherwise     = Node (Mod lRoot rRoot) childrenDiff : rest
        childrenDiff             = diffForest lChildren rChildren
        rest                     = diffForest ls rs
            
weighDiff :: Double -> Diff a -> Double
weighDiff α (Node (Ins _) _)    = α
weighDiff α (Node (Del _) _)    = α
weighDiff α (Node v children) = weight v + α * (sum $ weighDiff α <$> children)
  where weight Non{} = 0
        weight Mod{} = 0.1 * α
        weight _     = α