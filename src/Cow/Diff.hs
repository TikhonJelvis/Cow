{-# LANGUAGE ParallelListComp #-}
module Cow.Diff where

import Data.Algorithm.Diff     (getDiff, DI(..))
import Data.Function           (on)
import Data.Functor            ((<$>))
import Data.List               (genericLength, permutations, sortBy, (\\))
import Data.List.Extras.Argmax (argmax)

import Cow.Equality
import Cow.Type

wrap :: ExtEq a => AST a -> ExtWrap a
wrap (Node h _) = ExtWrap h

-- Returns a value roughly correlated with how close two trees are. 
(≈) :: Eq a => AST a -> AST a -> Double
(Node l []) ≈ (Node r [])               = if l == r then 1 else 0
(Node l lchildren) ≈ (Node r rchildren) = 0.25 * (if l == r then 1 else 0) + 0.75 * result
  where average ls = sum ls / genericLength ls
        val = (average .) . zipWith (≈) 
        result = maximum $ zipWith val (repeat lchildren) (permutations rchildren)

toIdEq :: Eq a => [AST a] -> [AST a] -> ([IdWrap (AST a)], [IdWrap (AST a)])
toIdEq left right = let (lf, rf) = go 0 left (zip [1..] right) in (lf, snd <$> sortBy (compare `on` fst) rf)
  where go n [] rs    = ([], [(pos, IdWrap num r) | num <- [n..] | (pos, r) <- rs])
        go n ls []    = (zipWith IdWrap [n..] ls, [])
        go n (l:ls) rs 
          | l ≈ best > 0.5 = let (lres, rres) = go (n + 1) ls (rs \\ [(pos, best)]) in
                                  (IdWrap n l : lres, (pos, IdWrap n best) : rres)
          | otherwise       = let (lres, rres) = go (n + 1) ls rs in
                                  (IdWrap n l : lres, rres)
          where (pos, best) = argmax ((l ≈) . snd) rs

diff :: (Show a, Eq a, ExtEq a) => AST a -> AST a -> Diff a
diff (Node l lchildren) (Node r rchildren) 
  | l ?= r    = Node (Non l r) $ comp lchildren rchildren
  | otherwise = Node (Mod l r) $ comp lchildren rchildren
                
comp :: (Show a, Eq a, ExtEq a) => [AST a] -> [AST a] -> [Diff a]
comp left right = walk left right . uncurry getDiff $ toIdEq left right
  where walk (l:ls) (r:rs) ((B,_):ds)       = diff l r : walk ls rs ds
        walk (l:ls) (r:rs) ((S,_):(F,_):ds) = mod l r : walk ls rs ds
        walk (l:ls) rs ((F,_):ds)           = del l : walk ls rs ds
        walk ls (r:rs) ((S,_):ds)           = ins r : walk ls rs ds
        walk [] [] []                       = []
        walk _ _ _                          = error "Wrong size of inputs or diff."
        mod l@(Node lr _) r@(Node rr _) = let Node _ children = diff l r in
                                                Node (Mod lr rr) children
        del (Node r c) = Node (Del r) $ del <$> c
        ins (Node r c) = Node (Ins r) $ ins <$> c
