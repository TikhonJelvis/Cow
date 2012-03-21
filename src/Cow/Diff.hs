module Cow.Diff where

import Data.Algorithm.Diff (getDiff, DI(..))
import Data.Functor        ((<$>))

import Cow.Equality
import Cow.Type

diff :: (Show a, Eq a, ExtEq a) => AST a -> AST a -> Diff a
diff (Node l lchildren) (Node r rchildren) 
  | l ?= r    = Node (Non l r) $ comp lchildren rchildren
  | otherwise = Node (Mod l r) $ comp lchildren rchildren
                
comp :: (Show a, Eq a, ExtEq a) => [AST a] -> [AST a] -> [Diff a]
comp left right = walk left right $ getDiff left right
  where walk (l:ls) (r:rs) ((B,_):ds) = diff l r : walk ls rs ds
        walk (l:ls) rs ((F,_):ds)     = del l : walk ls rs ds
        walk ls (r:rs) ((S,_):ds)     = ins r : walk ls rs ds
        walk [] [] []                 = []
        walk _ _ _                    = error "Wrong size of inputs or diff."
        del (Node r c) = Node (Del r) $ del <$> c
        ins (Node r c) = Node (Ins r) $ ins <$> c
