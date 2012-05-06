module Cow.Substructure where

import Control.Applicative    ((<$>), (<*>))

import Data.Algorithm.Munkres (hungarianMethodDouble)
import Data.Array.Unboxed
import Data.Function          (on)
import Data.List.Extras.Argmax
import Data.Maybe             (listToMaybe, mapMaybe, fromJust)

import Cow.Type
  
match :: Eq a => AST a -> AST a -> [(AST a, AST a)]
match left right = map toPair . fst $ hungarianMethodDouble inputArray 
  where pairs = allPairs numLeft numRight
        numLeft  = number left
        numRight = number right
        lenLeft  = maximum $ map (fst . val . fst) pairs
        lenRight = maximum $ map (fst . val . snd) pairs
        getVal (l@(Node (li, _) _), r@(Node (ri, _) _)) = 
          ((li, ri), weight (snd <$> l) (snd <$> r))
        inputArray = array ((1, 1), (lenLeft, lenRight)) $ getVal <$> pairs
        toPair (li, ri) = (fmap snd . fromJust $ findAST li numLeft,
                           fmap snd . fromJust $ findAST ri numRight)

findAST :: Eq i => i -> AST (i, a) -> Maybe (AST (i, a))
findAST i n@(Node (i', _) children)
  | i == i'    = Just n
  | otherwise = listToMaybe $ mapMaybe (findAST i) children

weight :: Eq a => AST a -> AST a -> Double
weight a b | a == b     = 0
           | otherwise = 1

number :: AST a -> AST (Int, a)
number = snd . go 1
  where go n (Node val [])       = (n + 1, Node (n, val) [])
        go n (Node val children) = (n', Node (n, val) children')
          where (n', children') = foldl combine (n + 1, []) children
                combine (index, acc) child =
                  let (index', child') = go index child in (index', acc ++ [child'])

allPairs :: AST (Int, a) -> AST (Int, a) -> [(AST (Int, a), AST (Int, a))]
allPairs l@(Node (li, _) lChildren) r@(Node (ri, _) rChildren) =
  (l, r):(lefts ++ rights ++ rest)
  where lefts  = rChildren >>= allPairs l
        rights = lChildren >>= (`allPairs` r)
        rest   = concat $ allPairs <$> lChildren <*> rChildren
        