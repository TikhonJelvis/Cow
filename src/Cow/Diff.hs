{-# LANGUAGE NamedFieldPuns #-}
module Cow.Diff (Change (..), diff, weighDiff) where

import           Cow.Type             (Change (..))

import           Data.Array           ((!), Array)
import qualified Data.Array           as A
import           Data.Functor         ((<$>))
import           Data.List            (minimumBy)
import           Data.Ord             (comparing)
import           Data.Tree            (Forest, Tree (..))

diff :: Eq a => Tree a -> Tree a -> Tree (Change a)
diff (Node v₁ forest₁) (Node v₂ forest₂)
  | v₁ == v₂   = Node (Non v₁)    $ diffed forest₁ forest₂ ! (0, 0)
  | otherwise = Node (Mod v₁ v₂) $ diffed forest₁ forest₂ ! (0, 0)

diffed :: Eq a => Forest a -> Forest a -> Array (Int, Int) [Tree (Change a)]
diffed forest₁ forest₂ = result
  where result = A.array ((0, 0), (max₁, max₂))
                 [((a, b), diffFrom a b) | a <- [0..max₁], b <- [0..max₂]]
        
        (max₁, max₂) = (2 + forestLength forest₁, 2 + forestLength forest₂)
        diffFrom start₁ start₂ = case (diffs₁, diffs₂) of
          ([], [])     -> []
          (forest, []) -> map (Del <$>) forest
          ([], forest) -> map (Ins <$>) forest
          (first₁@(Node value₁ _):_, first₂@(Node value₂ _):_) ->
            argmin (weighForest 0.1) [modified, removed, added]
              where removed  = (Del <$> first₁) : result ! (start₁ + length₁, start₂)
                    added    = (Ins <$> first₂) : result ! (start₁, start₂ + length₂)
                    modified | value₁ == value₂ = Node (Non value₁) subDiff : rest
                             | otherwise       = Node (Mod value₁ value₂) subDiff : rest

                    (length₁, length₂) = (treeLength first₁, treeLength first₂)
                    subDiff = result ! (start₁ + 1, start₂ + 1)
                    rest    = result ! (start₁ + length₁, start₂ + length₂)
          where diffs₁ = findForest start₁ forest₁
                diffs₂ = findForest start₂ forest₂

        treeLength Node { subForest } = 2 + forestLength subForest
        forestLength = sum . map treeLength
        findForest index forest = snd $ go (index, forest)
          where go (0, res)  = (0, res)
                go (n, [])   = (n, [])
                go (n, Node { subForest } : trees) = case go (n - 1, subForest) of
                  (0, res) -> (0, res)
                  (n', _)  -> go (n' - 1, trees)

-- | Assigns a weight to a potential diff based on how much the
-- structure of the tree changed.
weighDiff :: Double             -- ^ α, the parameter controlling how much changes
                               --   in children are discounted.
             -> Tree (Change a) -- ^ the diff to weigh
             -> Double          -- ^ the resulting score, representing how much
                               --   the diff changes the input tree
weighDiff α (Node (Ins _) _) = α
weighDiff α (Node (Del _) _) = α
weighDiff α (Node value children) = weight value + α * (sum $ weighDiff α <$> children)
  where weight Non{} = 0
        weight Mod{} = α
        weight _     = α

-- | Weighs all the diffs in the forest and sums them.
weighForest :: Double -> Forest (Change a) -> Double
weighForest α = sum . map (weighDiff α)

-- | Gets the minimum of a list by some weight function. Not defined
-- for an empty list.
argmin :: Ord o => (a -> o) -> [a] -> a
argmin fn ls = fst . minimumBy (comparing snd) . zip ls $ fn <$> ls
