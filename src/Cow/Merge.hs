module Cow.Merge where

import           Data.Functor     ((<$>))

import           Cow.Diff
import           Cow.Substructure
import           Cow.Type

resolveConflicts :: Eq a => AST a -> AST a -> AST a -> (Merged a, Diff a, Diff a, Diff (Change a))
resolveConflicts base left right = (resolve <$> diff2, diffLeft, diffRight, diff2)
  where diffLeft                = tagDiff base left
        diffRight               = tagDiff base right
        diff2                   = diff diffLeft diffRight
        resolve (Ins a)         = NoConflict a
        resolve (Del a)         = NoConflict a
        resolve (Non a)         = NoConflict a
        resolve (Mod (Non _) a) = NoConflict a
        resolve (Mod a (Non _)) = NoConflict a
        resolve (Mod a b)       = Conflict a b
        resolve From{}          = error "You should not use tagDiff to merge right now!"
        resolve To{}            = error "You should not use tagDiff to merge right now!"

