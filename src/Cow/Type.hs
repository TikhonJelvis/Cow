module Cow.Type (AST, Change (..), Conflict (..), Diff, Merged, Tag, Tagged (..), leaf,
                 module Data.Tree) where

import           Data.Functor    ((<$>))
import           Data.List       (intercalate)
import           Data.Tree       (Tree (..))

data Change a = Ins a
              | Del a
              | Mod a a
              | Non a -- No change
              | From Tag a
              | To Tag a deriving (Eq)

type AST a = Tree a

type Diff a = AST (Change a)

leaf :: a -> AST a
leaf a = Node a []

instance Show a => Show (Change a) where
  show (Ins a)    = "\\Ins{" ++ show a ++ "}"
  show (Del a)    = "\\Del{" ++ show a ++ "}"
  show (Mod a b)  = "\\Mod{" ++ show a ++ "}{" ++ show b ++ "}"
  show (Non a)    = "\\Non{" ++ show a ++ "}"
  show (From t a) = "\\From{" ++ show t ++ "}{" ++ show a ++ "}"
  show (To t a)   = "\\To{" ++ show t ++ "}{" ++ show a ++ "}"

type Tag = Int

data Tagged a = Tagged Tag a deriving (Eq)

instance Show a => Show (Tagged a) where show (Tagged i a) = "$\\langle" ++ show i ++ "\\rangle$ " ++ show a

instance Functor Tagged where fmap fn (Tagged i a) = Tagged i $ fn a

data Conflict a = Conflict (Change a) (Change a)
                | NoConflict (Change a) deriving Eq

type Merged a = AST (Conflict a)

instance Show a => Show (Conflict a) where
  show (Conflict a b) = "\\Conflict{" ++ show a ++ "}{" ++ show b ++ "}"
  show (NoConflict a) = "\\NoConflict{" ++ show a ++ "}"
