module Cow.Type where

import Data.Functor ((<$>))
import Data.List    (intercalate)

data AST a = Node a [AST a] deriving (Eq)

leaf :: a -> AST a
leaf = (`Node` [])
  
val :: AST a -> a
val (Node v _) = v

instance Functor AST where fmap fn (Node v children) = Node (fn v) $ map (fmap fn) children 

instance Show a => Show (AST a) where 
  show (Node value [])       = "[" ++ show value ++ "]"
  show (Node value children) = "[" ++ show value ++ intercalate " " (show <$> children) ++ "]"

data Change a = Ins a
              | Del a
              | Mod a a
              | Non a -- No change
              | From Tag a
              | To Tag a deriving (Eq)
                       
instance Show a => Show (Change a) where 
  show (Ins a)    = "\\Ins{" ++ show a ++ "}"
  show (Del a)    = "\\Del{" ++ show a ++ "}"
  show (Mod a b)  = "\\Mod{" ++ show a ++ "}{" ++ show b ++ "}"
  show (Non a)    = "\\Non{" ++ show a ++ "}"
  show (From t a) = "\\From{" ++ show t ++ "}{" ++ show a ++ "}"
  show (To t a)   = "\\To{" ++ show t ++ "}{" ++ show a ++ "}"
                       
type Diff a = AST (Change a)

data IdWrap a = IdWrap Integer a deriving (Show)
instance Eq (IdWrap a) where (IdWrap n1 _) == (IdWrap n2 _) = n1 == n2

type Tag = Int

data Tagged a = Tagged Tag a deriving (Eq)

instance Show a => Show (Tagged a) where show (Tagged i a) = "$\\langle" ++ show i ++ "\\rangle$ " ++ show a

instance Functor Tagged where fmap fn (Tagged i a) = Tagged i $ fn a
                              
type Merged a = AST (Conflict a)

data Conflict a = Conflict (Change a) (Change a)
                | NoConflict (Change a) deriving Eq
                                                 
instance Show a => Show (Conflict a) where
  show (Conflict a b) = "\\Conflict{" ++ show a ++ "}{" ++ show b ++ "}"
  show (NoConflict a) = "\\NoConflict{" ++ show a ++ "}"
