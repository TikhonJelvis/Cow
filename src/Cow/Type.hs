module Cow.Type where

import Data.Functor ((<$>))
import Data.List    (intercalate)

import Cow.Equality

data AST a = Node a [AST a] deriving (Eq)

leaf :: a -> AST a
leaf = (`Node` [])

instance Functor AST where fmap fn (Node v children) = Node (fn v) $ map (fmap fn) children 

instance Show a => Show (AST a) where 
  show (Node value [])       = "[" ++ show value ++ "]"
  show (Node value children) = "[" ++ show value ++ intercalate " " (show <$> children) ++ "]"

data Change a = Ins a
              | Del a
              | Mod a a
              | Non a -- No change
              deriving (Eq)
                       
instance Show a => Show (Change a) where 
  show (Ins a) = "\\Ins{" ++ show a ++ "}"
  show (Del a) = "\\Del{" ++ show a ++ "}"
  show (Mod a b) = "\\Mod{" ++ show a ++ "}{" ++ show b ++ "}"
  show (Non a) = "\\Non{" ++ show a ++ "}"
                       
type Diff a = AST (Change a)

data ExtWrap a = ExtWrap a deriving (Show)
instance ExtEq a => Eq (ExtWrap a) where ExtWrap a == ExtWrap b = a ?= b

data IdWrap a = IdWrap Integer a deriving (Show)
instance Eq (IdWrap a) where (IdWrap n1 _) == (IdWrap n2 _) = n1 == n2

