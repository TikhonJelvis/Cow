module Cow.Type where

import Data.Functor ((<$>))
import Data.List    (intercalate)

import Cow.Equality

data AST a = Node a [AST a] deriving (Eq)

instance Show a => Show (AST a) where 
  show (Node value [])       = "[" ++ show value ++ "]"
  show (Node value children) = "[" ++ show value ++ intercalate " " (show <$> children) ++ "]"

data Change a = Ins a
              | Del a
              | Mod a a
              | Non a a -- No change
              deriving (Show, Eq)
                       
type Diff a = AST (Change a)

data ExtWrap a = ExtWrap a deriving (Show)

instance ExtEq a => Eq (ExtWrap a) where ExtWrap a == ExtWrap b = a ?= b
