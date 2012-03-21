module Cow.Type where

import Cow.Equality

data AST a = Node a [AST a] deriving (Show, Eq)

data Change a = Ins a
              | Del a
              | Mod a a
              | Non a a -- No change
              deriving (Show, Eq)
                       
type Diff a = AST (Change a)

data ExtWrap a = ExtWrap a deriving (Show)

instance ExtEq a => Eq (ExtWrap a) where ExtWrap a == ExtWrap b = a ?= b
