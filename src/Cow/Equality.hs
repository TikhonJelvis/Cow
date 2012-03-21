module Cow.Equality where

class ExtEq a where
  (?=) :: a -> a -> Bool
  
instance ExtEq Integer where (?=) = (==)