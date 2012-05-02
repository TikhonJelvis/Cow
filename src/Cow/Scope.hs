module Cow.Scope where

import Cow.Type

type Tag = Int

data Tagged a = Tagged Tag a deriving (Show, Eq)

data Scopes a = Scopes Tag [a]

instance Functor Tagged where fmap fn (Tagged i a) = Tagged i $ fn a

class Scopable a where
  bindings :: a -> [a]  -- For things that introduce bindings into the local scope.
  newEnv   :: a -> Bool -- For things that introduce a new scope.
  bound    :: a -> Bool -- For things that can be bound (e.g. variable names)
  
tag :: Scopable a => AST a -> AST (Tagged a)
tag = undefined