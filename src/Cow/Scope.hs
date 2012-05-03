module Cow.Scope where

import Control.Monad.State (State, get, put)

import Data.Functor        ((<$>), (<$))
import Data.Maybe          (mapMaybe, listToMaybe)

import Cow.Type

type Tag = Int

data Tagged a = Tagged Tag a deriving (Show, Eq)

data Scopes a = Scopes Tag [[(a, Tag)]]

instance Functor Tagged where fmap fn (Tagged i a) = Tagged i $ fn a
                              
type WithScopes a = State (Scopes a)

class Scopable a where
  bindings :: a -> [a]  -- For things that introduce bindings into the local scope.
  newEnv   :: a -> Bool -- For things that introduce a new scope.
  bound    :: a -> Bool -- For things that can be bound (e.g. variable names)
  
getTag :: Eq a => [[(a, Tag)]] -> a -> Maybe Tag
getTag scopes val = listToMaybe $ mapMaybe (lookup val) scopes

pushScope :: [(a, Tag)] -> WithScopes a ()
pushScope scope = get >>= put . (push scope)
  where push val (Scopes lastTag scopes) = Scopes lastTag $ val:scopes
        
increment :: WithScopes a ()
increment = get >>= put . \ (Scopes lastTag scopes) -> Scopes (succ lastTag) scopes
        
bind :: a -> WithScopes a ()
bind val = do Scopes lastTag (curr:rest) <- get
              put . Scopes (succ lastTag) $ ((val, lastTag):curr):rest

tag :: Eq a => a -> WithScopes a (Tagged a)
tag val = do Scopes lastTag scopes <- get
             let newTag = Tagged lastTag val <$ increment
             maybe newTag (return . (`Tagged` val)) $ getTag scopes val
             