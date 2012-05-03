module Cow.Scope (tag) where

import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>))
import Control.Monad.State (State, get, put, runState)

import Data.Maybe          (mapMaybe, listToMaybe)

import Cow.Type

type Tag = Int

data Tagged a = Tagged Tag a deriving (Show, Eq)

data Scopes a = Scopes Tag [[(a, Tag)]]

instance Functor Tagged where fmap fn (Tagged i a) = Tagged i $ fn a
                              
type WithScopes a = State (Scopes a)

class Scopable a where
  bindings :: AST a -> [a]  -- For things that introduce bindings into the local scope.
  newEnv   :: a -> Bool -- For things that introduce a new scope.
  bound    :: a -> Bool -- For things that can be bound (e.g. variable names)
  
getTag :: Eq a => a -> Scopes a -> Maybe Tag
getTag val (Scopes _ scopes) = listToMaybe $ mapMaybe (lookup val) scopes

pushScope :: WithScopes a ()
pushScope = get >>= put . \ (Scopes lastTag scopes) -> Scopes lastTag ([]:scopes)

popScope :: WithScopes a ()
popScope = get >>= put . \ (Scopes lastTag scopes) -> Scopes lastTag (drop 1 scopes)
        
increment :: WithScopes a ()
increment = get >>= put . \ (Scopes lastTag scopes) -> Scopes (succ lastTag) scopes
        
bind :: a -> WithScopes a ()
bind val = do Scopes lastTag (curr:rest) <- get
              put . Scopes (succ lastTag) $ ((val, lastTag):curr):rest

newTag :: a -> WithScopes a (Tagged a)
newTag val = do Scopes lastTag _ <- get
                Tagged lastTag val <$ increment

tagVal :: Eq a => a -> WithScopes a (Tagged a)
tagVal val = getTag val <$> get >>= maybe (newTag val) (return . (`Tagged` val)) 
                
tag :: (Scopable a, Eq a) => AST a -> AST (Tagged a)
tag val = fst . runState (go val) $ Scopes 0 [[]] 
  where go ast@(Node value children)
          | newEnv value = pushScope *> taggedNode <* popScope
          | otherwise    = taggedNode
          where taggedNode  = newBindings *> (Node <$> currTag <*> mapM go children)
                newBindings = mapM_ bind (bindings ast)
                currTag | bound value = tagVal value
                        | otherwise   = newTag value
