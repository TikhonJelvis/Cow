module Cow.Scope (tag, Tagged(..), Tag, Scopable, bindings, globalBindings, newEnv, bound) where

import Control.Applicative ((<$>), (<$), (<*>), (<*), (*>))
import Control.Monad.State (State, get, put, runState, modify)

import Data.Maybe          (mapMaybe, listToMaybe, isJust, fromJust)

import Cow.Type

data Scopes a = Scopes Tag [[(a, Tag)]]
                              
type WithScopes a = State (Scopes a)

class Scopable a where
  bindings :: AST a -> [a]       -- For things that introduce bindings into the local scope.
  newEnv   :: a -> Bool          -- For things that introduce a new scope.
  bound    :: a -> Bool          -- For things that can be bound (e.g. variable names).

  globalBindings :: AST a -> [a] -- For things that add bindings to the root environment.
  globalBindings _ = []

  
getTag :: Eq a => a -> Scopes a -> Maybe Tag
getTag value (Scopes _ scopes) = listToMaybe $ mapMaybe (lookup value) scopes

pushScope :: WithScopes a ()
pushScope = modify $ \ (Scopes lastTag scopes) -> Scopes lastTag ([]:scopes)

popScope :: WithScopes a ()
popScope = modify $ \ (Scopes lastTag scopes) -> Scopes lastTag (drop 1 scopes)
        
increment :: WithScopes a ()
increment = modify $ \ (Scopes lastTag scopes) -> Scopes (succ lastTag) scopes
        
bind :: Eq a => a -> WithScopes a ()
bind value = modify bindVal >> increment
  where bindVal (Scopes lastTag []) = bindVal $ Scopes lastTag [[]]
        bindVal scope@(Scopes lastTag (curr:rest))
          | isJust (lookup value curr)= scope
          | otherwise               = Scopes lastTag $ ((value, lastTag):curr):rest

globalBind :: Eq a => a -> WithScopes a (Tagged a)
globalBind value = get >>= go
  where go (Scopes lastTag []) = go $ Scopes lastTag [[]]
        go state@(Scopes lastTag scopes)
          | isJust (getTag value state) = return $ Tagged (fromJust $ getTag value state) value
          | otherwise                 = binding <$ put updatedScopes <* increment
          where updatedScopes = Scopes lastTag $ init scopes ++ [(value, lastTag) : last scopes]
                binding = Tagged lastTag value

tagVal :: Eq a => a -> WithScopes a (Tagged a)
tagVal value = getTag value <$> get >>= maybe (globalBind value) (return . (`Tagged` value)) 
                
tag :: (Scopable a, Eq a) => AST a -> AST (Tagged a)
tag value = fst . runState (go value) $ Scopes 0 [[]] 
  where go ast@(Node v children)
          | newEnv v = pushScope *> taggedNode <* popScope
          | otherwise    = taggedNode
          where taggedNode  = newBindings *> (Node <$> currTag <*> mapM go children)
                newBindings = mapM_ bind (bindings ast) >> mapM_ globalBind (globalBindings ast)
                currTag | bound v = tagVal v
                        | otherwise   = get >>= \ (Scopes lastTag _) -> Tagged lastTag v <$ increment
