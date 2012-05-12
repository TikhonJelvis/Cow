module Cow.Type where

import Control.DeepSeq

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
  
instance NFData a => NFData (AST a) where
  rnf (Node a children) = a `seq` children `deepseq` ()

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
                       
instance NFData a => NFData (Change a) where
  rnf (Ins a) = a `seq` ()
  rnf (Del a) = a `seq` ()
  rnf (Mod a b) = a `seq` b `seq` ()
  rnf (Non a) = a `seq` ()
  rnf (From t a) = t `seq` a `seq` ()
  rnf (To t a) = t `seq` a `seq` ()

type Diff a = AST (Change a)

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
