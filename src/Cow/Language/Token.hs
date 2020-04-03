{-# LANGUAGE TemplateHaskell #-}
-- | Tokens that I work with preserve the whitespace that follows them
-- so that we can reconstruct the input text from the tree, but ignore
-- the whitespace for logical operations (comparing tokens for
-- equality and so on).
module Cow.Language.Token where

import           Control.Lens

import           Data.Generics.Labels ()
import           Data.Text            (Text)

import           GHC.Generics         (Generic)

-- | A single token that preserves the whitespace consumed in parsing
-- it.
data Token a = Token
    { whitespace :: Text
    -- ^ The whitespace *before* this token.
    , value      :: a
    -- ^ The semantic role of this token (ie list
    } deriving (Generic)

instance Eq a ⇒ Eq (Token a) where
  t1 == t2 = t1 ^. #value == t2 ^. #value

instance Show a ⇒ Show (Token a) where
  show = show . view #value
