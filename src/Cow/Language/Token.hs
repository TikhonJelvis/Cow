{-# LANGUAGE TemplateHaskell #-}
-- | Tokens that I work with preserve the whitespace that follows them
-- so that we can reconstruct the input text from the tree, but ignore
-- the whitespace for logical operations (comparing tokens for
-- equality and so on).
module Cow.Language.Token where

import           Control.Lens

import           Data.Text     (Text)

import           Cow.ParseTree


-- | A single token that preserves the whitespace consumed in parsing
-- it.
data Token a = Token
  { _whitespace :: Text
    -- ^ The whitespace *before* this token.
  , _value      :: a
    -- ^ The semantic role of this token (ie list
    -- separator, object key) with any relevant content
    -- (ie the identifier itself).
  }

makeLenses ''Token

instance Eq a => Eq (Token a) where
  t1 == t2 = t1 ^. value == t2 ^. value

instance Show a => Show (Token a) where show = show . _value

