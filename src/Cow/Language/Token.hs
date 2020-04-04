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

-- | A single token along with the whitespace that *followed* it.
data Token a = Token
    { whitespace :: Text
    -- ^ The whitespace *after* this token.
    , value      :: a
    -- ^ A value representing the token itself.
    } deriving (Generic)

instance Eq a ⇒ Eq (Token a) where
  t1 == t2 = t1 ^. #value == t2 ^. #value

instance Show a ⇒ Show (Token a) where
  show = show . view #value

-- | A class of values that can be converted directly to text. The
-- idea is that each language-specific token type can be rendered back
-- to text.
class ToText a where
  toText :: a -> Text

instance ToText a => ToText (Token a) where
  toText Token {..} = toText value <> whitespace
