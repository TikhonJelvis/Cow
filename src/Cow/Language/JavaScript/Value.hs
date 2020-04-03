{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Cow.Language.JavaScript.Value where

import           Control.Lens

import           Data.Generics.Labels ()
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Data.Text.Lens

import           GHC.Generics         (Generic)

import           Text.Printf          (printf)

import           Cow.Language.Token
import           Cow.ParseTree


-- | The name of an identifier or object key, kept abstract to
-- differentiate from strings. (Not 100% sure about this design
-- decision.)
newtype Name = Name { name :: Text } deriving (Eq, Generic)

instance Show Name where show n = n ^. #name . _Text

-- | The kinds of tokens we ultimately parse.
--
-- Note that this is not pure lexing data: differentiating between
-- some of these requires actual parsing.
data Value = Variable Name
           | Num Double
           | String Text
           | Regex Text

           | Label Name
           | LabelStart -- the colon after a label name

           | Keyword Text
           | Operator Text

           | Semicolon
           | LineEnd

             -- expressions in parentheses like (1 + 2), but *not*
             -- argument lists
           | ParenStart
           | ParenEnd

             -- array literals ([1,f(2),"foo",[1]])
           | ArrayStart
           | ArrayEnd
           | ArraySep

             -- array indexing (the [-] part of a[1])
           | IndexStart -- [
           | IndexEnd   -- ]

             -- arguments to called functions (the (1 + 2, 3) part of f(1 + 2, 3).
           | CallStart
           | CallEnd
           | CallSep

             -- argument lists (function definitions but *not* calls)
           | ArgStart
           | ArgEnd
           | ArgSep

           | DeclSep -- the ',' in 'var x = 10, y;'

             -- object literals
           | ObjStart
           | ObjEnd
           | ObjKey Name
           | ObjSep
           | ObjColon -- TODO: better name?

             -- loop/if/switch conditions (ie the (..) in if(..) and while(..)
           | CondStart
           | CondEnd

           | ForSep             -- the ; in a for(;;) loop.

           | CaseColon -- the ':' in 'case "foo":'

             -- the parens after a catch (ie (..) from 'catch (e) { .. }')
           | CatchStart
           | CatchEnd

             -- blocks: function bodies and control flow
           | BlockStart
           | BlockEnd

             -- comments
           | LineComment Text   -- // ..
           | BlockComment Text  -- /* .. */
           deriving (Eq)

makePrisms ''Value

instance Show Value where
  show = \case
    Variable (Name n) -> Text.unpack n
    Num n             -> printf "%.2f" n
    String text       -> show $ Text.unpack text
    Regex regex       -> printf "/%s/" $ Text.unpack regex

    Label (Name l)    -> printf "%s:" $ Text.unpack l
    LabelStart        -> ":"

    Keyword word      -> printf "<%s>" $ Text.unpack word
    Operator op       -> Text.unpack op

    Semicolon         -> ";"
    LineEnd           -> "<;>"

    ParenStart        -> "("
    ParenEnd          -> ")"

    ArrayStart        -> "["
    ArrayEnd          -> "]"
    ArraySep          -> ","

    IndexStart        -> "["
    IndexEnd          -> "]"

    CallStart         -> "("
    CallEnd           -> ")"
    CallSep           -> ","

    ArgStart          -> "("
    ArgEnd            -> ")"
    ArgSep            -> ","

    DeclSep           -> ","

    ObjStart          -> "{"
    ObjEnd            -> "}"
    ObjKey (Name key) -> printf "%s :" $ Text.unpack key
    ObjSep            -> ","
    ObjColon          -> ":"

    CondStart         -> "("
    CondEnd           -> ")"

    ForSep            -> ";"

    CaseColon         -> ":"

    CatchStart        -> "("
    CatchEnd          -> ")"

    BlockStart        -> "{"
    BlockEnd          -> "}"

    LineComment text  -> printf "//%s" $ Text.unpack text
    BlockComment text -> printf "/*%s*/" $ Text.unpack text

-- | Assigns a weight to each token based on how important it
-- is. Tokens that carry a lot of user-specific information are the
-- most important with a weight of '1', followed by keywords and
-- operators and finally punctuation at a low weight of '0.25'. The
-- numbers are arbitrary, but the idea is to *roughly* capture what is
-- and isn't important to the meaning of a program.
weigh ∷ Value → Double
weigh value | important value = 1
            | medium value    = 0.5
            | otherwise       = 0.2
  where important = \case
          Variable{}     -> True
          Num{}          -> True
          String{}       -> True
          Regex{}        -> True
          Label{}        -> True
          ObjKey{}       -> True
          LineComment{}  -> True
          BlockComment{} -> True
          _              -> False
        medium = \case
          Keyword{}      -> True
          Operator{}     -> True
          _              -> False

-- | A term is any valid fragment of JavaScript. This handles
-- whitespace (thanks to 'Token') and can be put directly into a
-- larger tree (ie a larger 'Term').
type Term = Parse' (Token Value)
