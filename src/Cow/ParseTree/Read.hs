module Cow.ParseTree.Read where

import qualified Data.List.NonEmpty as NonEmpty

import           Text.Parsec

import           Cow.ParseTree

-- | A helper function that reads trees of ints from a string. Each
-- node is denoted with square brackets which contain its children,
-- separated by spaces. Leaves are just numbers.
--
-- For example, [[1][2[3 4]]] parses to:
--
-- @
--                     o
--                    / \
--                   o   o
--                  /   / \
--                 1   2   o
--                        / \
--                       3   4
-- @
readTree ∷ String → Either ParseError (Parse' Int)
readTree str = parse tree "<interactive>" str
  where tree =  Leaf' <$> leaf
            <|> Node' <$> node

        leaf = read <$> many1 digit <* spaces
        node = between (char '[' *> spaces) (char ']' *> spaces) children
        children = NonEmpty.fromList <$> many1 tree

-- | An unsafe version of readTree where a bad parse results in a
-- runtime error.
readTree' ∷ String → Parse' Int
readTree' str = case readTree str of
  Left err  -> error $ show err
  Right res -> res
