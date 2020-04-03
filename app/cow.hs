module Main where

import           Control.Applicative           ((*>), (<$>), (<*), (<*>))

import           System.Environment            (getArgs)

import           Text.ParserCombinators.Parsec

main ∷ IO ()
main = putStrLn "Cow"
