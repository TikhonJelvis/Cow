module Cow.UI where

import Data.Functor ((<$>))
import Data.List    (intercalate)

import Cow.Language.JavaScript as JS
import Cow.Type

class Display a where
  displayChildren :: a -> [String] -> String
  displayHead :: a -> String


indent :: String -> String
indent = ("&nbsp;&nbsp;" ++)

instance Display JS.Value where
  displayChildren Root program      = intercalate "<br /><br />" program
  displayChildren (Operator o) [l, r] = l ++ " <span class=\"operator\">" ++ o ++ "</span> " ++ r
  displayChildren Call vs           = intercalate "" vs
  displayChildren Array vs          = "[" ++ intercalate ", " vs ++ "]"
  displayChildren Object vs         = "{ <div class=\"block\">" ++ intercalate ",<br />" vs ++ "</div>}"
  displayChildren Init sts          = "("  ++ intercalate "; " sts ++ ")"
  displayChildren Args args         = "("  ++ intercalate ", " args ++ ")"
  displayChildren Block sts         = "{ <div class=\"block\">" ++ intercalate "<br />" sts ++ "</div>}"
  displayChildren Parameters ps     = displayChildren Args ps
  displayChildren Assign ls         = displayChildren (Operator "=") ls
  displayChildren _ children        = intercalate " " children

  displayHead (Var v) = "<span class=\"var\">" ++ v ++ "</span> "
  displayHead (Num n) = "<span class=\"num\">" ++ show n ++ "</span> "
  displayHead (Str s) = "<span class=\"str\">" ++ show s ++ "</span> "
  displayHead (Key k) = "<span class=\"key\">" ++ k ++ "</span> "
  displayHead (Regex r) = "<span class=\"regex\">/" ++ r ++ "/</span> "
  displayHead (Keyword k) = "<span class=\"keyword\">" ++ k ++ "</span> "
  displayHead Function = "<span class=\"keyword\">function</span> "
  displayHead _ = ""

display :: Display a => AST a -> String
display (Node v children) = displayHead v ++ (displayChildren v $ display <$> children)

displayTagged :: Display a => AST (Tagged a) -> String
displayTagged (Node (Tagged t v) children) = "<span class=\"tagged n" ++ show t ++ "\">" ++ displayHead v ++ "</span>" ++ body
  where body = displayChildren v $ displayTagged <$> children
        
displayDiff :: Display a => AST (Change a) -> String
displayDiff n = go n
  where body a c = displayChildren a $ displayDiff <$> c
        go (Node (Ins a) c) = "<span class=\"ins\">" ++ displayHead a ++ body a c ++ "</span>"
        go (Node (Del a) c) = "<span class=\"del\">" ++ displayHead a ++ body a c ++ "</span>"
        go (Node (Mod a b) c) = "<span class=\"mod\"><del>" ++ displayHead a ++ "</del>" ++ displayHead b ++ body a c ++ "</span>"
        go (Node (Non a) c) = "<span class=\"non\">" ++ displayHead a ++ body a c ++ "</span>"
        go (Node (To t a) c) = "<span class=\"to id" ++ show t ++ "\">" ++ displayHead a ++ body a c ++ "</span>"
        go (Node (From t a) c) = "<span class=\"from id" ++ show t ++ "\">" ++ displayHead a ++ body a c ++ "</span>"
        
toHTML :: String -> String
toHTML content = unlines $
                 ["<html>",
                  "<head>",
                  "<title> Cow: Semantic Diffing and Merging </title>",
                  "<link rel=\"stylesheet\" type=\"text/css\" href=\"cow.css\" />",
                  "<script src=\"jquery.js\"></script>",
                  "<script src=\"cow.js\"></script>",
                  "</head>",
                  "<body>",
                  content,
                  "</body>",
                  "</html>"]
                            
                 