module ValidParentheses where

import Prelude

import Data.Array (filter)
import Data.Array as Array
import Data.Int (odd)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (toCharArray)

{-

Write a function that takes a string of parentheses,
and determines if the order of the parentheses is valid.

The function should return true if the string is valid,
and false if it's invalid.

Examples
"()"              =>  true
")(()))"          =>  false
"("               =>  false
"(())((()())())"  =>  true


-}
type Acc = { lefts :: Int, rights :: Int }

validate :: String -> Boolean
validate str =
  let
    pArr = filter ((_ == ')') || (_ == '(')) (toCharArray str)
  in
    case Array.length pArr of
      l | l < 1 -> false
      l | odd l -> false
      _ -> go pArr { lefts: 0, rights: 0 }
  where
  go :: Array Char -> Acc -> Boolean
  go arr acc =
    if acc.lefts < acc.rights then false
    else
      case fromMaybe 'e' $ Array.head arr of
        'e' -> if acc.rights == acc.lefts then true else false
        '(' -> go (Array.drop 1 arr) acc { lefts = acc.lefts + 1 }
        ')' -> go (Array.drop 1 arr) acc { rights = acc.rights + 1 }
        _ -> false

