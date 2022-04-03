module Palindrome3 where

import Prelude

import Data.Array (any, drop, filter, head, length)
import Data.Int (odd)
import Data.List (toUnfoldable)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Helpfulthings (arrCharToInt)
import Palindrome1 (isPalindrome)

{-
For a given number num, write a function to test if the number contains
a numerical palindrome or not and return a boolean 
(true if it does and false if does not). 
Return Nothing if the input is not an integer or is less than 0.

Note: Palindromes should be found without permutating num.

Single digit numbers will not be considered numerical palindromes.
-}

palindrome :: Int -> Maybe Boolean
palindrome i =
  let
    arr = arrCharToInt $ toCharArray $ show i
  in
    if i < 0 then Nothing
    else case length arr of
      _ | isPalindrome i -> Just true
      l | l == 1 -> Just false
      l -> go Map.empty arr l

  where
  go acc arr leng = case head arr of
    Just v -> go (Map.insertWith (\k _ -> k + 1) v 1 acc) (drop 1 arr) leng
    Nothing ->
      if odd leng then oddCheck acc else evenCheck acc
  evenCheck acc =
    let
      vals = toUnfoldable $ Map.values acc
    in
      if any odd vals then Just false else Just true
  oddCheck acc =
    let
      vals = toUnfoldable $ Map.values acc
      remOnes = filter ((_ == 1) >>> not)
    in
      if ((remOnes >>> length) vals) == length vals - 1 then
        if any odd (remOnes vals) then Just false else Just true
      else Just false
