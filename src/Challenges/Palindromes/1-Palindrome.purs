module Palindrome1 where

import Prelude

import Data.Array (deleteAt, length, reverse, slice)
import Data.Int (odd)
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (toCharArray)

{-
A palindrome is a word, phrase, number, or other sequence
of characters which reads the same backward as forward.
Examples of numerical palindromes are:

2332 -> True
110011 -> True
54322345 -> True 
2334 -> False 
9 -> False

For a given number num, write a function to test 
if it's a numerical palindrome or not and return a boolean 
(true if it is and false if not).

Return Nothing if the input is less than 0 and Just True 
or Just False otherwise.
-}

isPalindrome :: Int -> Boolean
isPalindrome i =
  let
    arr = toCharArray $ show i
    l' = fromMaybe [] $ deleteAt (length arr / 2) arr
  in
    case length arr of
      l | l == 1 -> false
      l | odd l -> sliceAndEq (length l') l'
      l -> sliceAndEq l arr
  where
  sliceAndEq l arr =
    (slice 0 (l / 2) arr) == (reverse $ slice (l / 2) l arr)
