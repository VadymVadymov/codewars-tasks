module Helpfulthings where

import Prelude

import Data.Either (Either(..))

charToInt :: Char -> Either Char Int
charToInt c = case c of
  '1' -> Right 1
  '2' -> Right 2
  '3' -> Right 3
  '4' -> Right 4
  '5' -> Right 5
  '6' -> Right 6
  '7' -> Right 7
  '8' -> Right 8
  '9' -> Right 9
  '0' -> Right 0
  _ -> Left c

unsafeCharToInt :: Char -> Int
unsafeCharToInt = case _ of
  '1' -> 1
  '2' -> 2
  '3' -> 3
  '4' -> 4
  '5' -> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9' -> 9
  '0' -> 0
  _ -> 42

unsafeCharConcat :: Char -> Char -> Int
unsafeCharConcat c1 c2 = unsafeCharToInt c1 * 10 + unsafeCharToInt c2