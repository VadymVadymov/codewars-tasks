module SnailSort where

import Prelude

import Data.Array (all, drop, dropEnd, length, modifyAt, reverse, take, takeEnd)
import Data.Either (Either(..))
import Data.Maybe (maybe)

{-
Given an n x n array, return the array elements arranged from outermost 
elements to the middle element, traveling clockwise.

array = [[1,2,3],
         [4,5,6],
         [7,8,9]]

snail(array) #=> [1,2,3,6,9,8,7,4,5]

For better understanding, please follow the numbers
of the next array consecutively:

array = [[1,2,3], [8,9,4], [7,6,5]]
snail(array) #=> [1,2,3,4,5,6,7,8,9]

array = [[1,2,3]
         [1,2,3]
         [1,2,3]]
        

-}

snail :: Array (Array Int) -> Either String (Array Int)
snail a =
  if validInput a then pure $ go a []
  else Left "Make sure the input is valid!"
  where
  validInput arr =
    let
      l = length arr
    in
      if all (length >>> eq l) arr then true else false

  go :: Array (Array Int) -> Array Int -> Array Int
  go arr acc = case length arr of
    l | l <= 2 -> acc <> maybe (join arr) join (modifyAt 1 reverse arr)
    _ ->
      go
        (arr # dropSides >>> map (drop 1 >>> dropEnd 1))
        ( acc
            <> (arr # take 1 >>> join)
            <> takeLasts arr
            <> (arr # takeEnd 1 >>> join >>> reverse)
            <> takeFirsts arr
        )
  takeLasts = dropSides >>> map (takeEnd 1) >>> join
  takeFirsts = dropSides >>> map (take 1) >>> join >>> reverse
  dropSides = drop 1 >>> dropEnd 1

