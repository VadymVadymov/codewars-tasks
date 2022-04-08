module ZerosToTheEnd where

import Prelude

import Data.Array (concatMap, deleteAt, elem, findIndex, replicate)
import Data.Maybe (Maybe(..), fromMaybe)

{-
Write a function that takes an array of values and moves all elements that are zero to the end of the array, otherwise preserving the order of the array. 

For example, the following array

[7, 2, 3, 0, 4, 6, 0, 0, 13, 0, 78, 0, 0, 19, 14]

is transformed into

[7, 2, 3, 4, 6, 13, 78, 19, 14, 0, 0, 0, 0, 0, 0]

You are NOT allowed to use any temporary arrays or objects.
-}

zerosToTheEnd :: Array Int -> Array Int
zerosToTheEnd ints =
  if elem 0 ints then go 0 ints
  else ints
  where
  go :: Int -> Array Int -> Array Int
  go acc arr = case findIndex (_ == 0) arr of
    Nothing -> (concatMap (\i -> if i == 0 then mempty else pure i) arr)
      <> replicate acc 0
    Just i ->
      let
        arr' = fromMaybe arr $ deleteAt i arr
      in
        go (acc + 1) arr'