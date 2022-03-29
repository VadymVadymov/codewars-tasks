module Oddint where

import Prelude

import Data.Array (cons, drop, head, length, singleton)
import Data.Int (odd)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), snd)

{-

Given an array of integers, find the one that appears an odd number of times.

There will always be only one integer that appears an odd number of times.

Examples
[7] should return 7, because it occurs 1 time (which is odd).
[0] should return 0, because it occurs 1 time (which is odd).
[1,1,2] should return 2, because it occurs 1 time (which is odd).
[0,1,0,1,0] should return 0, because it occurs 3 times (which is odd).
[1,2,2,3,3,3,4,3,3,3,2,2,1] should return 4, because it appears 1 time (which is odd).

-}
type Key = Int

type Value = Array Int

type State = Map.Map Key Value

solution :: Array Int -> Int
solution a = go a Map.empty
  where
  go :: Array Int -> State -> Int
  go arr st = case arr of
    [] ->
      fromMaybe 0
        $ head
        $ snd
        $ fromMaybe (Tuple 0 [ 0 ])
        $ head (Map.toUnfoldable (Map.filter (fil) st))
    _ ->
      let
        i = fromMaybe 0 $ head arr
      in
        go (drop 1 arr) $ Map.insertWith (\_ -> cons i) i (singleton i) st

  fil :: Array Int -> Boolean
  fil ar = odd $ length ar

  