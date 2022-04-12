module FibonacciElems where

import Prelude

import Data.Tuple (Tuple(..))

-- Create function fib that returns n'th element of Fibonacci sequence. 

fib :: Int -> Int
fib = go (Tuple 0 1)
  where
  go :: Tuple Int Int -> Int -> Int
  go (Tuple fst snd) = case _ of
    acc | acc <= 0 -> 0
    1 -> fst
    acc -> go (Tuple snd $ fst + snd) (acc - 1)
