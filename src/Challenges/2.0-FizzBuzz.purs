module FizzBuzz where

import Prelude

import Effect (Effect)
import Effect.Console as Console

{-

Write a function which prints ints from 1 to 100 where all of ones divided by
3 return `Fizz`, by 5 - `Buzz` and `FizzBuzz` in case of both.

-}

fizzBuzz :: Effect Unit
fizzBuzz = go 1
  where
  go i =
    let
      isDivBy3 = if mod i 3 == 0 then true else false
      isDivBy5 = if mod i 5 == 0 then true else false
      print v = Console.log v >>= \_ -> go (i + 1)
    in
      if i == 100 then Console.logShow i
      else case isDivBy3, isDivBy5 of
        true, true -> print "FizzBuzz"
        true, _ -> print "Fizz"
        _, true -> print "Buzz"
        _, _ -> print $ show i

