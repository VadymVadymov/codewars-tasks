module FizzBuzz where

import Prelude

import Data.Array as Array
import Data.String as String
import Effect.Console as Console
import Effect (Effect)

{-

Write a function which prints ints from 1 to 100 where all of ones divided by
3 return `Fizz`, by 5 - `Buzz` and `FizzBuzz` in case of both.

-}

fizzBuzz :: Effect Unit -- Standart version
fizzBuzz = go 1
  where
  go i = do
    let
      isDivBy3 = if mod i 3 == 0 then true else false
      isDivBy5 = if mod i 5 == 0 then true else false
      print v =
        if i <= 100 then Console.log v >>= \_ -> go (i + 1)
        else pure unit
    case isDivBy3, isDivBy5 of
      true, true -> print "FizzBuzz"
      true, _ -> print "Fizz"
      _, true -> print "Buzz"
      _, _ -> print $ show i

type Cfg = Array { num :: Int, word :: String }

fizzBuzzCfg :: Effect Unit
fizzBuzzCfg = go cfg 1
  where
  go arr i = do
    let
      output = Array.fold $
        arr <#> (\{ num, word } -> if mod i num == 0 then word else mempty)
      print v = when (i <= 100) do
        Console.log v
        go arr $ i + 1
    if String.null output then print $ show i else print output
  cfg =
    [ { num: 3, word: "Fizz" }
    , { num: 5, word: "Buzz" }
    , { num: 7, word: "Kekw" }
    , { num: 10, word: "Bruh" }
    ]
