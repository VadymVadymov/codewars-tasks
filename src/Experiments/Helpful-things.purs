module Helpfulthings where

import Prelude

import Data.Array (concat, fold, reverse, take)
import Data.Either (Either(..))
import Data.Formatter.Internal (foldDigits)
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (toCharArray)

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
  '-' -> 0
  _ -> 42

unsafeCharConcat :: Char -> Char -> Int
unsafeCharConcat c1 c2 = unsafeCharToInt c1 * 10 + unsafeCharToInt c2

arrCharToInt :: Array Char -> Array Int
arrCharToInt = map unsafeCharToInt

intSwap :: Int -> Int
intSwap = foldDigits <<< arrCharToInt <<< reverse <<< toCharArray <<< show

takeDigits :: Int -> Int -> Int
takeDigits i = show >>> toCharArray >>> take i >>> arrCharToInt >>> foldDigits

toArrDigits :: Int -> Array Int
toArrDigits = show >>> toCharArray >>> arrCharToInt

splitAndCut :: Int -> Array Int -> Array (Array Int)
splitAndCut i arr =
  let
    strArr = split ((show >>> Pattern) i) ((map show >>> fold) arr)
  in
    toCharArray >>> arrCharToInt <$> strArr

remFromArray :: Array Int -> Array Int -> Array Int
remFromArray pat arr =
  let
    strArr =
      split ((foldDigits >>> show >>> Pattern) pat) ((map show >>> fold) arr)
  in
    concat $ map (toCharArray >>> arrCharToInt) strArr