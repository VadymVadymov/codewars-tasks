module Palindrome2 where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Formatter.Internal (foldDigits)
import Data.Int (even)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple, fst, snd)
import Helpfulthings (arrCharToInt, intSwap)
import Palindrome1 (isPalindrome)
import Data.Array
  ( cons
  , dropEnd
  , elemIndex
  , filter
  , index
  , modifyAt
  , nub
  , reverse
  , slice
  , take
  , takeEnd
  , updateAt
  )

{-
You'll be given 2 numbers as arguments: (num,s). Write a function which returns
an array of s number of numerical palindromes that come after num. If num is a 
palindrome itself, it should be included in the count.

Return "Not valid" instead if any one of the inputs is not an integer or is less
than 0.

palindrome(6,4) => [11,22,33,44]
palindrome(59,3) => [66,77,88]
palindrome(101,2) => [101,111]
palindrome("15651",5) => "Not valid" 
palindrome(1221,"8") => "Not valid" 

Single numbers are not considered as valid.

  4580854

-}

-- if (fromMaybe 1 $ index arr (l/2 - 1)) == 0
palindrome :: Tuple Int Int -> Either String (Array Int)
palindrome t = case fst t, snd t of
  _, s | s < 0 -> Left "'s' value is less than zero!"
  n, _ | n < 0 -> Left "'num' value is less than zero!"
  n, s -> go n s []
  where
  go :: Int -> Int -> Array Int -> Either String (Array Int)
  go num acc arr = case acc of
    0 ->
      if isPalindrome $ fst t then
        (pure <<< nub <<< cons (fst t) <<< reverse) arr
      else pure $ reverse $ nub arr
    _ -> go (findPoli num) (acc - 1)
      ( cons
          ( if isPalindrome (findPoli num) then
              findPoli num
            else
              findPoli $ findPoli num
          )
          arr
      )

  findPoli num =
    let
      arr = arrCharToInt $ toCharArray $ show num
    in
      case Array.length arr of
        l | l == 1 -> 11
        l | even l ->
          case
            foldDigits $ take (l / 2) arr,
            foldDigits $ takeEnd (l / 2) arr
            of
            p1, p2 | p1 > p2 ->
              if isPalindrome $ foldDigits arr then foldDigits $ addOne l arr
              else
                foldDigits $ toPalindromeEven arr
            p1, p2 | p1 < p2 -> foldDigits $ addOne l arr
            p1, p2 | p1 == p2 ->
              if p1 > (intSwap p2) then
                foldDigits $ addOne l arr
              else if p1 == (intSwap p2) then
                foldDigits $ addOne l arr
              else
                foldDigits $ toPalindromeEven arr
            _, _ -> 0
        l ->
          if ((fromMaybe 0 <<< index arr) l / 2) == 9 then
            let
              arr' = addOne l arr
            in
              foldDigits $ toPalindromeOdd arr'
          else
            case
              foldDigits $ take (l / 2) arr,
              foldDigits $ takeEnd (l / 2) arr
              of
              p1, p2 | p1 > p2 -> foldDigits $ toPalindromeOdd $ arr
              p1, p2 | p1 < p2 -> foldDigits $ toPalindromeOdd $ addOne l arr
              _, _ -> maybe 0 (foldDigits <<< toPalindromeOdd)
                (modifyAt (l / 2) (_ + 1) arr)

  addOne l arr =
    if even l then case take (l / 2) arr of
      a | (filter (_ == 9) a) == a ->
        ( nineToZero $ arrCharToInt
            $ toCharArray
            $ show
            $ add 1
            $ foldDigits a
        ) <> (nineToZero $ takeEnd (l / 2) arr)
      _ -> toPalindromeEven $
        ( arrCharToInt $ toCharArray $ show $ add 1
            $ foldDigits
            $ take (l / 2) arr
        ) <> take (l / 2) arr
    else
      toPalindromeOdd $
        ( nineToZero $ arrCharToInt $ toCharArray $ show $ add 1 $ foldDigits
            $ take (l / 2 + 1) arr
        ) <> (nineToZero $ takeEnd (l / 2) arr)

  nineToZero arr = case elemIndex 9 arr of
    Just i -> nineToZero $ fromMaybe [] $ (updateAt i 0 arr)
    Nothing -> arr

  toPalindromeEven arr = (take (Array.length arr / 2) arr) <>
    ( reverse
        $ take (Array.length arr / 2) arr
    )

  toPalindromeOdd arr =
    let
      l = Array.length arr
    in
      if isPalindrome (foldDigits arr) then arr
      else (dropEnd (l / 2) arr) <> (reverse $ slice 0 (l / 2) arr)

-- Have no clue how to perform it easier
-- Issue to fix: with just '9' symbol input returns one less palindrome