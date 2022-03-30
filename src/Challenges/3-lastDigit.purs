module LastDigit where

import Prelude

import Data.Array (index, last, reverse)
import Data.Int (pow)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (length)
import Data.String.CodeUnits (toCharArray)
import Helpfulthings (unsafeCharConcat, unsafeCharToInt)

{-
Find the last digit of the big number.

Examples:

lastDigit 4 1             `shouldBe` 4 (4^1 == 4)
lastDigit 4 2             `shouldBe` 6 (4^2 == 16)
lastDigit 9 7             `shouldBe` 9
lastDigit 10 (10^10)      `shouldBe` 0
lastDigit (2^200) (2^300) `shouldBe` 6

Input is always valid.
-}

lastDigit :: String -> String -> Maybe Int
lastDigit n m =
  let
    num = lastD n
    twoLastMod = unsafeCharConcat (secLastD m) (lastD m)
  in
    case num of
      '0' -> Just 0
      '1' -> Just 1
      '2' ->
        if (length $ show twoLastMod) == 2 then
          case checkRemainder twoLastMod of
            0 -> Just 6
            1 -> Just 2
            2 -> Just 4
            3 -> Just 8
            _ -> Nothing
        else lastFromPowTo 2
      '3' ->
        if (length $ show twoLastMod) == 2 then
          case checkRemainder twoLastMod of
            0 -> Just 1
            1 -> Just 3
            2 -> Just 9
            3 -> Just 7
            _ -> Nothing
        else lastFromPowTo 3
      '4' -> lastDigit "2" (show $ twoLastMod * 2)
      '5' -> Just 5
      '6' -> Just 6
      '7' ->
        if (length $ show twoLastMod) == 2 then
          case checkRemainder twoLastMod of
            0 -> Just 1
            1 -> Just 7
            2 -> Just 9
            3 -> Just 3
            _ -> Nothing
        else lastFromPowTo 7
      '8' -> lastDigit "2" (show $ twoLastMod * 3)
      '9' -> lastDigit "3" (show $ twoLastMod * 2)
      _ -> Nothing
  where
  lastFromPowTo i = Just $ unsafeCharToInt $ lastD $ show $ pow i $ unsafeCharConcat (secLastD m) (lastD m)
  secLastD arr = fromMaybe '0' $ index (reverse $ toCharArray arr) 1
  lastD = fromMaybe '0' <<< last <<< toCharArray
  checkRemainder i = i - ((i / 4) * 4)

-- Needs more graceful implementation