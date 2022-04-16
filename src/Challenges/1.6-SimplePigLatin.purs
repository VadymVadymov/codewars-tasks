module SimplePigLatin where

import Prelude

import Data.Array (fold, uncons)
import Data.CodePoint.Unicode (isLetter)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), codePointFromChar, length, split, trim)
import Data.String.CodeUnits (toCharArray, singleton)
import Data.String.Unsafe (char)

{-

Move the first letter of each word to the end of it,
then add "ay" to the end of the word. Leave punctuation marks untouched.

Examples:

pigIt('Pig latin is cool'); // igPay atinlay siay oolcay
pigIt('Hello world !');     // elloHay orldway !
-}

pigIt :: String -> String
pigIt str =
  let
    arr = split (Pattern " ") str <#>
      toCharArray
        >>> map singleton
        >>> uncons
        >>> fromMaybe { head: "", tail: [] }
  in
    trim $ fold $ map
      (\s -> if checkIsLetter s then append s "ay " else append s " ")
      do
        { head, tail } <- arr
        pure $ fold tail <> head
  where
  checkIsLetter = case _ of
    s | length s == 1 ->
      if isLetter $ codePointFromChar $ char s then true
      else false
    _ -> true
