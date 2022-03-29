module SpinWords where

import Prelude

import Data.Array as Array
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), length, split, trim)
import Data.String.CodeUnits (fromCharArray, toCharArray)

{-
Write a function that takes in a string of one or more words, 
and returns the same string, but with all five or more letter words reversed 
Strings passed in will consist of only letters and spaces. 
Spaces will be included only when more than one word is present.

Examples: spinWords( "Hey fellow warriors" ) => returns "Hey wollef sroirraw" 
spinWords( "This is a test") => returns "This is a test" 
spinWords( "This is another test" )=> returns "This is rehtona test"
-}

spinWords :: String -> String
spinWords str = go "" $ split (Pattern " ") str
  where
  go :: String -> Array String -> String
  go acc arr = case fromMaybe "" $ Array.head arr of
    s | (length s) == 0 -> trim acc
    s | (length s) > 4 -> go (acc <> " " <> revert s) (Array.drop 1 arr)
    s -> go (acc <> " " <> s) (Array.drop 1 arr)

  revert = fromCharArray <<< Array.reverse <<< toCharArray

