module NextNumber where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray, deleteAt, dropEnd, elemIndex, fromArray, head, reverse, sort, takeEnd, toArray)
import Data.Either (Either(..))
import Data.Formatter.Internal (foldDigits)
import Data.Maybe (Maybe(..), fromMaybe)
import Helpfulthings (toArrDigits)

{-
Create a function that takes a positive integer and returns the next bigger 
number that can be formed by rearranging its digits. 
For example:

nextBigger(num: 12)   // returns 21
nextBigger(num: 513)  // returns 531
nextBigger(num: 2017) // returns 2071

If the digits can't be rearranged to form a bigger number, return -1.
-}

nextBigger :: Int -> Either String Int
nextBigger i =
  if i < 0 then Left "Make sure your input is valid!"
  else
    case fromArray $ toArrDigits i of
      Nothing -> Right (-1)
      Just arr ->
        if arr == (arr # sort >>> reverse) then Right (-1)
        else Right $ fromMaybe (-1) $ go arr 0

  where
  go :: NonEmptyArray Int -> Int -> Maybe Int
  go arr acc = do
    mbFst <- Array.last $ dropEnd acc arr
    mbSnd <- Array.last $ dropEnd (acc + 1) arr
    if mbFst <= mbSnd then go arr (acc + 1)
    else do
      mbNea <- fromArray $ takeEnd (acc + 2) arr
      mbUpdated <- updateNum mbNea
      pure $ foldDigits $ dropEnd (acc + 2) arr <> mbUpdated

  updateNum arr = do
    let h = head arr
    fst <- toArray arr # Array.sort >>> Array.find (_ > h)
    idx <- elemIndex fst arr
    arr' <- deleteAt idx arr
    arr' # Array.sort >>> Array.cons fst >>> pure
