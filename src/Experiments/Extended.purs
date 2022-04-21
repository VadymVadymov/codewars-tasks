module Experiments.Extended where

import Prelude

import Control.Extend ((<<=))
import Data.Array as Array
import Data.Either (Either)
import Data.Maybe (Maybe)

extendArray :: Array String -> Array Int
extendArray arr = Array.reverse $ Array.length <<= arr

extendMaybe :: Maybe Int -> Maybe String
extendMaybe mb = show <<= mb

extendEither :: Either String Int -> Either String String
extendEither eith = show <<= eith -- similar to `map (pure >>> show) `
