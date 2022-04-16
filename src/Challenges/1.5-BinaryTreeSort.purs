module BinaryTreeSort where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)

{-

Your task is to return the list with elements from tree sorted by levels, which 
means the root element goes first, then root children (from left to right) are 
second and third, and so on.

Return empty list if root is Nothing.
Ex: 

                2
            8        9
          1  3     4   5

Should return following list: [2,8,9,1,3,4,5]
-}

data TreeNode a = TreeNode
  { left :: Maybe (TreeNode a)
  , value :: a
  , right :: Maybe (TreeNode a)
  }

derive instance Generic (TreeNode a) _
instance Show a => Show (TreeNode a) where
  show (TreeNode a) = "TreeNode " <> show
    { left: map show a.left
    , value: show a.value
    , right: map show a.right
    }

instance Functor TreeNode where
  map f (TreeNode tr) = TreeNode
    { left: map (map f) tr.left
    , value: f tr.value
    , right: map (map f) tr.right
    }

instance Apply TreeNode where
  apply (TreeNode f) (TreeNode tr) = TreeNode
    { left: map (apply (TreeNode f)) tr.left
    , value: f.value tr.value
    , right: map (apply (TreeNode f)) tr.right
    }

instance Applicative TreeNode where
  pure a = TreeNode { left: Nothing, value: a, right: Nothing }

initTree :: TreeNode Int
initTree = TreeNode
  { left: Just $ TreeNode
      { left: Just $ TreeNode
          { left: Just $ TreeNode { left: Nothing, value: 1, right: Nothing }
          , value: 1
          , right: Just $ TreeNode { left: Nothing, value: 6, right: Nothing }
          }
      , value: 8
      , right: Nothing
      }
  , value: 2
  , right: Just $ TreeNode
      { left: Just $ TreeNode
          { left: Just $ TreeNode { left: Nothing, value: 11, right: Nothing }
          , value: 10
          , right: Just $ TreeNode { left: Nothing, value: 16, right: Nothing }
          }
      , value: 9
      , right: Nothing
      }
  }

fTree :: TreeNode (Int -> Int)
fTree = TreeNode
  { left: Just $ TreeNode { left: Nothing, value: (_ + 10), right: Nothing }
  , value: (_ + 20)
  , right: Just $ TreeNode { left: Nothing, value: (_ + 30), right: Nothing }
  }

runTreeRight :: forall a. TreeNode a -> Array a
runTreeRight (TreeNode { left, value, right }) = pure value
  <> (fromMaybe [] $ map runTreeLeft left)
  <> (fromMaybe [] $ map runTreeLeft right)

runTreeLeft :: forall a. TreeNode a -> Array a
runTreeLeft (TreeNode { left, value, right }) = pure value
  <> (fromMaybe [] $ map runTreeRight left)
  <> (fromMaybe [] $ map runTreeLeft right)
