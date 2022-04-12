module Monads where

import Prelude

import Data.Array (singleton)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Data.String as String

newtype Identity a = Identity { eval :: a }

derive instance Newtype (Identity a) _
derive instance Generic (Identity a) _
instance Show a => Show (Identity a) where
  show = genericShow

instance Eq a => Eq (Identity a) where
  eq = genericEq

instance Functor Identity where
  map f (Identity a) = wrap $ a { eval = f a.eval }

instance Apply Identity where
  apply (Identity f) (Identity a) = wrap $ a { eval = f.eval a.eval }

instance Applicative Identity where
  pure a = Identity { eval: a }

instance Bind Identity where
  bind (Identity a) f = f a.eval

runIdentity :: forall a. Identity a -> a
runIdentity (Identity a) = a.eval

evaluate :: Identity Int -> Identity Int
evaluate a = (_ + 2) <$> (_ + 10) <$> a

initialId :: Identity Int
initialId = pure 10

kleisonEval :: Int -> Identity Int
kleisonEval i = wrap { eval: i * 2 }

eval :: Int -> Int
eval v = runIdentity do
  i <- v # kleisonEval >>= kleisonEval
  i' <- pure $ i * 2
  pure i'

{-
Monad laws:
1. pure x >>= f  ===  f x

2. m >>= pure  ===  m

3. m >>= f >>= f'  ===  m >>= (\x -> f x >>= f')
-}

firstLaw :: Boolean
firstLaw = (pure 10 >>= kleisonEval) == (kleisonEval 10)

secondLaw :: Boolean
secondLaw = (initialId >>= pure) == initialId

thirdLaw :: Boolean
thirdLaw = (initialId >>= kleisonEval >>= kleisonEval)
  == (initialId >>= \x -> kleisonEval x >>= kleisonEval)

checkLaws :: Boolean
checkLaws = firstLaw && secondLaw && thirdLaw -- returns true

arrayKleisli :: String -> Array Int
arrayKleisli = String.length >>> singleton

evalArray :: Array Int
evalArray = do
  i <- arrayKleisli "Bruh"
  i' <- arrayKleisli "kekl" <> arrayKleisli "lol"
  pure $ i * i'