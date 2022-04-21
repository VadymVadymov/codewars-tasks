module Main where

import Prelude

import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Ansi.Output (bold, italic, underline, withGraphics)

main :: Effect Unit
main = Aff.launchAff_ $ go 1000
  where
  go = case _ of
    i | i < 0 -> liftEffect $ Console.log $
      withGraphics style "I'm dead inside"
    i -> do
      Console.log $ withGraphics (italic <> bold)
        ( show i
            <> " - 7 = "
            <> show (i - 7)
        )
      Aff.delay $ Aff.Milliseconds 25.0
      go (i - 7)
  style = underline <> bold
