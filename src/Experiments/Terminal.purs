module Experiments.Terminal where

import Ansi.Output
import Prelude

import Ansi.Codes (Color(..))
import Effect (Effect)
import Effect.Class.Console as Console

-- Some experiments with `Ansi` terminal features

hello :: Effect Unit
hello = do
  Console.log $ withGraphics (foreground BrightYellow) "Hello!"
  Console.log $ withGraphics (foreground BrightYellow <> bold) "Hello!"
  Console.log $ withGraphics
    (bold <> underline <> background Black <> foreground BrightWhite)
    "hello world"
  Console.log $ withGraphics
    (bold <> underline <> foreground BrightBlack)
    "hello world"

