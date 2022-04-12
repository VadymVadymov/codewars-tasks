module Subscriptions where

import Prelude

import Data.String (length)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, launchSuspendedAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Halogen.Subscription as HS

main :: Effect Unit
main = do
  { emitter, listener } <- HS.create
  subscription <- HS.subscribe emitter $ Console.logShow <<< add 42 <<< length
  HS.notify listener "kk"
  HS.unsubscribe subscription
  pure unit

main' :: Aff Unit
main' = do
  liftEffect $ Console.logShow "ping"
  _ <- delay (Milliseconds 5000.0)
  liftEffect $ Console.logShow "pong"
  pure unit
