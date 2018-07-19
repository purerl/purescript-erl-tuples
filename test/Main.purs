module Test.Main where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Console (log)
import Erl.Data.Tuple (tuple3, toNested3)
import Test.Assert (assert)

main :: Effect Unit
main = do
  log "Testing toNested"
  assert $ (6 == _) $ case toNested3 (tuple3 1 2 3) of a /\ (b /\ (c /\ _)) -> a + b + c
