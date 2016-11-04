module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Tuple.Nested ((/\))
import Erl.Data.Tuple (tuple3, toNested3)
import Test.Assert (ASSERT, assert)

main :: forall t3. Eff ( assert :: ASSERT | t3) Unit
main = do
  assert $ (6 == _) $ case toNested3 (tuple3 1 2 3) of a /\ (b /\ c) -> a + b + c
