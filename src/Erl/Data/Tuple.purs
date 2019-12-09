-- | Native Erlang tuples
module Erl.Data.Tuple where

import Prelude

import Data.Eq (class Eq1)
import Data.Tuple.Nested as Nested

foreign import data Tuple1 :: Type -> Type

foreign import data Tuple2 :: Type -> Type -> Type

foreign import data Tuple3 :: Type -> Type -> Type -> Type

foreign import data Tuple4 :: Type -> Type -> Type -> Type -> Type

foreign import data Tuple5 :: Type -> Type -> Type -> Type -> Type -> Type

foreign import tuple1 :: forall a. a -> Tuple1 a

foreign import tuple2 :: forall a b. a -> b -> Tuple2 a b

foreign import tuple3 :: forall a b c. a -> b -> c -> Tuple3 a b c

foreign import tuple4 :: forall a b c d. a -> b -> c -> d -> Tuple4 a b c d

foreign import tuple5 :: forall a b c d e. a -> b -> c -> d -> e -> Tuple5 a b c d e

foreign import uncurry1 :: forall a z. (a -> z) -> Tuple1 a -> z

foreign import uncurry2 :: forall a b z. (a -> b -> z) -> Tuple2 a b -> z

foreign import uncurry3 :: forall a b c z. (a -> b -> c -> z) -> Tuple3 a b c -> z

foreign import uncurry4 :: forall a b c d z. (a -> b -> c -> d -> z) -> Tuple4 a b c d -> z

foreign import uncurry5 :: forall a b c d e z. (a -> b -> c -> d -> e -> z) -> Tuple5 a b c d e -> z

foreign import fst :: forall a b. Tuple2 a b -> a

foreign import snd :: forall a b. Tuple2 a b -> b

untuple1 :: forall a. Tuple1 a -> a
untuple1 = uncurry1 identity

toNested2 :: forall a b. Tuple2 a b -> Nested.Tuple2 a b
toNested2 = uncurry2 Nested.tuple2

toNested3 :: forall a b c. Tuple3 a b c -> Nested.Tuple3 a b c
toNested3 = uncurry3 Nested.tuple3

toNested4 :: forall a b c d. Tuple4 a b c d -> Nested.Tuple4 a b c d
toNested4 = uncurry4 Nested.tuple4

toNested5 :: forall a b c d e. Tuple5 a b c d e -> Nested.Tuple5 a b c d e
toNested5 = uncurry5 Nested.tuple5

instance eqTuple1 :: (Eq a) => Eq (Tuple1 a) where
  eq a b = uncurry1 (\a1 ->
    (uncurry1 (\b1 -> a1 == b1) b))
    a

instance eqTuple2 :: (Eq a, Eq b) => Eq (Tuple2 a b) where
  eq a b = uncurry2 (\a1 a2 ->
    (uncurry2 (\b1 b2 -> a1 == b1 && a2 == b2) b))
    a

instance eqTuple3 :: (Eq a, Eq b, Eq c) => Eq (Tuple3 a b c) where
  eq a b = uncurry3 (\a1 a2 a3 ->
    (uncurry3 (\b1 b2 b3 -> a1 == b1 && a2 == b2 && a3 == b3) b))
    a

instance eqTuple4 :: (Eq a, Eq b, Eq c, Eq d) => Eq (Tuple4 a b c d) where
  eq a b = uncurry4 (\a1 a2 a3 a4 ->
      (uncurry4 (\b1 b2 b3 b4 ->
        a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4)
      b))
    a

instance eqTuple5 :: (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq (Tuple5 a b c d e) where
  eq a b = uncurry5 (\a1 a2 a3 a4 a5 ->
      (uncurry5 (\b1 b2 b3 b4 b5 ->
        a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5)
      b))
    a

instance eq1Tuple1 :: Eq1 Tuple1 where
  eq1 = eq

instance eq1Tuple2 :: Eq a => Eq1 (Tuple2 a) where
  eq1 = eq

instance eq1Tuple3 :: (Eq a, Eq b) => Eq1 (Tuple3 a b) where
  eq1 = eq

instance eq1Tuple4 :: (Eq a, Eq b, Eq c) => Eq1 (Tuple4 a b c) where
  eq1 = eq

instance eq1Tuple5 :: (Eq a, Eq b, Eq c, Eq d) => Eq1 (Tuple5 a b c d) where
  eq1 = eq

instance showTuple1 :: (Show a) => Show (Tuple1 a) where
  show = uncurry1 (\a ->
    "(Tuple1 " <> show a <> ")")

instance showTuple2 :: (Show a, Show b) => Show (Tuple2 a b) where
  show = uncurry2 (\a b ->
    "(Tuple2 " <> show a <> " " <> show b <> ")")

instance showTuple3 :: (Show a, Show b, Show c) => Show (Tuple3 a b c) where
  show = uncurry3 (\a b c ->
    "(Tuple3 " <> show a <> " " <> show b <> " " <> show c <> ")")

instance showTuple4 :: (Show a, Show b, Show c, Show d) => Show (Tuple4 a b c d) where
  show = uncurry4 (\a b c d ->
    "(Tuple4 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> ")")

instance showTuple5 :: (Show a, Show b, Show c, Show d, Show e) => Show (Tuple5 a b c d e) where
  show = uncurry5 (\a b c d e ->
    "(Tuple5 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> ")")
