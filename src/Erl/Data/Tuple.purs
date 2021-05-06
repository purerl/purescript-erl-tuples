-- | Native Erlang tuples
module Erl.Data.Tuple where

import Prelude

import Data.Eq (class Eq1)
import Data.Tuple.Nested as Nested
import Data.Bifunctor (class Bifunctor)
import Data.Bifoldable (class Bifoldable)
import Data.Bitraversable (class Bitraversable)

foreign import data Tuple1 :: Type -> Type

foreign import data Tuple2 :: Type -> Type -> Type

foreign import data Tuple3 :: Type -> Type -> Type -> Type

foreign import data Tuple4 :: Type -> Type -> Type -> Type -> Type

foreign import data Tuple5 :: Type -> Type -> Type -> Type -> Type -> Type

foreign import data Tuple6 :: Type -> Type -> Type -> Type -> Type -> Type -> Type

foreign import data Tuple7 :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type

foreign import data Tuple8 :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type

foreign import data Tuple9 :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type

foreign import data Tuple10 :: Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type -> Type

foreign import tuple1 :: forall a. a -> Tuple1 a

foreign import tuple2 :: forall a b. a -> b -> Tuple2 a b

foreign import tuple3 :: forall a b c. a -> b -> c -> Tuple3 a b c

foreign import tuple4 :: forall a b c d. a -> b -> c -> d -> Tuple4 a b c d

foreign import tuple5 :: forall a b c d e. a -> b -> c -> d -> e -> Tuple5 a b c d e

foreign import tuple6 :: forall a b c d e f. a -> b -> c -> d -> e -> f -> Tuple6 a b c d e f

foreign import tuple7 :: forall a b c d e f g. a -> b -> c -> d -> e -> f -> g -> Tuple7 a b c d e f g

foreign import tuple8 :: forall a b c d e f g h. a -> b -> c -> d -> e -> f -> g -> h -> Tuple8 a b c d e f g h

foreign import tuple9 :: forall a b c d e f g h i. a -> b -> c -> d -> e -> f -> g -> h -> i -> Tuple9 a b c d e f g h i

foreign import tuple10 :: forall a b c d e f g h i j. a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> Tuple10 a b c d e f g h i j

foreign import uncurry1 :: forall a z. (a -> z) -> Tuple1 a -> z

foreign import uncurry2 :: forall a b z. (a -> b -> z) -> Tuple2 a b -> z

foreign import uncurry3 :: forall a b c z. (a -> b -> c -> z) -> Tuple3 a b c -> z

foreign import uncurry4 :: forall a b c d z. (a -> b -> c -> d -> z) -> Tuple4 a b c d -> z

foreign import uncurry5 :: forall a b c d e z. (a -> b -> c -> d -> e -> z) -> Tuple5 a b c d e -> z

foreign import uncurry6 :: forall a b c d e f z. (a -> b -> c -> d -> e -> f -> z) -> Tuple6 a b c d e f -> z

foreign import uncurry7 :: forall a b c d e f g z. (a -> b -> c -> d -> e -> f -> g -> z) -> Tuple7 a b c d e f g -> z

foreign import uncurry8 :: forall a b c d e f g h z. (a -> b -> c -> d -> e -> f -> g -> h -> z) -> Tuple8 a b c d e f g h -> z

foreign import uncurry9 :: forall a b c d e f g h i z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> z) -> Tuple9 a b c d e f g h i -> z

foreign import uncurry10 :: forall a b c d e f g h i j z. (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> z) -> Tuple10 a b c d e f g h i j -> z

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

toNested6 :: forall a b c d e f. Tuple6 a b c d e f -> Nested.Tuple6 a b c d e f
toNested6 = uncurry6 Nested.tuple6

toNested7 :: forall a b c d e f g. Tuple7 a b c d e f g -> Nested.Tuple7 a b c d e f g
toNested7 = uncurry7 Nested.tuple7

toNested8 :: forall a b c d e f g h. Tuple8 a b c d e f g h -> Nested.Tuple8 a b c d e f g h
toNested8 = uncurry8 Nested.tuple8

toNested9 :: forall a b c d e f g h i. Tuple9 a b c d e f g h i -> Nested.Tuple9 a b c d e f g h i
toNested9 = uncurry9 Nested.tuple9

toNested10 :: forall a b c d e f g h i j. Tuple10 a b c d e f g h i j -> Nested.Tuple10 a b c d e f g h i j
toNested10 = uncurry10 Nested.tuple10

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

instance eqTuple6 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq (Tuple6 a b c d e f) where
  eq a b = uncurry6 (\a1 a2 a3 a4 a5 a6 ->
      (uncurry6 (\b1 b2 b3 b4 b5 b6 ->
        a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6)
      b))
    a

instance eqTuple7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq (Tuple7 a b c d e f g) where
  eq a b = uncurry7 (\a1 a2 a3 a4 a5 a6 a7 ->
      (uncurry7 (\b1 b2 b3 b4 b5 b6 b7 ->
        a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7)
      b))
    a

instance eqTuple8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Eq (Tuple8 a b c d e f g h) where
  eq a b = uncurry8 (\a1 a2 a3 a4 a5 a6 a7 a8 ->
      (uncurry8 (\b1 b2 b3 b4 b5 b6 b7 b8 ->
        a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7 && a8 == b8)
      b))
    a

instance eqTuple9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq (Tuple9 a b c d e f g h i) where
  eq a b = uncurry9 (\a1 a2 a3 a4 a5 a6 a7 a8 a9 ->
      (uncurry9 (\b1 b2 b3 b4 b5 b6 b7 b8 b9 ->
        a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7 && a8 == b8 && a9 == b9)
      b))
    a

instance eqTuple10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j) => Eq (Tuple10 a b c d e f g h i j) where
  eq a b = uncurry10 (\a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 ->
      (uncurry10 (\b1 b2 b3 b4 b5 b6 b7 b8 b9 b10 ->
        a1 == b1 && a2 == b2 && a3 == b3 && a4 == b4 && a5 == b5 && a6 == b6 && a7 == b7 && a8 == b8 && a9 == b9 && a10 == b10)
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

instance eq1Tuple6 :: (Eq a, Eq b, Eq c, Eq d, Eq e) => Eq1 (Tuple6 a b c d e) where
  eq1 = eq

instance eq1Tuple7 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f) => Eq1 (Tuple7 a b c d e f) where
  eq1 = eq

instance eq1Tuple8 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g) => Eq1 (Tuple8 a b c d e f g) where
  eq1 = eq

instance eq1Tuple9 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h) => Eq1 (Tuple9 a b c d e f g h) where
  eq1 = eq

instance eq1Tuple10 :: (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i) => Eq1 (Tuple10 a b c d e f g h i) where
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

instance showTuple6 :: (Show a, Show b, Show c, Show d, Show e, Show f) => Show (Tuple6 a b c d e f) where
  show = uncurry6 (\a b c d e f ->
    "(Tuple6 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> ")")

instance showTuple7 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g) => Show (Tuple7 a b c d e f g) where
  show = uncurry7 (\a b c d e f g ->
    "(Tuple7 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> ")")

instance showTuple8 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h) => Show (Tuple8 a b c d e f g h) where
  show = uncurry8 (\a b c d e f g h ->
    "(Tuple8 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> ")")

instance showTuple9 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i) => Show (Tuple9 a b c d e f g h i) where
  show = uncurry9 (\a b c d e f g h i ->
    "(Tuple9 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> ")")

instance showTuple10 :: (Show a, Show b, Show c, Show d, Show e, Show f, Show g, Show h, Show i, Show j) => Show (Tuple10 a b c d e f g h i j) where
  show = uncurry10 (\a b c d e f g h i j ->
    "(Tuple10 " <> show a <> " " <> show b <> " " <> show c <> " " <> show d <> " " <> show e <> " " <> show f <> " " <> show g <> " " <> show h <> " " <> show i <> " " <> show j <> ")")

instance bifunctorTuple2 :: Bifunctor Tuple2 where
  bimap f g tuple = tuple2 (f (fst tuple)) (g (snd tuple))

instance bitraversableTuple2 :: Bitraversable Tuple2 where
  bitraverse f g t = tuple2 <$> f (fst t) <*> g (snd t) 
  bisequence t = tuple2 <$> (fst t) <*> (snd t)

instance bifoldableTuple :: Bifoldable Tuple2 where
  bifoldMap f g t = f (fst t) <> g (snd t)
  bifoldr f g z t = f (fst t) (g (snd t) z)
  bifoldl f g z t = g (f z (fst t)) (snd t)

