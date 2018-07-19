-- | Native Erlang tuples
module Erl.Data.Tuple where

import Prelude
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
