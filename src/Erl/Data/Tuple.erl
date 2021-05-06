-module(erl_data_tuple@foreign).
-export([tuple1/1, tuple2/2, tuple3/3, tuple4/4, tuple5/5, tuple6/6, tuple7/7, tuple8/8, tuple9/9, tuple10/10,
         uncurry1/2, uncurry2/2, uncurry3/2, uncurry4/2, uncurry5/2, uncurry6/2, uncurry7/2, uncurry8/2, uncurry9/2, uncurry10/2,
         fst/1, snd/1]).

tuple1(A) -> {A}.
tuple2(A,B) -> {A,B}.
tuple3(A,B,C) -> {A,B,C}.
tuple4(A,B,C,D) -> {A,B,C,D}.
tuple5(A,B,C,D,E) -> {A,B,C,D,E}.
tuple6(A,B,C,D,E,F) -> {A,B,C,D,E,F}.
tuple7(A,B,C,D,E,F,G) -> {A,B,C,D,E,F,G}.
tuple8(A,B,C,D,E,F,G,H) -> {A,B,C,D,E,F,G,H}.
tuple9(A,B,C,D,E,F,G,H,I) -> {A,B,C,D,E,F,G,H,I}.
tuple10(A,B,C,D,E,F,G,H,I,J) -> {A,B,C,D,E,F,G,H,I,J}.

uncurry1(F,{A}) -> F(A).
uncurry2(F,{A,B}) -> (F(A))(B).
uncurry3(F,{A,B,C}) -> ((F(A))(B))(C).
uncurry4(F,{A,B,C,D}) -> (((F(A))(B))(C))(D).
uncurry5(F,{A,B,C,D,E}) -> ((((F(A))(B))(C))(D))(E).
uncurry6(F,{A,B,C,D,E,F}) -> (((((F(A))(B))(C))(D))(E))(F).
uncurry7(F,{A,B,C,D,E,F,G}) -> ((((((F(A))(B))(C))(D))(E))(F))(G).
uncurry8(F,{A,B,C,D,E,F,G,H}) -> (((((((F(A))(B))(C))(D))(E))(F))(G))(H).
uncurry9(F,{A,B,C,D,E,F,G,H,I}) -> ((((((((F(A))(B))(C))(D))(E))(F))(G))(H))(I).
uncurry10(F,{A,B,C,D,E,F,G,H,I,J}) -> (((((((((F(A))(B))(C))(D))(E))(F))(G))(H))(I))(J).

fst({A, _B}) -> A.

snd({_A, B}) -> B.
