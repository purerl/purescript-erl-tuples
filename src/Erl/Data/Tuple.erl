-module(erl_data_tuple@foreign).
-export([tuple1/1, tuple2/2, tuple3/3, tuple4/4, tuple5/5,
  uncurry1/2, uncurry2/2, uncurry3/2, uncurry4/2, uncurry5/2]).

tuple1(A) -> {A}.
tuple2(A,B) -> {A,B}.
tuple3(A,B,C) -> {A,B,C}.
tuple4(A,B,C,D) -> {A,B,C,D}.
tuple5(A,B,C,D,E) -> {A,B,C,D,E}.

uncurry1(F,{A}) -> F(A).
uncurry2(F,{A,B}) -> (F(A))(B).
uncurry3(F,{A,B,C}) -> ((F(A))(B))(C).
uncurry4(F,{A,B,C,D}) -> (((F(A))(B))(C))(D).
uncurry5(F,{A,B,C,D,E}) -> ((((F(A))(B))(C))(D))(E).
