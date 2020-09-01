:- module(transitive_closure, [transitive_closure/3]).

transitive_closure(R, A, B) :- call(R, A, B).
transitive_closure(R, A, C) :- call(R, A, B), transitive_closure(R, B, C).
