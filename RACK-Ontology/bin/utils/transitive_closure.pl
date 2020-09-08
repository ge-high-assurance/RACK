:- module(transitive_closure, [transitive_closure/3]).

%! transitive_closure(+R, ?A, ?B) is nondet.
%
%    Acts as the transitive closure of a binary relation R, that is, it succeeds
%    whenever R(A, B) would, or when there exists a sequence of intermediate
%    atoms I_0 through I_n (for some n) such that R(A, I_0), R(I_i, I_i+1), and
%    R(I_n, B).
transitive_closure(R, A, B) :- call(R, A, B).
transitive_closure(R, A, C) :- call(R, A, B), transitive_closure(R, B, C).
