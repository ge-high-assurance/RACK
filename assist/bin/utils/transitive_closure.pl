% Copyright (c) 2020, Galois, Inc.
%
% All Rights Reserved
%
% This material is based upon work supported by the Defense Advanced Research
% Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
%
% Any opinions, findings and conclusions or recommendations expressed in this
% material are those of the author(s) and do not necessarily reflect the views
% of the Defense Advanced Research Projects Agency (DARPA).

:- module(transitive_closure, [transitive_closure/3]).

%! transitive_closure(+R, ?A, ?B) is nondet.
%
%    Acts as the transitive closure of a binary relation R, that is, it succeeds
%    whenever R(A, B) would, or when there exists a sequence of intermediate
%    atoms I_0 through I_n (for some n) such that R(A, I_0), R(I_i, I_i+1), and
%    R(I_n, B).
transitive_closure(R, A, B) :- call(R, A, B).
transitive_closure(R, A, C) :- call(R, A, B), transitive_closure(R, B, C).
