% Copyright (c) 2020, General Electric Company and Galois, Inc.
:- module(float_equality, [float_equality/2]).

%! float_equality(+A, +B) is det.
%
%    Checks whether two floats A and B are "reasonably" equal for some
%    configurable epsilon.
float_equality(A, B) :-
    Delta is abs(A - B),
    Epsilon is 0.000000001,
    Delta @=< Epsilon.
