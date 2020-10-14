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

:- module(float_equality, [float_equality/2]).

%! float_equality(+A, +B) is det.
%
%    Checks whether two floats A and B are "reasonably" equal for some
%    configurable epsilon.
float_equality(A, B) :-
    Delta is abs(A - B),
    Epsilon is 0.000000001,
    Delta @=< Epsilon.
