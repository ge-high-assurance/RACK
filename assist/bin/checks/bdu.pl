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

% This module contains several checks relating to BDUs
% (Belief-Disbelief-Uncertainty metrics).  In particular, we may want to check:
%
% - that there is no incomplete triple,
% - that there are no duplicate assessments for a thing,
% - that all triples sum to 1.0.
%
% TODO (val) We will probably want to allow duplicate assessments as long as
% they are created as part of a different activity.

:- module(
    bdu,
    [
        sum_BDU/2,
        check_BDUs/0,
        check_BDUs_complete/0,
        check_BDUs_sums/0,
        check_BDUs_unique/0
    ]).

:- ensure_loaded('../paths').

:- use_module(ontology(confidence)).
:- use_module(rack(model)).
:- use_module(utils(float_equality)).

prolog:message(bad_BDU_sum(Thing, Sum)) -->
    [ 'CE-121: BDU sum for ~w is ~:f, expected 1.0'-[Thing, Sum] ].

prolog:message(multiple_beliefs(A, B1, B2)) -->
    [ 'CE-122: ~w has two (or more) belief values (such as ~w and ~w)'-[A, B1, B2] ].
prolog:message(multiple_disbeliefs(A, B1, B2)) -->
    [ 'CE-123: ~w has two (or more) disbelief values (such as ~w and ~w)'-[A, B1, B2] ].
prolog:message(multiple_uncertainties(A, B1, B2)) -->
    [ 'CE-124: ~w has two (or more) uncertainties values (such as ~w and ~w)'-[A, B1, B2] ].

prolog:message(no_belief(Thing)) -->
    [ 'CE-125: ~w has a disbelief or uncertainty value, but no belief'-[Thing] ].
prolog:message(no_disbelief(Thing)) -->
    [ 'CE-126: ~w has a belief or uncertainty value, but no disbelief'-[Thing] ].
prolog:message(no_uncertainty(Thing)) -->
    [ 'CE-127: ~w has a belief or disbelief value, but no uncertainty'-[Thing] ].

% Belief-Disbelief-Uncertainty metrics should:
% * have all three values
% * sum to 1.0

%! sum_BDU(+Thing, -Sum) is semidet.
%
%    Computes all sums of BDUs for a Thing that has at least one of each of the
%    three assessment types.  Succeeds multiple times in the presence of
%    duplicate assessments.
sum_BDU(Thing, Sum) :-
    belief(Thing, Belief),
    disbelief(Thing, Disbelief),
    uncertainty(Thing, Uncertainty),
    rdf_literal_val_type(Belief, B, _),
    rdf_literal_val_type(Disbelief, D, _),
    rdf_literal_val_type(Uncertainty, U, _),
    Sum is B + D + U.

%! check_BDUs_sums is det.
%
%    Checks that all BDUs sum to 1.0.  Always succeeds, emits warnings.
check_BDUs_sums :-
    forall(
        (sum_BDU(Thing, Sum), \+ float_equality(Sum, 1.0)),
        print_message(warning, bad_BDU_sum(Thing, Sum))
    ).

%! check_beliefs_unique is det.
%
%    Checks that there is at most one belief assessment for a thing.  Always
%    succeeds, emits warnings.
check_beliefs_unique :-
    forall(
        (belief(A, B), belief(A, C), \+ B = C),
        print_message(warning, multiple_beliefs(A, B, C))
    ).

%! check_disbeliefs_unique is det.
%
%    Checks that there is at most one disbelief assessment for a thing.  Always
%    succeeds, emits warnings.
check_disbeliefs_unique :-
    forall(
        (disbelief(A, B), disbelief(A, C), \+ B = C),
        print_message(warning, multiple_disbeliefs(A, B, C))
    ).

%! check_uncertainties_unique is det.
%
%    Checks that there is at most one uncertainty assessment for a thing.
%    Always succeeds, emits warnings.
check_uncertainties_unique :-
    forall(
        (uncertainty(A, B), uncertainty(A, C), \+ B = C),
        print_message(warning, multiple_uncertainties(A, B, C))
    ).

%! check_BDUs_unique is det.
%
%    Checks that all BDUs assessments for a thing are unique.  Always succeeds,
%    emits warnings.
check_BDUs_unique :-
    check_beliefs_unique,
    check_disbeliefs_unique,
    check_uncertainties_unique.

%! check_missing_belief is det.
%
%    Checks that no assessed thing is missing its belief assessment.  Always
%    succeeds, emits warnings.
check_missing_belief :-
    findall(
        Thing,
        (
            (disbelief(Thing, _) ; uncertainty(Thing, _)),
            \+ belief(Thing, _)
        ),
        Things
    ),
    forall(
        member(Thing, Things),
        print_message(warning, no_belief(Thing))
    ).

%! check_missing_disbelief is det.
%
%    Checks that no assessed thing is missing its disbelief assessment.  Always
%    succeeds, emits warnings.
check_missing_disbelief :-
    findall(
        Thing,
        (
            (belief(Thing, _) ; uncertainty(Thing, _)),
            \+ disbelief(Thing, _)
        ),
        Things
    ),
    forall(
        member(Thing, Things),
        print_message(warning, no_disbelief(Thing))
    ).

%! check_missing_uncertainty is det.
%
%    Checks that no assessed thing is missing its uncertainty assessment.
%    Always succeeds, emits warnings.
check_missing_uncertainty :-
    findall(
        Thing,
        (
            (belief(Thing, _) ; disbelief(Thing, _)),
            \+ uncertainty(Thing, _)
        ),
        Things
    ),
    forall(
        member(Thing, Things),
        print_message(warning, no_uncertainty(Thing))
    ).

%! check_BDUs_complete is det.
%
%    Checks that no assessed thing is missing some of its assessment.
%    Always succeeds, emits warnings.
check_BDUs_complete :-
    check_missing_belief,
    check_missing_disbelief,
    check_missing_uncertainty.

%! check_BDUs is det.
%
%    Performs all checks for BDUs.  Always succeeds, emits warnings.
check_BDUs :-
    check_BDUs_unique,
    check_BDUs_complete,
    check_BDUs_sums.
