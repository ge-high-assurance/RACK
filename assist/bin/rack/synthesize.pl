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

:- module(
    synthesize,
    [
        synthesize/0
    ]
).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(library(yall)).

:- use_module(ontology(file),
    [
        assert_createBy/2,
        assert_file/1,
        assert_file_creation/1,
        assert_filename/2,
        file/1,
        file_creation/1
    ]
).

:- use_module(rack(model), [
    load_model_from_rack/0,
    property/3,
    property_target/5,
    rack_instance/2,
    rack_property_assert/3,
    rack_ref/2,
    rdf_literal_val_type/3,
    upload_model_to_rack/1
]).

:- use_module(rack(owl), [write_owl/3]).

:- use_module(ontology('prov-s'), [
    assert_identifier/2,
    assert_title/2,
    assert_wasDerivedFrom/2
]).

:- use_module(rack(write_ontology), [rack_property/1]).

% :- debug(triples).

char_between(MinChar, MaxChar, Char) :-
    char_code(MinChar, Min),
    char_code(MaxChar, Max),
    between(Min, Max, Code),
    char_code(Char, Code).

chars_between(MinChar, MaxChar, Codes) :-
    setof(Code, char_between(MinChar, MaxChar, Code), Codes).

all_decimal_characters(Chars) :- chars_between('0', '9', Chars).

all_lowercase_alphas(Chars) :- chars_between('a', 'z', Chars).

all_uppercase_alphas(Chars) :- chars_between('A', 'Z', Chars).

arbi_from_possibilities(Possibilities, Arbitrary) :-
    length(Possibilities, Length),
    random_between(1, Length, Index),
    nth1(Index, Possibilities, Arbitrary).

arbi_lowercase_alpha(Char) :-
    all_lowercase_alphas(Alphas), arbi_from_possibilities(Alphas, Char).
arbi_uppercase_alpha(Char) :-
    all_uppercase_alphas(Alphas), arbi_from_possibilities(Alphas, Char).

arbi_list(Size, Arbi, List) :-
    length(List, Size),
    foreach(
        (between(1, Size, Index), call(Arbi, Element)),
        nth1(Index, List, Element)
    ).

arbi_string(Length, ArbiChar, String) :-
    arbi_list(Length, ArbiChar, Chars),
    atomic_list_concat(Chars, String).

arbi_lowercase_alphas(Length, String) :-
    arbi_string(Length, arbi_lowercase_alpha, String).
arbi_uppercase_alphas(Length, String) :-
    arbi_string(Length, arbi_uppercase_alpha, String).

arbi_filename(Filename) :-
    arbi_lowercase_alphas(10, Prefix),
    arbi_lowercase_alphas(3, Extension),
    atomic_list_concat([Prefix, ".", Extension], Filename).

arbi_file_identifier(Identifier) :-
    arbi_uppercase_alphas(20, Name),
    atomic_list_concat(["FILE_", Name], Identifier).

%! arbi_rack_thing(+Prefix:str, +AssertRackClass:pred, -Atom:atom).
%
% Creates and registers an arbitrary RACK PROV-S#THING, using the given
% 'AssertRackClass' predicate to actually register the instance.  It is expected
% that the 'AssertRackClass' is a call to 'rack_instance_assert' with a class
% that is a type of THING.
%
% A random identifier made from the 'Prefix' and some random string will be
% created, and an atom will be created from it and returned as 'Atom'.
arbi_rack_thing(Prefix, AssertRackClass, Atom) :-
    arbi_uppercase_alphas(20, Name),
    atomic_list_concat([Prefix, "_", Name], Identifier),
    atom_string(Atom, Identifier),
    call(AssertRackClass, Atom),
    assert_identifier(Atom, Identifier).

arbi_file(Atom) :- arbi_rack_thing('FILE', assert_file, Atom).

arbi_file_creation(Atom) :- arbi_rack_thing('FILE_CREATION', assert_file_creation, Atom).

% temp copied from sadl.pl FIXME
set_graph_for_file(File, Graph) :-
    file_base_name(File, Base),
    file_name_extension(Graph,_,Base),
    rdf_create_graph(Graph),
    rdf_default_graph(_, Graph).

% This API is awful.  Seems like this needs to run to completion before any
% `rdf_assert` for that prefix, and it also seems to need to be in the same
% module?
% :- rdf_register_prefix(syn, 'http://synthetic.data/', [force(true)]).

prolog:message(make_chain_exhausted_candidates(Node, Remaining)) -->
    [ 'make_chain exhausted the list of candidates for ~w, needs ~d additional values.'
    - [ Node, Remaining ]
    ].

make_chain_among(_, Edge, 1, Source, Target, _) :-
    !, call(Edge, Source, Target).
make_chain_among(Node, _, NumberOfIntermediates, _, _, []) :-
    NumberOfIntermediates > 1,
    print_message(error, make_chain_exhausted_candidates(Node, NumberOfIntermediates)),
    !, fail.
make_chain_among(Node, Edge, NumberOfIntermediates, Source, Target, Candidates) :-
    NumberOfIntermediates > 1,
    random_member(Intermediate, Candidates),
    subtract(Candidates, [Intermediate], RemainingCandidates),
    call(Edge, Source, Intermediate),
    Rest is NumberOfIntermediates - 1,
    make_chain_among(Node, Edge, Rest, Intermediate, Target, RemainingCandidates).

make_chain(Node, Edge, NumberOfIntermediates, Source, Target) :-
    bagof(N, call(Node, N), AllNodes),
    subtract(AllNodes, [Source, Target], Candidates),
    make_chain_among(Node, Edge, NumberOfIntermediates, Source, Target, Candidates).

was_derived_from_chain(0, SourceFile, TargetFile) :-
    !, assert_wasDerivedFrom(SourceFile, TargetFile).
was_derived_from_chain(Size, SourceFile, TargetFile) :-
    Size > 0,
    arbi_file(File),
    assert_file(File),
    assert_wasDerivedFrom(SourceFile, File),
    Rest is Size - 1,
    was_derived_from_chain(Rest, File, TargetFile).

synthesize_asserts_for_property_aux(0, _, _, _, _).
synthesize_asserts_for_property_aux(HowMany, Sources, AssertProperty, Targets, Avoid) :-
    HowMany > 0,
    member(Source, Sources),
    member(Target, Targets),
    \+ member([Source, Target], Avoid),
    !,
    call(AssertProperty, Source, Target),
    NewAvoid = [[Source, Target] | Avoid],
    Rest is HowMany - 1,
    synthesize_asserts_for_property_aux(Rest, Sources, AssertProperty, Targets, NewAvoid).

%! synthesize_asserts_for_property(
%      +HowMany:int,
%      +QuerySource:pred,
%      +AssertProperty:pred,
%      +QueryTarget: pred
%  ).
%
% Synthesizes a given amount 'HowMany' edges in the model.  The edge will be
% created by calling 'AssertProperty' (which we expect to be a wrapper around
% 'rack_property_assert'). Source and target will be sampled from the values
% predicated by 'QuerySource' and 'QueryTarget' respectively.
%
% At most one instance of an edge between a given source and a given target will
% be synthesized, but multiple sources may get an edge to the same target, and
% vice-versa.
synthesize_asserts_for_property(HowMany, QuerySource, AssertProperty, QueryTarget) :-
    bagof(Source, call(QuerySource, Source), Sources),
    bagof(Target, call(QueryTarget, Target), Targets),
    synthesize_asserts_for_property_aux(HowMany, Sources, AssertProperty, Targets, []).

prolog:message(synthesize_disjoint_ran_out_of_sources(Source, Remaining)) -->
    [ 'synthesize_disjoint_asserts_for_property exhausted the list of sources from ~w, needs ~d additional values.'
    - [ Source, Remaining ]
    ].
prolog:message(synthesize_disjoint_ran_out_of_targets(Target, Remaining)) -->
    [ 'synthesize_disjoint_asserts_for_property exhausted the list of targets from ~w, needs ~d additional values.'
    - [ Target, Remaining ]
    ].

synthesize_disjoint_asserts_for_property_aux(_, _, 0, _, _, _) :- !.
synthesize_disjoint_asserts_for_property_aux(QuerySource, _, HowMany, [], _, _) :-
    HowMany > 0, !,
    print_message(error, synthesize_disjoint_ran_out_of_sources(QuerySource, HowMany)),
    fail.
synthesize_disjoint_asserts_for_property_aux(_, QueryTarget, HowMany, _, _, []) :-
    HowMany > 0, !,
    print_message(error, synthesize_disjoint_ran_out_of_targets(QueryTarget, HowMany)),
    fail.
synthesize_disjoint_asserts_for_property_aux(
    QuerySource, QueryTarget, HowMany, Sources, AssertProperty, Targets
) :-
    HowMany > 0,
    % pick a source and target
    member(Source, Sources),
    member(Target, Targets),
    % remove them from their respective candidate pool
    subtract(Sources, [Source], RemainingSources),
    subtract(Targets, [Target], RemainingTargets),
    !,
    % register them, then recurse for the rest
    call(AssertProperty, Source, Target),
    Rest is HowMany - 1,
    synthesize_disjoint_asserts_for_property_aux(
        QuerySource, QueryTarget, Rest, RemainingSources, AssertProperty, RemainingTargets
    ).

%! synthesize_disjoint_asserts_for_property(+HowMany:int, +Property:atom).
%
% Synthesizes a given amount 'HowMany' edges in the model.  The edge will be
% created by calling 'AssertProperty' (which we expect to be a wrapper around
% 'rack_property_assert'). Source and target will be sampled from the values
% predicated by 'QuerySource' and 'QueryTarget' respectively.
%
% At most one edge coming out of a source will be synthesized, likewise, at most
% one edge going into a target will be synthesized.
synthesize_disjoint_asserts_for_property(HowMany, Property) :-
    rack_domain(Property, Domain),
    rack_range(Property, Range),
    bagof(Source, rdf(Source, rdf:type, Domain), Sources),
    bagof(Target, rdf(Target, rdf:type, Range), Targets),
    synthesize_disjoint_asserts_for_property_aux(
        Domain, Range, HowMany, Sources, [A, B]>>rack_property_assert(A, Property, B), Targets
    ).

rack_domain(OntologyClassName, Domain) :-
    rack_ref(OntologyClassName, Ref),
    rdf(Ref, rdfs:domain, Domain).

rack_range(OntologyClassName, Range) :-
    rack_ref(OntologyClassName, Ref),
    rdf(Ref, rdfs:range, Range).

synthesize() :-
    % rdf_retractall(_, _, _), % delete all data in RACK
    load_model_from_rack(),
    arbi_list(1_000, arbi_file, _),
    arbi_list(1_000, arbi_file_creation, _),
    synthesize_disjoint_asserts_for_property(100, 'FILE#createBy').
    % rdf(rack:'FILE#createBy', rdfs:domain, Domain),
    % set_graph_for_file(File, _Graph),
    % arbi_list(100, arbi_file, [FileA, FileB | _MoreFiles]),
    % make_chain(file, assert_wasDerivedFrom, 10, FileA, FileB),
    % write_owl(File, c, [syn]).
    % upload_model_to_rack('http://rack001/data').
