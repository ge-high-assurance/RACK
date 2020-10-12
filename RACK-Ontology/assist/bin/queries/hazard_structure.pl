% Copyright (c) 2020, General Electric Company and Galois, Inc.
:- module(hazard_structure, [hazard_structure/5]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(ontology(hazard)).
:- use_module(ontology(requirements)).
:- use_module(ontology(testing)).
:- use_module(rack(model)).
:- use_module(utils(transitive_closure)).

hazard_structure(Hazard, Source, LLR, HLR, TestCase) :-
    hazard(Hazard),
    rack_entity_instance(Source),
    requirement(LLR),
    requirement(HLR),
    test(TestCase),
    verifies(TestCase, HLR),
    transitive_closure(requirements:satisfies, LLR, HLR),
    mitigates(HLR, Hazard),
    source(Hazard, Source).
