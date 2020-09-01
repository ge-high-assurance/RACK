:- module(hazard_structure, [hazard_structure/5]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(ontology(hazard)).
:- use_module(ontology(requirements)).
:- use_module(ontology(testing)).
:- use_module(rack(model)).
:- use_module(utils(transitive_closure)).

% val: reified because I don't know whether Prolog has anonymous partial application
requirements_satisfies(A, B) :- rdf(A, rack:'REQUIREMENTS#satisfies', B).

hazard_structure(Hazard, Source, LLR, HLR, TestCase) :-
    hazard(Hazard),
    rack_entity_instance(Source),
    requirement(LLR),
    requirement(HLR),
    test(TestCase),
    verifies(TestCase, HLR),
    transitive_closure(hazard_structure:requirements_satisfies, LLR, HLR),
    mitigates(HLR, Hazard),
    source(Hazard, Source).
