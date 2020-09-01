:- module(hazard_structure, [
              hazard_debug/3,
              hazard_structure/5
          ]).

:- consult('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module('../rack_model').
:- use_module(utils(transitive_closure)).

% val: reified because I don't know whether Prolog has anonymous partial application
requirements_satisfies(A, B) :- rdf(A, rack:'REQUIREMENTS#satisfies', B).

hazard_structure(Hazard, Source, LLR, HLR, TestCase) :-
    rack_instance('HAZARD#HAZARD', Hazard),
    rack_entity_instance(Source),
    rack_instance('REQUIREMENTS#REQUIREMENT', LLR),
    rack_instance('REQUIREMENTS#REQUIREMENT', HLR),
    rack_instance('TESTING#TEST', TestCase),
    rdf(TestCase, rack:'TESTING#verifies', HLR),
    transitive_closure(hazard_structure:requirements_satisfies, LLR, HLR),
    rdf(HLR, rack:'REQUIREMENTS#mitigates', Hazard),
    rdf(Hazard, rack:'HAZARD#source', Source).
