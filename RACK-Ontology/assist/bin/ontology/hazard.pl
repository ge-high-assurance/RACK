% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(hazard,
          [
            hazard/1,
            hazard_identification/1,
            definition/2,
            source/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

hazard(C) :- rack_instance('HAZARD#HAZARD', C).
hazard_identification(C) :- rack_instance('HAZARD#HAZARD_IDENTIFICATION', C).

definition(A, B) :- rdf(A, rack:'HAZARD#definition', B).
source(A, B) :- rdf(A, rack:'HAZARD#source', B).
