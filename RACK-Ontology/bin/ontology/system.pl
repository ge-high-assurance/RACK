% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(system,
          [
            interface/1,
            system/1,
            system_development/1,
            destination/2,
            partOf/2,
            source/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

interface(C) :- rack_instance('SYSTEM#INTERFACE', C).
system(C) :- rack_instance('SYSTEM#SYSTEM', C).
system_development(C) :- rack_instance('SYSTEM#SYSTEM_DEVELOPMENT', C).

destination(A, B) :- rdf(A, rack:'SYSTEM#destination', B).
partOf(A, B) :- rdf(A, rack:'SYSTEM#partOf', B).
source(A, B) :- rdf(A, rack:'SYSTEM#source', B).
