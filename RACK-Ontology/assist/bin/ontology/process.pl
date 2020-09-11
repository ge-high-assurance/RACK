% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(process,
          [
            objective/1,
            description/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

objective(C) :- rack_instance('PROCESS#OBJECTIVE', C).

description(A, B) :- rdf(A, rack:'PROCESS#description', B).
