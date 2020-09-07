% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(agents,
          [
            organization/1,
            person/1,
            softwareagent/1,
            name/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

organization(C) :- rack_instance('AGENTS#ORGANIZATION', C).
person(C) :- rack_instance('AGENTS#PERSON', C).
softwareagent(C) :- rack_instance('AGENTS#SOFTWAREAGENT', C).

name(A, B) :- rdf(A, rack:'AGENTS#name', B).
