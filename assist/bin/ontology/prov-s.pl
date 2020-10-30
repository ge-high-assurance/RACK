% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(prov-s,
          [
            activity/1,
            agent/1,
            attribute/1,
            collection/1,
            entity/1,
            field/1,
            thing/1,
            dataInsertedBy/2,
            field/2,
            fieldValue/2,
            hasAttribute/2,
            startedAtTime/2,
            identifier/2,
            wasAssociatedWith/2,
            wasGeneratedBy/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

activity(C) :- rack_instance('PROV-S#ACTIVITY', C).
agent(C) :- rack_instance('PROV-S#AGENT', C).
attribute(C) :- rack_instance('PROV-S#ATTRIBUTE', C).
collection(C) :- rack_instance('PROV-S#COLLECTION', C).
entity(C) :- rack_instance('PROV-S#ENTITY', C).
field(C) :- rack_instance('PROV-S#FIELD', C).
thing(C) :- rack_instance('PROV-S#THING', C).

dataInsertedBy(A, B) :- rdf(A, rack:'PROV-S#dataInsertedBy', B).
field(A, B) :- rdf(A, rack:'PROV-S#field', B).
fieldValue(A, B) :- rdf(A, rack:'PROV-S#fieldValue', B).
hasAttribute(A, B) :- rdf(A, rack:'PROV-S#hasAttribute', B).
startedAtTime(A, B) :- rdf(A, rack:'PROV-S#startedAtTime', B).
identifier(A, B) :- rdf(A, rack:'PROV-S#identifier', B).
wasAssociatedWith(A, B) :- rdf(A, rack:'PROV-S#wasAssociatedWith', B).
wasGeneratedBy(A, B) :- rdf(A, rack:'PROV-S#wasGeneratedBy', B).
