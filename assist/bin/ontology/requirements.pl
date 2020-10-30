% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(requirements,
          [
            data_dictionary_term/1,
            requirement/1,
            requirement_development/1,
            consumedBy/2,
            governs/2,
            mitigates/2,
            providedBy/2,
            satisfies/2,
            text/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

data_dictionary_term(C) :- rack_instance('REQUIREMENTS#DATA_DICTIONARY_TERM', C).
requirement(C) :- rack_instance('REQUIREMENTS#REQUIREMENT', C).
requirement_development(C) :- rack_instance('REQUIREMENTS#REQUIREMENT_DEVELOPMENT', C).

consumedBy(A, B) :- rdf(A, rack:'REQUIREMENTS#consumedBy', B).
governs(A, B) :- rdf(A, rack:'REQUIREMENTS#governs', B).
mitigates(A, B) :- rdf(A, rack:'REQUIREMENTS#mitigates', B).
providedBy(A, B) :- rdf(A, rack:'REQUIREMENTS#providedBy', B).
satisfies(A, B) :- rdf(A, rack:'REQUIREMENTS#satisfies', B).
text(A, B) :- rdf(A, rack:'REQUIREMENTS#text', B).
