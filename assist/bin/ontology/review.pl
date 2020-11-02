% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(review,
          [
            review/1,
            review_log/1,
            review_state/1,
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

review(C) :- rack_instance('REVIEW#REVIEW', C).
review_log(C) :- rack_instance('REVIEW#REVIEW_LOG', C).
review_state(C) :- rack_instance('REVIEW#REVIEW_STATE', C).

