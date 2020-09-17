% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(confidence,
          [
            assessing_confidence/1,
            bdu_confidence_assessment/1,
            confidence_assessment/1,
            assesses/2,
            belief/2,
            createBy/2,
            disbelief/2,
            uncertainty/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

assessing_confidence(C) :- rack_instance('CONFIDENCE#ASSESSING_CONFIDENCE', C).
bdu_confidence_assessment(C) :- rack_instance('CONFIDENCE#BDU_CONFIDENCE_ASSESSMENT', C).
confidence_assessment(C) :- rack_instance('CONFIDENCE#CONFIDENCE_ASSESSMENT', C).

assesses(A, B) :- rdf(A, rack:'CONFIDENCE#assesses', B).
belief(A, B) :- rdf(A, rack:'CONFIDENCE#belief', B).
createBy(A, B) :- rdf(A, rack:'CONFIDENCE#createBy', B).
disbelief(A, B) :- rdf(A, rack:'CONFIDENCE#disbelief', B).
uncertainty(A, B) :- rdf(A, rack:'CONFIDENCE#uncertainty', B).
