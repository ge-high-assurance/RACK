:- module(testing,
          [
              test/1,
              verifies/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

test(R) :- rack_instance('TESTING#TEST', R).

verifies(TestCase, HLR) :- rdf(TestCase, rack:'TESTING#verifies', HLR).
