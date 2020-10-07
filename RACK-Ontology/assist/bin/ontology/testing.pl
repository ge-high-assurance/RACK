% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(testing,
          [
            test/1,
            test_development/1,
            test_execution/1,
            test_result/1,
            test_status/1,
            confirms/2,
            executedBy/2,
            result/2,
            verifies/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

test(C) :- rack_instance('TESTING#TEST', C).
test_development(C) :- rack_instance('TESTING#TEST_DEVELOPMENT', C).
test_execution(C) :- rack_instance('TESTING#TEST_EXECUTION', C).
test_result(C) :- rack_instance('TESTING#TEST_RESULT', C).
test_status(C) :- rack_instance('TESTING#TEST_STATUS', C).

confirms(A, B) :- rdf(A, rack:'TESTING#confirms', B).
executedBy(A, B) :- rdf(A, rack:'TESTING#executedBy', B).
result(A, B) :- rdf(A, rack:'TESTING#result', B).
verifies(A, B) :- rdf(A, rack:'TESTING#verifies', B).
