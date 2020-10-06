% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(sacm-s,
          [
            argument_package/1,
            argument_reasoning/1,
            artifact_reference/1,
            asserted_context/1,
            asserted_inference/1,
            claim/1,
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

argument_package(C) :- rack_instance('SACM-S#ARGUMENT_PACKAGE', C).
argument_reasoning(C) :- rack_instance('SACM-S#ARGUMENT_REASONING', C).
artifact_reference(C) :- rack_instance('SACM-S#ARTIFACT_REFERENCE', C).
asserted_context(C) :- rack_instance('SACM-S#ASSERTED_CONTEXT', C).
asserted_inference(C) :- rack_instance('SACM-S#ASSERTED_INFERENCE', C).
claim(C) :- rack_instance('SACM-S#CLAIM', C).

