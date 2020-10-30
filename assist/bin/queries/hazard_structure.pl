% Copyright (c) 2020, Galois, Inc.
%
% All Rights Reserved
%
% This material is based upon work supported by the Defense Advanced Research
% Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
%
% Any opinions, findings and conclusions or recommendations expressed in this
% material are those of the author(s) and do not necessarily reflect the views
% of the Defense Advanced Research Projects Agency (DARPA).

:- module(hazard_structure, [hazard_structure/5]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(ontology(hazard)).
:- use_module(ontology(requirements)).
:- use_module(ontology(testing)).
:- use_module(rack(model)).
:- use_module(utils(transitive_closure)).

hazard_structure(Hazard, Source, LLR, HLR, TestCase) :-
    hazard(Hazard),
    rack_entity_instance(Source),
    requirement(LLR),
    requirement(HLR),
    test(TestCase),
    verifies(TestCase, HLR),
    transitive_closure(requirements:satisfies, LLR, HLR),
    mitigates(HLR, Hazard),
    source(Hazard, Source).
