:- module(hazard,
          [
              hazard/1,
              source/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

hazard(Hazard) :- rack_instance('HAZARD#HAZARD', Hazard).

source(Hazard, Source) :- rdf(Hazard, rack:'HAZARD#source', Source).
