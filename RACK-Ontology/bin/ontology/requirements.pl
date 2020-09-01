:- module(requirements,
          [
              requirement/1,
              mitigates/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

requirement(R) :- rack_instance('REQUIREMENTS#REQUIREMENT', R).

mitigates(HLR, Hazard) :- rdf(HLR, rack:'REQUIREMENTS#mitigates', Hazard).
