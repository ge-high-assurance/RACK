% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(analysis,
          [
            analysis/1,
            analysis_annotation/1,
            analysis_annotation_type/1,
            analysis_report/1,
            analysis_result/1,
            analyzes/2,
            annotationType/2,
            description/2,
            fromReport/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

analysis(C) :- rack_instance('ANALYSIS#ANALYSIS', C).
analysis_annotation(C) :- rack_instance('ANALYSIS#ANALYSIS_ANNOTATION', C).
analysis_annotation_type(C) :- rack_instance('ANALYSIS#ANALYSIS_ANNOTATION_TYPE', C).
analysis_report(C) :- rack_instance('ANALYSIS#ANALYSIS_REPORT', C).
analysis_result(C) :- rack_instance('ANALYSIS#ANALYSIS_RESULT', C).

analyzes(A, B) :- rdf(A, rack:'ANALYSIS#analyzes', B).
annotationType(A, B) :- rdf(A, rack:'ANALYSIS#annotationType', B).
description(A, B) :- rdf(A, rack:'ANALYSIS#description', B).
fromReport(A, B) :- rdf(A, rack:'ANALYSIS#fromReport', B).
