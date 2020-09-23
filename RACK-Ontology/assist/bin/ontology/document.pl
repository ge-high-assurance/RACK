% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README

:- module(document,
          [
            description/1,
            doc_status/1,
            plan/1,
            procedure/1,
            report/1,
            request/1,
            section/1,
            specification/1,
            approvalAuthority/2,
            content/2,
            dateOfIssue/2,
            issuingOrganization/2,
            status/2,
            title/2
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

description(C) :- rack_instance('DOCUMENT#DESCRIPTION', C).
doc_status(C) :- rack_instance('DOCUMENT#DOC_STATUS', C).
plan(C) :- rack_instance('DOCUMENT#PLAN', C).
procedure(C) :- rack_instance('DOCUMENT#PROCEDURE', C).
report(C) :- rack_instance('DOCUMENT#REPORT', C).
request(C) :- rack_instance('DOCUMENT#REQUEST', C).
section(C) :- rack_instance('DOCUMENT#SECTION', C).
specification(C) :- rack_instance('DOCUMENT#SPECIFICATION', C).

approvalAuthority(A, B) :- rdf(A, rack:'DOCUMENT#approvalAuthority', B).
content(A, B) :- rdf(A, rack:'DOCUMENT#content', B).
dateOfIssue(A, B) :- rdf(A, rack:'DOCUMENT#dateOfIssue', B).
issuingOrganization(A, B) :- rdf(A, rack:'DOCUMENT#issuingOrganization', B).
status(A, B) :- rdf(A, rack:'DOCUMENT#status', B).
title(A, B) :- rdf(A, rack:'DOCUMENT#title', B).
