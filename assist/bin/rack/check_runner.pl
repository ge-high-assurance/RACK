% Copyright (c) 2022, Galois, Inc.
%
% All Rights Reserved
%
% This material is based upon work supported by the Defense Advanced Research
% Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
%
% Any opinions, findings and conclusions or recommendations expressed in this
% material are those of the author(s) and do not necessarily reflect the views
% of the Defense Advanced Research Projects Agency (DARPA).

:- module(check_runner, [ run_checks/0 ]).

:- ensure_loaded('../paths').
:- use_module(checks(interfaceChecks)).
:- use_module(checks(sbvt_checks)).
:- use_module(checks(srs_checks)).
:- use_module(checks(system_checks)).
:- use_module(checks(software_checks)).


run_checks :-
    findall((C,N), runnable_check(C, N), CNS),
    format('~`.t Summary ~`.t~78|~n'),
    core_ontology_size(CoreSize),
    overlay_ontology_size(OverlaySize),
    num_instances(InstanceCount),
    format('Checked ~w core ontology classes, ~w overlay ontology classes, and ~w data instances~n',
           [CoreSize, OverlaySize, InstanceCount]),
    warn_all_nonzero(CNS),
    sum_all_nonzero(CNS, TotalIssues),
    (TotalIssues == 0,
     !,
     format('No issues found~n')
    ;
    format('~w ISSUES FOUND IN CHECK~n', [TotalIssues]), halt(1)),
    true.

% A check function shouldn't succeed unless there's a problem.  This
% returns the number of times the passed check function succeeds.
check_each_with(CheckFun, NFail) :-
    findall(C, call(CheckFun, C), CS),
    length(CS, NFail).


% Oh the checks we will run.
% Oh this will be fun.
% Run fun, run.
% Oh when will we be done.
runnable_check("definitions missing a Note/Description", Num) :-
    check_each_with(check_missing_notes, Num).
runnable_check("definitions not a subclass of PROV-S#THING", Num) :-
    check_each_with(check_not_prov_s, Num).
runnable_check("instance type issues", Num) :-
    check_each_with(check_instance_types, Num).
runnable_check("instance property issues", Num) :-
    check_each_with(check_instance_property_violations, Num).
runnable_check("INTERFACE issues", Num) :- check_each_with(check_INTERFACE, Num).
runnable_check("SBVT issues",      Num) :- check_each_with(check_SBVT,      Num).
runnable_check("SRS issues",       Num) :- check_each_with(check_SRS,       Num).
runnable_check("SYSTEM issues",    Num) :- check_each_with(check_SYSTEM,    Num).
runnable_check("SOFTWARE issues",  Num) :- check_each_with(check_SOFTWARE,  Num).
