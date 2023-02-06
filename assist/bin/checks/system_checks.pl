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

:- module(system_checks,
          [
              check_SYSTEM/1
          ]).

:- ensure_loaded('../paths').
:- use_module(rack(model)).


%! check_SYSTEM_partOf_SYSTEM is det.
%
%    Checks every SYSTEM partOf target is a SYSTEM.
%    Always succeeds, emits warnings.
%
% Similar to "nodegroups/query/query dataVer SYSTEM without partOf SYSTEM.json"
%
check_SYSTEM_partOf_SYSTEM(I) :-
    check_has_no_rel('SYS1',
                     'http://arcos.rack/SYSTEM#SYSTEM',
                     'http://arcos.rack/SYSTEM#partOf',
                     'http://arcos.rack/SYSTEM#SYSTEM',
                     I).

%! check_SYSTEM is det.
%
%    Performs all checks for SYSTEM classes.  Always succeeds, emits warnings.
check_SYSTEM(SYS) :-
    check_SYSTEM_partOf_SYSTEM(SYS).
