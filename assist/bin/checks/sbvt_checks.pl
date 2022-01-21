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

:- module(sbvt_checks,
          [
              check_SBVT/1
          ]).

:- ensure_loaded('../paths').
:- use_module(rack(model)).

%! check_Result_not_confirmed is det.
%
%    Checks that no SBVT_Result is lacking a confirming TEST.
%    Always succeeds, emits warnings.
check_Result_not_confirmed(I) :-
    check_has_no_rel('http://arcos.AH-64D/Boeing#SBVT_Result',
                     'http://arcos.rack/TESTING#confirms',
                     'http://arcos.rack/TESTING#TEST',
                     I).

%! check_no_Test_requirement is det.
%
%    Checks that no SBVT_Test is lacking a Requirement to verify.
%    Always succeeds, emits warnings.
check_no_Test_requirement(I) :-
    check_has_no_rel('http://arcos.AH-64D/Boeing#SBVT_Test',
                     'http://arcos.rack/TESTING#verifies',
                     I).

%! check_SBVT is det.
%
%    Performs all checks for SBVT classes.  Always succeeds, emits warnings.
check_SBVT(SBVT) :-
    check_Result_not_confirmed(SBVT).
    %% check_no_Test_requirement(SBVT).
