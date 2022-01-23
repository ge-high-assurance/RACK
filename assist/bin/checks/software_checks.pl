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

:- module(software_checks,
          [
              check_SOFTWARE/1
          ]).

:- ensure_loaded('../paths').
:- use_module(rack(model)).


%! check_SOFTWARE_partOf_SOFTWARE is det.
%
%    Checks every SOFTWARE partOf target is a SOFTWARE.
%    Always succeeds, emits warnings.
%
% Similar to "nodegroups/query/query dataVer SOFTWARE without partOf SOFTWARE.json"
%
check_SOFTWARE_COMPONENT_contained(I) :-
    check_has_no_rel('http://arcos.rack/SOFTWARE#SWCOMPONENT',
                     'http://arcos.rack/SOFTWARE#subcomponentOf',
                     'http://arcos.rack/SOFTWARE#SWCOMPONENT',
                     I).

%! check_SOFTWARE_COMPONENT_impact is det.
%
%    Checks every SOFTWARE partOf target is a SOFTWARE.
%    Always succeeds, emits warnings.
%
% Similar to "nodegroups/query/query dataVer SOFTWARE without partOf SOFTWARE.json"
%
check_SOFTWARE_COMPONENT_impact(I) :-
    check_has_no_rel('http://arcos.rack/SOFTWARE#SWCOMPONENT',
                     'http://arcos.rack/PROV-S#wasImpactedBy',
                     'http://arcos.rack/REQUIREMENTS#REQUIREMENT',
                     I).

%! check_SOFTWARE is det.
%
%    Performs all checks for SOFTWARE classes.  Always succeeds, emits warnings.
check_SOFTWARE(SC) :-
    check_SOFTWARE_COMPONENT_contained(SC);
    check_SOFTWARE_COMPONENT_impact(SC).
