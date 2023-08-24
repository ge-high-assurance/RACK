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

:- module(interfaceChecks,
          [
              check_INTERFACE/1
          ]).

:- ensure_loaded('../paths').
:- use_module(rack(model)).


%! check_INTERFACE_no_dest_SYSTEM is det.
%
%    Checks that no INTERFACE is lacking a destination SYSTEM.
%    Always succeeds, emits warnings.
%
% Similar to "nodegroups/query/query dataVer INTERFACE without destination SYSTEM.json"
%
check_INTERFACE_no_dest_SYSTEM(IFACE) :-
    check_has_no_rel('I1',
                     'http://arcos.rack/SYSTEM#INTERFACE',
                     'http://arcos.rack/SYSTEM#destination',
                     'http://arcos.rack/SYSTEM#SYSTEM', IFACE).


%! check_INTERFACE_no_src_SYSTEM is det.
%
%    Checks that no INTERFACE is lacking a source SYSTEM.
%    Always succeeds, emits warnings.
%
% Similar to "nodegroups/query/query dataVer INTERFACE without source SYSTEM.json"
%
check_INTERFACE_no_src_SYSTEM(IFACE) :-
    check_has_no_rel('I2',
                     'http://arcos.rack/SYSTEM#INTERFACE',
                     'http://arcos.rack/SYSTEM#source',
                     'http://arcos.rack/SYSTEM#SYSTEM', IFACE).

%! check_INTERFACE is det.
%
%    Performs all checks for INTERFACEs.  Always succeeds, emits warnings.
check_INTERFACE(IFACE).
%    check_INTERFACE_no_dest_SYSTEM(IFACE);
%    check_INTERFACE_no_src_SYSTEM(IFACE).
