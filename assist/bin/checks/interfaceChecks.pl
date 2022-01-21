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

:- module(interfaceChecks,
          [
              check_INTERFACE/1
          ]).

something_is_true.

:- ensure_loaded('../paths').

:- use_module(rack(model)).

prolog:message(missing_dest_system(IFace, Name)) -->
    [ 'INTERFACE ~w (~w) has no destination SYSTEM'-[IFace, Name] ].
prolog:message(missing_src_system(IFace, Name)) -->
    [ 'INTERFACE ~w (~w) has no source SYSTEM'-[IFace, Name] ].

get_interface_instance(IFACE) :-
    rdf_reachable(C, rdf:subClass, 'http://arcos.rack/SYSTEM#INTERFACE'),
    rdf(IFACE, rdf:type, C).

interface_ident(IFACE, Name) :-
    rdf(IFACE, 'http://arcos.rack/PROV-S#identifier', Name), !.
interface_ident(_, "<unknown>").

no_dest_system(IFACE) :-
    rdf(IFACE, 'http://arcos.rack/SYSTEM#destination', _), !, fail.
no_dest_system(_).

no_src_system(IFACE) :-
    rdf(IFACE, 'http://arcos.rack/SYSTEM#source', _), !, fail.
no_src_system(_).

%! check_INTERFACE_no_dest_SYSTEM is det.
%
%    Checks that no INTERFACE is lacking a destination SYSTEM.
%    Always succeeds, emits warnings.
check_INTERFACE_no_dest_SYSTEM(IFACE) :-
    get_interface_instance(IFACE),
    no_dest_system(IFACE),
    interface_ident(IFACE, Name),
    print_message(warning, missing_dest_system(IFACE, Name)).

%! check_INTERFACE_no_src_SYSTEM is det.
%
%    Checks that no INTERFACE is lacking a source SYSTEM.
%    Always succeeds, emits warnings.
check_INTERFACE_no_src_SYSTEM(IFACE) :-
    get_interface_instance(IFACE),
    no_src_system(IFACE),
    interface_ident(IFACE, Name),
    print_message(warning, missing_src_system(IFACE, Name)).

%! check_INTERFACE is det.
%
%    Performs all checks for INTERFACEs.  Always succeeds, emits warnings.
check_INTERFACE(IFACE) :-
    check_INTERFACE_no_dest_SYSTEM(IFACE);
    check_INTERFACE_no_src_SYSTEM(IFACE).
