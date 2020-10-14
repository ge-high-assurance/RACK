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

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

report(Opts) :-
    rdf_equal(rack:'PROV-S#THING', Thing),
    rdf(Thing, rdf:type, _), !, % verify the ontology is loaded
    rack_nodes(Nodes),
    (member(hide_ontology(true), Opts), !; report_rack_nodes(Nodes)),
    format('~n~n'),
    instance_prefixes(Prefixes),
    report_instance_prefixes(Prefixes).

report(_) :-
    rdf_equal(rack:'PROV-S#THING', Thing),
    \+ rdf(Thing, rdf:type, _),
    print_message(error, no_ontology_loaded).

report(InstancePfx, Opts) :-
    report(Opts),
    format('~n~n'),
    report_instances_in(InstancePfx),
    report_instances_in_summary(InstancePfx).

prolog:message(no_ontology_loaded) -->
    [ 'No ontology has been loaded for analysis.'-[] ].

% ----------------------------------------------------------------------

rack_nodes(Nodes) :-
    setof(E, T^Area^Item^(rdf(E, rdf:type, T),
                          (rdf_reachable(T, rdfs:subClassOf, owl:'Class');
                           rdf_reachable(T, rdf:type, rdfs:'Datatype')),
                          rack_ontology_node(E, Area, Item)),
          Nodes).

report_rack_nodes(Nodes) :-
    length(Nodes, N),
    format('~`#t Ontology Nodes (~:d) ~`#t~78|~n', [N]),
    report_rack_nodes_(Nodes).

report_rack_nodes_([]).
report_rack_nodes_([E|ES]) :-
    report_rack_node(E),
    report_rack_nodes_(ES).

report_rack_node(E) :-
    rdf(E, rdf:type, rdfs:'Datatype'),
    rdf(E, owl:equivalentClass, Equiv),
    rdf(Equiv, owl:onDatatype, BaseTy), !,
    prefix_shorten(E, SE),
    prefix_shorten(BaseTy, SBT),
    format('~n~w  :: Datatype of ~w~n', [SE, SBT]),
    report_datatype_restrictions(Equiv).
report_rack_node(E) :-
    rdf(E, rdf:type, T),
    class_comment(E, C),
    prefix_shorten(E, SE),
    prefix_shorten(T, ST),
    subclass_path(E, PPath),
    format('~n~w~w  :: ~w~n  ~w~n', [PPath, SE, ST, C]),
    report_rack_properties(E),
    report_rack_subclasses(E),
    report_instance_counts(E).


report_datatype_restrictions(Equiv) :-
    rdf(Equiv, owl:withRestrictions, Restrs), !,
    rdf_list(Restrs, RList),
    member(R, RList),
    report_datatype_restriction(R).
report_datatype_restrictions(_).

report_datatype_restriction(R) :-
    rdf(R, xsd:minInclusive, MinR),
    rdf(R, xsd:maxInclusive, MaxR),
    rdf_equal(Min^^T, MinR),
    rdf_equal(Max^^T, MaxR),
    format('    restriction: [~w..~w]~n', [Min, Max]).


class_comment(Class, Comment) :- rdf(Class, rdfs:comment, Comment), !.
class_comment(_, "?").

report_rack_properties(E) :-
    findall(P, property_target(E, P, _, _, _), AllP),
    sort(AllP, SortP),
    findall(P, (member(P, SortP),
                report_rack_properties(E, P)), _PS).

report_rack_properties(E, P) :-
    property_target(E, P, PType, T, Extra),
    show_property(E, P, PType, T, Extra).

show_property(_E, P, unique, T, Extra) :-
    rack_ref(ST, T), !,
    prefix_shorten(P, SP),
    show_property_extra(Extra, "---", SE),
    format('   . ~w ~`-t~w-> ~w~79|~n', [SP, SE, ST]).
show_property(_E, P, shared, T, Extra) :-
    rack_ref(ST, T), !,
    prefix_shorten(P, SP),
    show_property_extra(Extra, "---", SE),
    format('  *. ~w ~`-t~w-> ~w~79|~n', [SP, SE, ST]).
show_property(_E, P, shared, T, Extra) :-
    prefix_shorten(P, SP),
    prefix_shorten(T, ST),
    show_property_extra(Extra, "", SE),
    format('  *. ~w ~w :: ~w~n', [SP, SE, ST]).
show_property(_E, P, unique, T, Extra) :-
    prefix_shorten(P, SP),
    prefix_shorten(T, ST),
    show_property_extra(Extra, "", SE),
    format('   . ~w ~w :: ~w~n', [SP, SE, ST]).

show_property_extra(cardinality(N), _, NS) :-
    atom_concat('(', N, N1), atom_concat(N1, ')', NS).
show_property_extra(maybe, _, '(0/1)').
show_property_extra(normal, D, D).

subclass_path(E, Path) :-
    parent_path(E, RPath),
    reverse(RPath, ES),
    length(ES, ESLen),
    (ESLen > 0, !,
     atomics_to_string(ES, '.', PPath),
     string_concat(PPath, '.', Path) ;
     Path = "").
parent_path(E, [R|RS]) :-
    rdf(E, rdfs:subClassOf, LR),
    % Some nodes have multiple parent classes; the additional classes
    % are blank nodes with (for example) cardinality restrictions.
    \+ rdf_bnode(LR),
    prefix_shorten(LR, R),
    parent_path(LR, RS).
parent_path(E, []) :- rdf_subject(E), \+ rdf(E, rdfs:subClassOf, _).

% ----------------------------------------------------------------------

report_rack_subclasses(E) :-
    findall(S, rdf(S, rdfs:subClassOf, E), AllS),
    sort(AllS, SortS),
    findall(S, (member(S, SortS),
                report_rack_subclasses(E, S)), _).

report_rack_subclasses(E, S) :-
    rdf(S, rdfs:subClassOf, E),
    rdf(S, rdf:type, T),
    prefix_shorten(T, ST),
    format('  ^-- ~w  ~`.t~52| :: ~w~n', [S, ST]).

report_instance_counts(E) :-
    findall(I, rdf(I, rdf:type, E), IS),
    sort(IS, SIS),
    report_instance_counts(E, SIS).

report_instance_counts(E, IS) :-
    length(IS, ISLen),
    (ISLen == 0, !, true;
     setof(NS, L^X^(member(X, IS), rdf_split_url(NS, L, X)), NSL),
     findall(RNS, report_instance_counts(E, IS, NSL, RNS), _RNSL)).

report_instance_counts(E, _IS, NSL, CNS) :-
    member(CNS, NSL),
    findall(NSX, (rdf(NSX, rdf:type, E), rdf_split_url(CNS, _, NSX)), NSXS),
    length(NSXS, N),
    format('  () of ~d instances in ~w~n', [N, CNS]),
    report_rack_instances(E, CNS, N, 20).

report_rack_instances(E, Pfx, Count, Limit) :-
    findall(I, (rdf(I, rdf:type, E),
                rack_ontology_node(I, _, _),
                rdf_split_url(Pfx, _, I)
               ), RACK_IS),
    ((Count >= Limit, !,
      format('    = MANY [use -i ~w to view]~n', [Pfx]));
     sort(RACK_IS, S_RACK_IS),
     findall(I, report_rack_instance(S_RACK_IS, I), _IS)).

report_rack_instance(RACK_IS, I) :-
    member(I, RACK_IS),
    format('    = ~w~n', [I]).

instance_prefixes(Prefixes) :-
    setof(P, E^T^I^L^PL^PB^
             (rdf(E, rdf:type, T),
              rdf_reachable(T, rdfs:subClassOf, owl:'Class'),
              rdf(I, rdf:type, E),
              \+ rack_ontology_node(I, _, _),
              \+ rdf_bnode(I),
              rdf_split_url(PL, L, I),
              atomic_list_concat([PB|_],'#',PL),
              atom_concat(PB,'#',P)
             ),
          Prefixes).

report_instance_prefixes(Prefixes) :-
    length(Prefixes, N),
    format('~`#t Instance Prefixes (~:d) ~`#t~78|~n', [N]),
    findall(P, (member(P, Prefixes), report_instance_prefix(P)), _).

report_instance_prefix(Prefix) :-
    findall(I, (rdf(I, rdf:type, E),
                rdf(E, rdf:type, T),
                rdf_reachable(T, rdfs:subClassOf, owl:'Class'),
                \+ rdf_bnode(I),
                atom_concat(Prefix, _, I)
               ), IS),
    length(IS, N),
    format('  ~:d in ~w~n', [N, Prefix]).

report_instances_in(Pfx) :-
    findall(I, (rdf(I, rdf:type, E),
                rdf(E, rdf:type, T),
                rdf_reachable(T, rdfs:subClassOf, owl:'Class'),
                \+ rdf_bnode(I),
                atom_concat(Pfx, _, I)
               ), IS),
    length(IS, N),
    format('~`#t ~:d Instances in ~w ~`#t~78|~n', [N, Pfx]),
    sort(IS, SIS),
    findall(I, (member(I, SIS), report_instance(I)), _).

report_instance(I) :-
    rdf(I, rdf:type, T),
    prefix_shorten(I, SI),
    prefix_shorten(T, ST),
    format('~n~w :: ~w~n', [SI, ST]),
    findall((PR, PV), report_instance_propval(I, PR, PV), _PRVS),
    findall(MP, report_instance_missingprop(T, I, MP), _MPS),
    true.

report_instance_propval(I, PR, PV) :-
    rdf(I, PR, PV),
    report_instance_propval_(I, PR, PV).

report_instance_propval_(_I, PR, _PV) :-
    rdf_equal(PR, rdf:type), !.  % do nothing else, already reported
report_instance_propval_(_I, PR, PV) :-
    rdf_is_literal(PV), !,   % n.b. do not use rdf_literal: admits all atoms?!
    prefix_shorten(PR, SPR),
    rdf_literal_val_type(PV, PVv, PVt),
    prefix_shorten(PVt, SPVt),
    format('   . ~w = ~w :: ~w~n', [ SPR, PVv, SPVt ]).
report_instance_propval_(_I, PR, PV) :-
    prefix_shorten(PV, SPV),
    rdf(PV, rdf:type, PVT), !,
    prefix_shorten(PVT, SPVT),
    prefix_shorten(PR, SPR),
    format('   . ~w = ~w  :: ~w~n', [ SPR, SPV, SPVT ]).
report_instance_propval_(_I, PR, PV) :-
    prefix_shorten(PV, SPV),
    % case where rdf(PV, rdf:type, PVT) fails, usually a value not in the enum set
    \+ rdf(PV, rdf:type, _),
    rdf(PR, rdfs:range, PTy),
    rdf(PTy, owl:equivalentClass, PTyEquiv),
    rdf(PTyEquiv, owl:oneOf, Enums), !,
    rdf_list(Enums,L),
    prefix_shorten(PR, SPR),
    maplist(prefix_shorten, L, SL),
    format('   . ~w = ', [SPR]),
    ansi_format([bold,fg(red)], '~w  !! not in: ~w~n', [SPV, SL]).
report_instance_propval_(_I, PR, PV) :-
    prefix_shorten(PR, SPR),
    ansi_format([bold,fg(yellow)], '  -. ~w = ~w~n', [SPR, PV]).


report_instance_missingprop(T, I, Property) :-
    property_target(T, Property, _Usage, Target, _Restr),
    \+ rdf(I, Property, _Val),
    prefix_shorten(Target, ST),
    format('  ?. ~w = ? :: ~w~n', [Property, ST]).

report_instances_in_summary(Pfx) :-
    format('~n~n~`#t ~w Instances Summary ~`#t~78|~n', [Pfx]),
    findall(E, instances_in_summary(Pfx, E), _).

instances_in_summary(Pfx, T) :-
    findall(Leaf, ontology_leaf_class(Leaf), Leaves),
    sort(Leaves, SL),
    member(T, SL),
    findall(I, (rdf(I, rdf:type, T),
                \+ rdf_bnode(I),
                atom_concat(Pfx, _, I)
               ), IS),
    length(IS, ISLen),
    ISLen > 0,
    format('  ~:d~6|~t ~w instances~n', [ISLen, T]).
