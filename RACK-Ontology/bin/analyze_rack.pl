:- use_module(library(semweb/rdf11)).
:- use_module(rack_model).

report :-
    rdf_equal(rack:'PROV-S#THING', Thing),
    rdf(Thing, rdf:type, _), !, % verify the ontology is loaded
    rack_nodes(Nodes),
    report_rack_nodes(Nodes),
    format('~n~n'),
    instance_prefixes(Prefixes),
    report_instance_prefixes(Prefixes),
    true.

report :-
    rdf_equal(rack:'PROV-S#THING', Thing),
    \+ rdf(Thing, rdf:type, _),
    print_message(error, no_ontology_loaded).

report(InstancePfx) :-
    rdf_equal(rack:'PROV-S#THING', Thing),
    rdf(Thing, rdf:type, _), !, % verify the ontology is loaded
    rack_nodes(Nodes),
    report_rack_nodes(Nodes),
    format('~n~n'),
    instance_prefixes(Prefixes),
    report_instance_prefixes(Prefixes),
    format('~n~n'),
    report_instances_in(InstancePfx),
    true.

report(_) :-
    rdf_equal(rack:'PROV-S#THING', Thing),
    \+ rdf(Thing, rdf:type, _),
    print_message(error, no_ontology_loaded).

prolog:message(no_ontology_loaded) -->
    [ 'No ontology has been loaded for analysis.'-[] ].

% ----------------------------------------------------------------------

rack_nodes(Nodes) :-
    setof(E, T^Area^Item^(rdf(E, rdf:type, T),
                          rdf_reachable(T, rdfs:subClassOf, owl:'Class'),
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
    rdf(E, rdf:type, T),
    class_comment(E, C),
    prefix_shorten(E, SE),
    prefix_shorten(T, ST),
    subclass_path(E, PPath),
    format('~n~w~w  :: ~w~n  ~w~n', [PPath, SE, ST, C]),
    findall(P, report_rack_properties(E, P), _PS),
    findall(S, report_rack_subclasses(E, S), _),
    report_instance_counts(E).

class_comment(Class, Comment) :- rdf(Class, rdfs:comment, Comment), !.
class_comment(_, "?").

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

report_rack_subclasses(E, S) :-
    rdf(S, rdfs:subClassOf, E),
    rdf(S, rdf:type, T),
    prefix_shorten(T, ST),
    format('  ^-- ~w  ~`.t~52| :: ~w~n', [S, ST]).

report_instance_counts(E) :-
    findall(I, rdf(I, rdf:type, E), IS),
    report_instance_counts(E, IS).

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
     findall(I, report_rack_instance(RACK_IS, I), _IS)).

report_rack_instance(RACK_IS, I) :-
    member(I, RACK_IS),
    format('    = ~w~n', [I]).

instance_prefixes(Prefixes) :-
    setof(P, E^T^I^L^(rdf(E, rdf:type, T),
                      rdf_reachable(T, rdfs:subClassOf, owl:'Class'),
                      rdf(I, rdf:type, E),
                      \+ rack_ontology_node(I, _, _),
                      \+ rdf_bnode(I),
                      rdf_split_url(P, L, I)), Prefixes).

report_instance_prefixes(Prefixes) :-
    length(Prefixes, N),
    format('~`#t Instance Prefixes (~:d) ~`#t~78|~n', [N]),
    findall(P, (member(P, Prefixes), report_instance_prefix(P)), _).

report_instance_prefix(Prefix) :-
    findall(I, (rdf(I, rdf:type, E),
                rdf(E, rdf:type, T),
                rdf_reachable(T, rdfs:subClassOf, owl:'Class'),
                \+ rdf_bnode(I),
                rdf_split_url(Prefix, _L, I)
               ), IS),
    length(IS, N),
    format('  ~:d in ~w~n', [N, Prefix]).

report_instances_in(Pfx) :- write('rii '), write(Pfx), nl,
    findall(I, (rdf(I, rdf:type, E),
                rdf(E, rdf:type, T),
                rdf_reachable(T, rdfs:subClassOf, owl:'Class'),
                \+ rdf_bnode(I),
                atom_concat(Pfx, _, I)
               ), IS),
    length(IS, N),
    format('~`#t ~:d Instances in ~w ~`#t~78|~n', [N, Pfx]),
    findall(I, (member(I, IS), report_instance(I)), _).

report_instance(I) :-
    rdf(I, rdf:type, T),
    prefix_shorten(I, SI),
    prefix_shorten(T, ST),
    format('~n~w :: ~w~n', [SI, ST]),
    findall((PR, PV), report_instance_propval(I, PR, PV), _PRVS),
    true.

report_instance_propval(I, PR, PV) :-
    rdf(I, PR, PV),
    report_instance_propval_(I, PR, PV).

report_instance_propval_(_I, PR, _PV) :-
    rdf_equal(PR, rdf:type), !.  % do nothing else, already reported
report_instance_propval_(_I, PR, PV) :-
    rdf_literal(PV), !,
    prefix_shorten(PR, SPR),
    rdf_literal_val_type(PV, PVv, PVt),
    prefix_shorten(PVt, SPVt),
    format('   . ~w = ~w :: ~w~n', [ SPR, PVv, SPVt ]).
report_instance_propval_(_I, PR, PV, SPVT) :-
    \+ rdf_literal(PV),
    prefix_shorten(PV, SPV),
    rdf(PV, rdf:type, PVT),
    prefix_shorten(PVT, SPVT),
    prefix_shorten(PR, SPR),
    format('   . ~w ~w :.: ~w~n', [ SPR, SPV, SPVT ]).

% ----------------------------------------------------------------------

prefix_shorten(URI, ShortOrURI) :-
    rdf_current_prefix(Prefix, Exp), atom_concat(Exp, Local, URI), !,
    atom_concat(Prefix, ':', A),
    atom_concat(A, Local, ShortOrURI).
prefix_shorten(URI, URI).
