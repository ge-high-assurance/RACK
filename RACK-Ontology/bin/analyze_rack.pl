:- use_module(library(semweb/rdf11)).
:- use_module(rack_model).

report :-
    rdf_equal(rack:'PROV-S#THING', Thing),
    rdf(Thing, rdf:type, _), !, % verify the ontology is loaded
    rack_nodes(Nodes),
    report_rack_nodes(Nodes),
    true.

report :-
    rdf_equal(rack:'PROV-S#THING', Thing),
    \+ rdf(Thing, rdf:type, _),
    print_message(error, no_ontology_loaded).

prolog:message(no_ontology_loaded) -->
    [ 'No ontology has been loaded for analysis.'-[] ].

rack_nodes(Nodes) :-
    setof(E, T^Area^Item^(rdf(E, rdf:type, T),
                          rdf_reachable(T, rdfs:subClassOf, owl:'Class'),
                          rack_ontology_node(E, Area, Item)),
          Nodes).

report_rack_nodes([]).
report_rack_nodes([E|ES]) :-
    report_rack_node(E),
    report_rack_nodes(ES).

report_rack_node(E) :-
    rdf(E, rdf:type, T),
    class_comment(E, C),
    prefix_shorten(E, SE),
    prefix_shorten(T, ST),
    subclass_path(E, PPath),
    format('~n~w~w  :: ~w~n  ~w~n', [PPath, SE, ST, C]),
    findall(P, report_rack_properties(E, P), _PS),
    findall(S, report_rack_subclasses(E, S), _),
    report_instances(E).

class_comment(Class, Comment) :- rdf(Class, rdfs:comment, Comment), !.
class_comment(_, "?").

report_rack_properties(E, P) :-
    property_target(E, P, PType, T),
    show_property(E, P, PType, T).

show_property(_E, P, unique, T) :-
    rack_ref(ST, T), !,
    prefix_shorten(P, SP),
    format('   . ~w ~`-t-> ~w~79|~n', [SP, ST]).
show_property(_E, P, shared, T) :-
    rack_ref(ST, T), !,
    prefix_shorten(P, SP),
    format('  *. ~w ~`-t-> ~w~79|~n', [SP, ST]).
show_property(_E, P, shared, T) :-
    prefix_shorten(P, SP),
    prefix_shorten(T, ST),
    format('  *. ~w :: ~w~n', [SP, ST]).
show_property(_E, P, unique, T) :-
    prefix_shorten(P, SP),
    prefix_shorten(T, ST),
    format('   . ~w :: ~w~n', [SP, ST]).

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

report_rack_subclasses(E, S) :-
    rdf(S, rdfs:subClassOf, E),
    rdf(S, rdf:type, T),
    prefix_shorten(T, ST),
    format('  ^-- ~w  ~`.t~52| :: ~w~n', [S, ST]).

report_instances(E) :-
    findall(I, rdf(I, rdf:type, E), IS),
    report_instances(E, IS).

report_instances(E, IS) :-
    length(IS, ISLen),
    (ISLen == 0, !, true;
     setof(NS, L^X^(member(X, IS), rdf_split_url(NS, L, X)), NSL),
     findall(RNS, report_instances(E, IS, NSL, RNS), _RNSL)).

report_instances(E, _IS, NSL, CNS) :-
    member(CNS, NSL),
    findall(NSX, (rdf(NSX, rdf:type, E), rdf_split_url(CNS, _, NSX)), NSXS),
    length(NSXS, N),
    format('  () of ~d instances in ~w~n', [N, CNS]).


prefix_shorten(URI, ShortOrURI) :-
    rdf_current_prefix(Prefix, Exp), atom_concat(Exp, Local, URI), !,
    atom_concat(Prefix, ':', A),
    atom_concat(A, Local, ShortOrURI).
prefix_shorten(URI, URI).
