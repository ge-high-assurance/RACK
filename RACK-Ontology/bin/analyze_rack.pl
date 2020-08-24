:- use_module(library(semweb/rdf11)).
:- use_module(rack_model).

report :-
    rdf_equal(rack:'PROV-S#THING', Thing),
    rdf(Thing, rdf:type, _), !, % verify the ontology is loaded
    report_rack_ontology(Thing, [], NonThings),
    findall(NT, (member(NT, NonThings),
                 report_rack_ontology(NT, [], _)), _).

report :-
    rdf_equal(rack:'PROV-S#THING', Thing),
    \+ rdf(Thing, rdf:type, _),
    print_message(error, no_ontology_loaded).

prolog:message(no_ontology_loaded) -->
    [ 'No ontology has been loaded for analysis.'-[] ].

report_rack_ontology(E, Parents, SeenNonThings) :-
    (rdf(E, rdfs:comment, C), ! ; C = "?"),
    rdf(E, rdf:type, T),
    prefix_shorten(E, SE),
    prefix_shorten(T, ST),
    show_parent_path(Parents, PPath),
    format('~n~w~w  :: ~w~n  ~w~n', [PPath, SE, ST, C]),
    findall(M, report_rack_properties(E, _P, M), MS),
    findall(S, report_rack_subclasses(E, S), _),
    report_instances(E),
    findall(SM, (rdf(S, rdfs:subClassOf, E),
                 report_rack_ontology(S, [SE|Parents], SM)), SMS),
    flatten(MS, MSF),
    flatten(SMS, SMSF),
    append(MSF, SMSF, MSFL),
    list_to_set(MSFL, SeenNonThings).

report_rack_properties(E, P, Extra) :-
    property_target(E, P, PType, T),
    add_non_thing_target(T, Extra),
    show_property(E, P, PType, T).

add_non_thing_target(T, []) :-
    rack_ref('PROV-S#THING', Thing),
    rdf_reachable(T, rdfs:subClassOf, Thing), !.
add_non_thing_target(T, [T]).


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

show_parent_path([], "") :- !.
show_parent_path(P, ParentPath) :-
    reverse(P, R),
    atomics_to_string(R, '.', Path),
    string_concat(Path, ".", ParentPath).

report_rack_subclasses(E, S) :-
    rdf(S, rdfs:subClassOf, E),
    rdf(S, rdf:type, T),
    prefix_shorten(T, ST),
    format('  ^-- ~w  ~`.t~52| :: ~w~n', [S, ST]).

report_instances(E) :-
    setof(NS, X^L^(rdf(X, rdf:type, E), rdf_split_url(NS, L, X)), NSL),
    length(NSL, NSLC),
    NSLC > 0, !,
    member(CNS, NSL),
    findall(NSX, (rdf(NSX, rdf:type, E), rdf_split_url(CNS, _, NSX)), NSXS),
    length(NSXS, N),
    format('  () of ~d instances in ~w~n', [N, CNS]).
report_instances(_E).


prefix_shorten(URI, ShortOrURI) :-
    rdf_current_prefix(Prefix, Exp), atom_concat(Exp, Local, URI), !,
    atom_concat(Prefix, ':', A),
    atom_concat(A, Local, ShortOrURI).
prefix_shorten(URI, URI).
