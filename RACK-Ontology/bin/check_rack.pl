:- use_module(library(semweb/rdf11)).
:- use_module(rack_model).

check_rack :-
    findall(C, check_missing_notes(C), CS),
    findall(NPC, check_not_prov_s(NPC), NPCS),
    length(CS, CSLen),
    length(NPCS, NPCSLen),
    warn_if_nonzero("missing a Note/Description", CSLen),
    warn_if_nonzero("not a subclass of PROV-S#THING", NPCSLen),
    true.

check_missing_notes(Class) :-
    rdf(Class, rdf:type, owl:'Class'),
    \+ rdf(Class, rdfs:comment, _),
    rack_ontology_node(Class, _, _),
    print_message(warning, class_missing_note(Class)).

check_not_prov_s(Class) :-
    rdf(Class, rdf:type, owl:'Class'),
    rack_ontology_node(Class, _, _),
    rack_ref('PROV-S#THING', Thing),
    \+ rdf_reachable(Class, rdfs:subClassOf, Thing),
    print_message(warning, not_prov_s_thing_class(Class)).

warn_if_nonzero(What, Count) :-
    Count > 0, !,
    print_message(warning, num_classes(What, Count)).
warn_if_nonzero(_, _).

prolog:message(class_missing_note(Class)) -->
    [ 'No Note/Description for class ~w'-[Class] ].
prolog:message(not_prov_s_thing_class(Class)) -->
    [ 'Not a subclass of PROV-S#THING: ~w'-[Class] ].
prolog:message(num_classes(What, Count)) -->
    [ 'There are ~:d RACK definitions ~w.'-[Count, What] ].
