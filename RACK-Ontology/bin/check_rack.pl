:- use_module(library(semweb/rdf11)).
:- use_module(rack_model).

check_rack :-
    findall(C, check_missing_notes(C), CS),
    length(CS, CSLen),
    ((CSLen > 0, !,
      print_message(warning, num_classes_missing_note(CSLen))) ; true).

check_missing_notes(Class) :-
    rdf(Class, rdf:type, owl:'Class'),
    \+ rdf(Class, rdfs:comment, _),
    rack_ontology_node(Class, _, _),
    print_message(warning, class_missing_note(Class)).

prolog:message(class_missing_note(Class)) -->
    [ 'No Note/Description for class ~w'-[Class] ].
prolog:message(num_classes_missing_note(CSLen)) -->
    [ 'There are ~d RACK definitions missing a Note/Description.'-[CSLen] ].
