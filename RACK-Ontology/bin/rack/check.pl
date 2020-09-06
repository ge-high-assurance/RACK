:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

check_rack :-
    findall(C, check_missing_notes(C), CS),
    findall(NPC, check_not_prov_s(NPC), NPCS),
    findall(MI, check_instance_property_violations(MI), MIS),
    length(CS, CSLen),
    length(NPCS, NPCSLen),
    length(MIS, MISLen),
    format('~`.t Summary ~`.t~78|~n'),
    warn_if_nonzero("missing a Note/Description", CSLen),
    warn_if_nonzero("not a subclass of PROV-S#THING", NPCSLen),
    warn_if_nonzero("with instance property issues", MISLen),
    (CSLen == 0, NPCSLen == 0, MISLen == 0, !,
     format('No issues found~n') ; format('ISSUES FOUND IN CHECK~n')),
    true.

check_missing_notes(Class) :-
    rdf(Class, rdf:type, owl:'Class'),
    rack_ontology_node(Class, _, _),
    \+ rdf(Class, rdfs:comment, _),
    print_message(warning, class_missing_note(Class)).

check_not_prov_s(Class) :-
    rdf(Class, rdf:type, owl:'Class'),
    rack_ontology_node(Class, _, _),
    rack_ref('PROV-S#THING', Thing),
    \+ rdf_reachable(Class, rdfs:subClassOf, Thing),
    print_message(warning, not_prov_s_thing_class(Class)).

check_instance_property_violations(Property) :-
    % Get an instance
    rdf(I, rdf:type, T),
    rdf(T, rdf:type, C),
    rdf_reachable(C, rdfs:subClassOf, owl:'Class'),
    % Exclude namespaces we aren't interested in
    has_interesting_prefix(I),
    % Find a required property defined on that instance type (or parent type)
    (check_cardinality(Property, I, T);
     check_maybe_prop(Property, I, T)).

check_cardinality(Property, I, T) :-
    property_target(T, Property, _PUsage, _Target, cardinality(N)),
    has_interesting_prefix(Property),
    % How many actual values for that property on this instance
    findall(V, rdf(I, Property, V), VS),
    length(VS, VSLen),
    % Error if no match
    VSLen \= N,
    prefix_shorten(I, SI),
    prefix_shorten(Property, SP),
    print_message(error, cardinality_violation(SI, SP, N, VSLen)).

check_maybe_prop(Property, I, T) :-
    property_target(T, Property, _PUsage, _Target, maybe),
    has_interesting_prefix(Property),
    % How many actual values for that property on this instance
    findall(V, rdf(I, Property, V), VS),
    (length(VS, 0), !, fail; % fail to not report during check
     length(VS, 1), !, fail; % fail to not report during check
     length(VS, VSLen),
     prefix_shorten(I, SI),
     prefix_shorten(Property, SP),
     print_message(error, maybe_restriction(SI, SP, VSLen))).


has_interesting_prefix(I) :-
    member(Pfx, [ 'http://sadl.org/' ]),
    atom_concat(Pfx, _Local, I), !, fail.
has_interesting_prefix(_).

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
prolog:message(cardinality_violation(Instance, Property, Specified, Actual)) -->
    [ '~w . ~w has ~d values but cardinality of ~d~n'-[
          Instance, Property, Actual, Specified] ].
prolog:message(maybe_restriction(Instance, Property, Actual)) -->
    [ '~w . ~w must have only zero or one instance, but has ~d~n'-[
          Instance, Property, Actual] ].
