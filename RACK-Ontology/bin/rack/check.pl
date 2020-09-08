:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

check_rack :-
    findall(C, check_missing_notes(C), CS),
    findall(NPC, check_not_prov_s(NPC), NPCS),
    findall(BI, check_instance_types(BI), BIS),
    findall(MI, check_instance_property_violations(MI), MIS),
    length(CS, CSLen),
    length(NPCS, NPCSLen),
    length(BIS, BISLen),
    length(MIS, MISLen),
    format('~`.t Summary ~`.t~78|~n'),
    warn_if_nonzero("missing a Note/Description", CSLen),
    warn_if_nonzero("not a subclass of PROV-S#THING", NPCSLen),
    warn_if_nonzero("with instance issues", BISLen),
    warn_if_nonzero("with instance property issues", MISLen),
    (CSLen == 0, NPCSLen == 0, BISLen == 0, MISLen == 0, !,
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

check_instance_types(I) :-
    % Get an instance
    rdf(I, rdf:type, T),
    rdf(T, rdf:type, C),
    rdf_reachable(C, rdfs:subClassOf, owl:'Class'),
    % Exclude namespaces we aren't interested in
    has_interesting_prefix(I),
    findall(IT, rdf(I, rdf:type, IT), ITs),
    length(ITs, N),
    N > 1,
    ITs = [T|_],
    prefix_shorten(I, SI),
    maplist(prefix_shorten, ITs, SITs),
    print_message(error, multiple_types_for_instance(SI, SITs)).

check_instance_property_violations(Property) :-
    % Get an instance
    rdf(I, rdf:type, T),
    rdf(T, rdf:type, C),
    rdf_reachable(C, rdfs:subClassOf, owl:'Class'),
    % Exclude namespaces we aren't interested in
    has_interesting_prefix(I),
    % Find a required property defined on that instance type (or parent type)
    (check_cardinality(Property, I, T);
     check_maybe_prop(Property, I, T);
     check_invalid_value(Property, I, T)).

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

check_invalid_value(Property, I, T) :-
    property_target(T, Property, _PUsage, _Target, _Restr),
    has_interesting_prefix(Property),
    rdf(I, Property, V),
    \+ rdf_is_literal(V),  % literals checked elsewhere
    \+ rdf(V, rdf:type, _),
    rdf(Property, rdfs:range, PTy),
    rdf(PTy, owl:equivalentClass, PTyEquiv),
    rdf(PTyEquiv, owl:oneOf, Enums), !,
    rdf_list(Enums,L),
    prefix_shorten(I, SI),
    prefix_shorten(Property, SP),
    prefix_shorten(V, SV),
    maplist(prefix_shorten, L, SL),
    print_message(error, invalid_value_in_enum(SI, SP, SV, SL)).

check_invalid_value(Property, I, T) :-
    property_target(T, Property, _PUsage, _Target, _Restr),
    has_interesting_prefix(Property),
    rdf(I, Property, V),
    rdf_is_literal(V),  % literals checked elsewhere
    rdf(Property, rdfs:range, R),
    rdf(R, owl:equivalentClass, REquiv),
    rdf(REquiv, owl:withRestrictions, RL),
    rdf(REquiv, owl:onDatatype, RT),
    rdf_list(RL, L),
    member(E,L),
    rdf(E, xsd:minInclusive, MinV),
    rdf(E, xsd:maxInclusive, MaxV),
    actual_val(MinV, RT, MinVal),
    actual_val(MaxV, RT, MaxVal),
    actual_val(V, RT, Val),
    (rdf_compare(<,Val,MinVal) ; rdf_compare(>,Val,MaxVal)),
    prefix_shorten(I, SI),
    prefix_shorten(Property, SP),
    print_message(error, value_outside_range(SI, SP, RT, Val, MinVal, MaxVal)).

actual_val((V^^VT),VT,(V^^VT)).  % normal
actual_val(V,VT,Val) :-
    rdf_equal(V, VS^^(xsd:string)),
    % SADL doesn't specify the type for the maxInclusive/minInclusive
    % values, so they are interpreted as strings.  Attempt to convert
    % them to th1e target type here.
    \+ rdf_equal(xsd:string, VT),
    atom_string(VA,VS),
    % Note: converted based on representation, not VT because there is
    % no way to direct the conversion.
    atom_number(VA,VN),
    Val = VN^^VT.

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
prolog:message(invalid_value_in_enum(Instance, Property, Value, Valid)) -->
    [ '~w . ~w value of ~w is invalid, allowed enumerations: ~w~n'-[
          Instance, Property, Value, Valid] ].
prolog:message(value_outside_range(Instance, Property, Ty, V, MinV, MaxV)) -->
    { (rdf_equal(xsd:T, Ty) ; T = Ty),
      (rdf_equal(Val^^Ty, V) ; Val = V),
      (rdf_equal(Min^^Ty, MinV) ; Min = MinV),
      (rdf_equal(Max^^Ty, MaxV) ; Max = MaxV)
    },
    [ '~w . ~w value of ~w is outside ~w range [~w .. ~w]~n'-[
          Instance, Property, Val, T, Min, Max ] ].
prolog:message(multiple_types_for_instance(Instance, Types)) -->
    [ 'Instance ~w has multiple types: ~w~n'-[Instance, Types] ].
