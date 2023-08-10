% Copyright (c) 2020-2022, Galois, Inc.
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

core_ontology_size(Size) :-
    findall(Class,
            (rdf(Class, rdf:type, owl:'Class'),
             rack_ontology_node(Class, _, _)),
            Classes),
    length(Classes, Size).

overlay_ontology_size(Size) :-
    findall(Class,
            (rdf(Class, rdf:type, owl:'Class'),
             \+ rack_ontology_node(Class, _, _),
             \+ rdf_is_bnode(Class)),
            Classes),
    length(Classes, Size).

num_instances(Count) :-
    findall(I, rack_data_instance(I), IS),
    length(IS, Count).

check_missing_notes(Class) :-
    rdf(Class, rdf:type, owl:'Class'),
    has_interesting_prefix(Class),
    \+ rdf(Class, rdfs:comment, _),
    \+ rdf_is_bnode(Class),
    print_message(informational, unary_op(check_missing_notes, Class)),
    print_message(warning, class_missing_note(Class)).

check_not_prov_s(Class) :-
    rdf(Class, rdf:type, owl:'Class'),
    has_interesting_prefix(Class),
    rack_ref('PROV-S#NODE', NodeBase),
    \+ rdf_reachable(Class, rdfs:subClassOf, NodeBase),
    \+ rdf_is_bnode(Class),
    print_message(informational, unary_op(check_not_prov_s, Class)),
    print_message(warning, not_prov_s_node_class(Class)).

check_instance_types(I) :-
    % Get an instance
    rack_data_instance(I),
    % Exclude namespaces we aren't interested in
    has_interesting_prefix(I),
    findall(IT, rdf(I, rdf:type, IT), ITs),
    length(ITs, N),
    N > 1,
    print_message(informational, unary_op(check_instance_types, I)),
    print_message(error, multiple_types_for_instance(I, ITs)).

check_instance_property_violations(Property) :-
    % Get an instance
    rack_data_instance(I),
    % Exclude namespaces we aren't interested in
    has_interesting_prefix(I),
    % Find a required property defined on that instance type (or parent type)
    rdf(I, rdf:type, T),
    ( check_cardinality_exact(Property, I, T)
    ; check_cardinality_min(Property, I, T)
    ; check_cardinality_max(Property, I, T)
    ; check_maybe_prop(Property, I, T)
    ; check_target_type(Property, I, T)
    ; check_target_type_restrictions(Property, I, T)
    ; check_values_from(Property, I, T)
    ; check_invalid_value(Property, I, T)
    ).

check_cardinality_exact(Property, I, T) :-
    property_extra(T, Property, cardinality(N)),
    has_interesting_prefix(Property),
    % How many actual values for that property on this instance
    findall(V, rdf(I, Property, V), VS),
    length(VS, VSLen),
    VSLen \= N,
    rack_instance_ident(I, IName),
    print_message(informational, trinary_op(check_cardinality_exact, Property, I, T)),
    print_message(error, cardinality_violation(T, I, IName, Property, N, VSLen)).

check_cardinality_min(Property, I, T) :-
    property_extra(T, Property, min_cardinality(N)),
    has_interesting_prefix(Property),
    % How many actual values for that property on this instance
    findall(V, rdf(I, Property, V), VS),
    length(VS, VSLen),
    VSLen < N,
    rack_instance_ident(I, IName),
    print_message(informational, trinary_op(check_cardinality_min, Property, I, T)),
    print_message(error, min_cardinality_violation(T, I, IName, Property, N, VSLen)).

check_cardinality_max(Property, I, T) :-
    property_extra(T, Property, max_cardinality(N)),
    has_interesting_prefix(Property),
    % How many actual values for that property on this instance
    findall(V, rdf(I, Property, V), VS),
    length(VS, VSLen),
    VSLen > N,
    rack_instance_ident(I, IName),
    print_message(informational, trinary_op(check_cardinality_max, Property, I, T)),
    print_message(error, max_cardinality_violation(T, I, IName, Property, N, VSLen)).

check_maybe_prop(Property, I, T) :-
    property_extra(T, Property, maybe),
    has_interesting_prefix(Property),
    % How many actual values for that property on this instance
    findall(V, rdf(I, Property, V), VS),
    (length(VS, 0), !, fail; % fail: do not report during check
     length(VS, 1), !, fail; % fail: do not report during check
     length(VS, VSLen),
     rack_instance_ident(I, IName),
     print_message(informational, trinary_op(check_maybe_prop, Property, I, T)),
     print_message(error, maybe_restriction(T, I, IName, Property, VSLen))).

check_target_type(Property, I, T) :-
    property(T, Property, _),
    \+ rdf_is_bnode(T),
    has_interesting_prefix(Property),
    rdf(I, Property, Val),
    \+ rdf_is_literal(Val),
    \+ rack_instance_target(I, Property, Val),
    rack_instance_ident(I, IName),
    rdf(Val, rdf:type, ValTy),
    property_range_type(T, Property, ModelTy),
    print_message(informational, trinary_op(check_target_type, Property, I, T)),
    print_message(error, property_value_wrong_type(T, I, IName, Property, ValTy, Val, ModelTy)).

check_target_type_restrictions(Property, I, T) :-
    rdf(T, rdfs:subClassOf, R),
    rdf_is_bnode(R),
    rdf(R, rdf:type, owl:'Restriction'),
    rdf(R, owl:'onProperty', Property),
    has_interesting_prefix(Property),
    rdf(R, owl:'allValuesFrom', RTgtTy),
    rdf(I, Property, Val),
    rack_instance_ident(I, IName),
    \+ rdf_is_literal(Val),
    rdf(Val, rdf:type, ValTy),
    ( owl_list(RTgtTy, TyLst),
      \+ member(ValTy, TyLst),
      print_message(error, property_value_wrong_type(T, I, IName, Property, ValTy, Val, 'TyLst'))
    ; \+ owl_list(RTgtTy, _),
      ValTy \= RTgtTy,
        print_message(error, property_value_wrong_type(T, I, IName, Property, ValTy, Val, RTgtTy))
    ).

check_values_from(Property, I, T) :-
    property_extra(T, Property, value_from(Cls)),
    has_interesting_prefix(Property),
    rdf(I, Property, Val),
    \+ rdf_is_literal(Val),  % TODO check these as well?
    rdf(Val, rdf:type, DefTy),
    DefTy \= Cls,
    rack_instance_ident(I, IName),
    % Cls might be a direct type or a oneOf restriction to a list of types
    ( rdf_is_bnode(Cls), rdf(Cls, owl:oneOf, ClsLst), !,
      rdf_list(ClsLst, CList),
      ( member(CL, CList),
        rdf_reachable(DefTy, rdfs:subClassOf, CL), !  % matches, stop processing
      ; print_message(informational, trinary_op(check_values_from, Property, I, T)),
        print_message(error,
                      property_value_wrong_type_in(T, I, IName, Property, DefTy, Val, CList))
      )
    ; \+ rdf_reachable(DefTy, rdfs:subClassOf, Cls),
      print_message(informational, trinary_op(check_values_from, Property, I, T)),
      print_message(error, property_value_wrong_type(T, I, IName, Property, DefTy, Val, Cls))
    ).

check_invalid_value(Property, I, T) :-
    property_extra(T, Property, _Restr),
    rdf(Property, rdfs:range, PTy),
    rdf(PTy, owl:equivalentClass, PTyEquiv),
    rdf(PTyEquiv, owl:oneOf, Enums),
    rdf(I, Property, V),
    \+ rdf_is_literal(V),  % literals checked elsewhere
    \+ rdf(V, rdf:type, _),
    has_interesting_prefix(Property),
    !,
    rdf_list(Enums,L),
    rack_instance_ident(I, IName),
    print_message(informational, trinary_op(check_invalid_value, Property, I, T)),
    print_message(error, invalid_value_in_enum(T, I, IName, Property, V, L)).

check_invalid_value(Property, I, T) :-
    property_extra(T, Property, _Restr),
    has_interesting_prefix(Property),
    rdf(I, Property, V),
    rdf_is_literal(V),  % non-literals handled elsewhere
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
    rack_instance_ident(I, IName),
    print_message(informational, trinary_op(check_invalid_value, Property, I, T)),
    print_message(error, value_outside_range(T, I, IName, Property, RT, Val, MinVal, MaxVal)).

% True for any SrcClass that has no Prop relationship to any target
% instance.  Returns the SrcInst that this occurs for as well as
% generating a warning.
check_has_no_rel(Context, SrcClass, Prop, SrcInst) :-
    is_valid_property(Context, SrcClass, Prop),
    rack_data_instance(SrcClass, SrcInst),
    none_of(SrcInst, rack_instance_relationship(SrcClass, Prop)),
    rack_instance_ident(SrcInst, SrcName),
    print_message(informational, quarnary_op(check_has_no_rel, Context, SrcClass, Prop, SrcInst)),
    print_message(warning, missing_any_tgt(Context, SrcClass, SrcInst, SrcName, Prop)).

% True for any SrcClass that has no Prop relationship to an instance
% of the specific target class.  Returns the SrcInst that this occurs
% for as well as generating a warning.
check_has_no_rel(Context, SrcClass, Prop, TgtClass, SrcInst) :-
    is_valid_property(Context, SrcClass, Prop),
    rack_data_instance(SrcClass, SrcInst),
    none_of(SrcInst, rack_instance_relationship(SrcClass, Prop, TgtClass)),
    rack_instance_ident(SrcInst, SrcName),
    print_message(informational, quintary_op(check_has_no_rel, Context, SrcClass, Prop, TgtClass, SrcInst)),
    print_message(warning, missing_tgt(Context, SrcClass, SrcInst, SrcName, Prop, TgtClass)),
    % -- if the above fails, it's probably useful to see if there are
    % *any* targets of Src--[Rel]-->
    check_also_has_no_rel(SrcClass, Prop).

check_also_has_no_rel(SrcClass, SrcInst, Rel) :-
    check_has_no_rel(SrcClass, Rel, SrcInst), !.
check_also_has_no_rel(_, _).

is_valid_property(_, SrcClass, Property) :-
    rdf(Property, rdfs:domain, PropClass),
    rdf_reachable(SrcClass, rdfs:subClassOf, PropClass), !.
is_valid_property(Context, SrcClass, Property) :-
    print_message(informational, trinary_op(is_valid_property, '_', SrcClass, Property)),
    print_message(error, invalid_property_in_check(Context, SrcClass, Property)),
    fail.


% Sometimes there will be things in SADL like:
%
%   FOO is a type of X.
%     p of FOO only has values of type Y.
%
% and the problem is that p is not defined for X, but for (unrelated) Z instead.
% SADL will not complain and will generate a property constraint, but that
% property cannot ever exist.  This checks for that situation.
check_invalid_domain(Property) :-
    check_invalid_domain_class(_SrcClass, Property, _DefinedClass).

check_invalid_domain_class(SrcClass, Property, DefinedClass) :-
    property(SrcClass, Property, Usage),
    rdf_reachable(Property, rdfs:subPropertyOf, ParentProp),
    property(DefinedClass, ParentProp, _ParentUsage),
    \+ rdf_reachable(SrcClass, rdfs:subClassOf, DefinedClass),
    findall(OtherParent, (property(OtherParent, Property, Usage), OtherParent \= SrcClass), OtherParents),
    \+ member(DefinedClass, OtherParents),
    ( Property = ParentProp,
      print_message(informational, trinary_op(check_invalid_domain_class, SrcClass, Property, DefinedClass)),
      print_message(error, invalid_domain(SrcClass, Property, DefinedClass))
    ; Property \= ParentProp,
      print_message(informational, trinary_op(check_invalid_domain_class, SrcClass, Property, DefinedClass)),
      print_message(error, invalid_subclass_domain(SrcClass, Property, ParentProp, DefinedClass))
    ).


actual_val((V^^VT),VT,(V^^VT)).  % normal
actual_val(V,VT,Val) :-
    rdf_equal(V, VS^^(xsd:string)),
    % OWL1 and older SADL doesn't specify the type for the
    % maxInclusive/minInclusive values, so they are interpreted as
    % strings.  Attempt to convert them to the target type here.  In
    % general, the OWL specification specifies using a "lexical value"
    % which is then converted at use sites to the "actual value" of
    % the desired type.
    \+ rdf_equal(xsd:string, VT),
    atom_string(VA,VS),
    % Note: converted based on representation, not VT because there is
    % no way to direct the conversion.
    atom_number(VA,VN),
    Val = VN^^VT.

has_interesting_prefix(I) :-
    member(Pfx, [ 'http://sadl.org/',
                  'http://com.ge.research/',
                  'http://demo/',
                  'http://research.ge.com/'
                ]),
    atom_concat(Pfx, _Local, I), !, fail.
has_interesting_prefix(_).

warn_if_nonzero(What, Count) :-
    Count > 0, !,
    print_message(warning, num_classes(What, Count)).
warn_if_nonzero(_, _).

warn_all_nonzero([]).
warn_all_nonzero([(C,N)|CNS]) :- warn_if_nonzero(C,N), warn_all_nonzero(CNS).

sum_all_nonzero([], 0).
sum_all_nonzero([(_,N)|CNS], Sum) :-
    sum_all_nonzero(CNS, SubSum),
    Sum is N + SubSum.


prolog:message(unary_op(Name, Arg1)) -->
    [ 'vv ~w(~w)'-[Name, Arg1] ].
prolog:message(trinary_op(Name, Arg1, Arg2, Arg3)) -->
    [ 'vv ~w(~w, ~w, ~w)'-[Name, Arg1, Arg2, Arg3] ].
prolog:message(quarnary_op(Name, Arg1, Arg2, Arg3, Arg4)) -->
    [ 'vv ~w(~w, ~w, ~w, ~w)'-[Name, Arg1, Arg2, Arg3, Arg4] ].
prolog:message(quintary_op(Name, Arg1, Arg2, Arg3, Arg4, Arg5)) -->
    [ 'vv ~w(~w, ~w, ~w, ~w, ~w)'-[Name, Arg1, Arg2, Arg3, Arg4, Arg5] ].
prolog:message(class_missing_note(Class)) -->
    [ 'CE-100: No Note/Description for class ~w'-[Class] ].
prolog:message(not_prov_s_node_class(Class)) -->
    [ 'CE-101: Not a subclass of PROV-S#NODE: ~w'-[Class] ].
prolog:message(num_classes(What, Count)) -->
    [ 'There are ~:d RACK ~w.'-[Count, What] ].
prolog:message(cardinality_violation(InstType, Instance, InstanceIdent, Property, Specified, Actual)) -->
    { prefix_shorten(Instance, SI),
      prefix_shorten(InstType, ST),
      prefix_shorten(Property, SP)
    },
    [ 'CE-103: ~w ~w . ~w has ~d values but an allowed cardinality of ~d~n                Domain: ~w~n'-[
          ST, InstanceIdent, SP, Actual, Specified, SI ] ].
prolog:message(min_cardinality_violation(InstType, Instance, IName, Property, Specified, Actual)) -->
    { prefix_shorten(Instance, SI),
      prefix_shorten(InstType, ST),
      prefix_shorten(Property, SP),
      rdf_literal_val_type(IName, InstName, _)
    },
    [ 'CE-104: ~w ~w . ~w has ~d values but a minimum allowed cardinality of ~d~n                Domain: ~w~n'-[
          ST, InstName, SP, Actual, Specified, SI] ].
prolog:message(max_cardinality_violation(InstType, Instance, IName, Property, Specified, Actual)) -->
    { prefix_shorten(Instance, SI),
      prefix_shorten(InstType, ST),
      prefix_shorten(Property, SP),
      rdf_literal_val_type(IName, InstName, _)
    },
    [ 'CE-105: ~w ~w . ~w has ~d values but a maximum allowed cardinality of ~d~n                Domain: ~w~n'-[
          ST, InstName, SP, Actual, Specified, SI ] ].
prolog:message(maybe_restriction(InstType, Instance, IName, Property, Actual)) -->
    { prefix_shorten(Instance, SI),
      prefix_shorten(InstType, ST),
      prefix_shorten(Property, SP),
      rdf_literal_val_type(IName, InstName, _)
    },
    [ 'CE-106: ~w ~w . ~w must have only zero or one instance, but has ~d~n                Domain: ~w~n'-[
          ST, InstName, SP, Actual, SI ] ].
prolog:message(invalid_value_in_enum(InstType, Instance, IName, Property, Value, Valid)) -->
    { prefix_shorten(Instance, SI),
      prefix_shorten(InstType, ST),
      prefix_shorten(Property, SP),
      prefix_shorten(Value, SV),
      maplist(prefix_shorten, Valid, SL),
      rdf_literal_val_type(IName, InstName, _)
    },
    [ 'CE-107: ~w ~w . ~w value of ~w is invalid, allowed enumerations: ~w~n                Domain: ~w~n'-[
          ST, InstName, SP, SV, SL, SI ] ].
prolog:message(value_outside_range(InstType, Instance, IName, Property, Ty, V, MinV, MaxV)) -->
    { prefix_shorten(Instance, SI),
      prefix_shorten(InstType, ST),
      prefix_shorten(Property, SP),
      (rdf_equal(xsd:T, Ty) ; T = Ty),
      (rdf_equal(Val^^Ty, V) ; Val = V),
      (rdf_equal(Min^^Ty, MinV) ; Min = MinV),
      (rdf_equal(Max^^Ty, MaxV) ; Max = MaxV),
      rdf_literal_val_type(IName, InstName, _)
    },
    [ 'CE-108: ~w, ~w . ~w value of ~w is outside ~w range [~w .. ~w]~n                Domain: ~w~n'-[
          ST, InstName, SP, Val, T, Min, Max, SI ] ].
prolog:message(multiple_types_for_instance(Instance, Types)) -->
    { prefix_shorten(Instance, SI),
      maplist(prefix_shorten, Types, STys)
    },
    [ 'CE-109: Instance ~w has multiple types: ~w~n'-[SI, STys] ].
prolog:message(property_value_wrong_type(InstType, Instance, IName, Property, DefType, Val, ValType)) -->
    { prefix_shorten(Instance, SI),
      prefix_shorten(InstType, ST),
      prefix_shorten(Property, SP),
      prefix_shorten(DefType, SDTy),
      prefix_shorten(ValType, SVTy),
      prefix_shorten(Val, SV),
      rdf_literal_val_type(IName, InstName, _)
    },
    [ 'CE-110: ~w instance property ~w . ~w of ~w should be a ~w but is a ~w~n                Domain: ~w'-[
          ST, InstName, SP, SV, SVTy, SDTy, SI ] ].
prolog:message(property_value_wrong_type_in(InstType, Instance, IName, Property, DefType, Val, ValTypes)) -->
    { prefix_shorten(Instance, SI),
      prefix_shorten(InstType, ST),
      prefix_shorten(Property, SP),
      prefix_shorten(DefType, SDTy),
      findall(SVT, (member(VT, ValTypes), prefix_shorten(VT, SVT)), SVTys),
      prefix_shorten(Val, SV),
      rdf_literal_val_type(IName, InstName, _)
    },
    [ 'CE-111: ~w instance property ~w . ~w of ~w should be one of ~w but is a ~w~n                Domain: ~w'-[
          ST, InstName, SP, SV, SVTys, SDTy, SI ] ].
prolog:message(missing_any_tgt(Context, SrcClass, SrcInst, SrcIdent, Rel)) -->
    [ 'CE-112-~w: ~w ~w has no ~w target relationships~n                Domain: ~w'-[
          Context, SrcClass, SrcIdent, Rel, SrcInst] ].
prolog:message(missing_tgt(Context, SrcClass, SrcInst, SrcIdent, Rel, TgtClass)) -->
    { rdf_literal_val_type(SrcIdent, IdentName, _)
    },
    [ 'CE-113-~w: ~w ~w missing the ~w target of type ~w~n                Domain: ~w~n'-[
          Context, SrcClass, IdentName, Rel, TgtClass, SrcInst ] ].
prolog:message(invalid_domain(SrcClass, Property, DefinedClass)) -->
    [ 'CE-114: Property ~w was referenced on class ~w, but that property is defined for the unrelated class ~w~n'-[
          Property, SrcClass, DefinedClass] ].
prolog:message(invalid_subclass_domain(SrcClass, Property, ParentProperty, DefinedClass)) -->
    [ 'CE-115: Property ~w was referenced on class ~w, but that property is a sub-type of ~w, which is defined for the unrelated class ~w~n'-[
          Property, SrcClass, ParentProperty, DefinedClass] ].
prolog:message(invalid_property_in_check(Context, SrcClass, Property)) -->
    [ 'CE-116-~w: INVALID CHECK for ~w property on ~w class!~n'-[
          Context, Property, SrcClass] ].
