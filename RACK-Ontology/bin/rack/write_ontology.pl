:- module(write_ontology,
          [
           write_ontology/0
          ]).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).

%! rack_qualified(+Namespace, +Name, -Qualified) is det.
%
%    Computes the RACK-qualified name given a namespace and a name.
%
%      rack_qualified('SOFTWARE', 'FILE', 'SOFTWARE#FILE') :- true.
rack_qualified(Namespace, Name, Qualified) :-
    atomic_list_concat([Namespace, Name], '#', Qualified).

%! rack_class(-Qualified) is nondet.
%
%    Enumerates RACK classes from the current in-memory model.  Output is a
%    qualified name, stripped of the 'http://arcox.rack' URI prefix.
%
%      rack_class('SOFTWARE#FILE') :- true.   % assuming it is in the model
rack_class(Qualified) :-
    is_owl_class(Class), rack_ref(Qualified, Class).

rack_class_decomposed(Namespace, Name) :-
    rack_class(Class), rack_qualified(Namespace, Name, Class).

%! rack_classes(Classes) is det.
%
%    Gathers all classes in the loaded model, and groups them by namespace.
%    Classes will be a list of pairs, where the first component is the
%    namespace, and the second component is the list of classes for this
%    namespace, e.g.:
%
%      ['Namespace1'-['A', 'B'], 'Namespace2'-['C' 'D']].
rack_classes(Classes) :-
    setof(Namespace-Name, rack_class_decomposed(Namespace, Name), Pairs),
    group_pairs_by_key(Pairs, Classes).

rack_property(Qualified) :-
    rdf(_, Class, _), rack_ref(Qualified, Class).

rack_property_decomposed(Namespace, Name) :-
    rack_property(Property), rack_qualified(Namespace, Name, Property).

rack_properties(Properties) :-
    setof(Namespace-Name, rack_property_decomposed(Namespace, Name), Pairs),
    group_pairs_by_key(Pairs, Properties).

% NOTE (val) I feel like there is a smarter way of doing this
%! zip_by_key_aux(+KeyValues1, +KeyValues2, ?Key, -Values1, -Values2) is det.
%
%    Enumerates all keys from the union of the keys of the first two arguments.
%    For each key, outputs the values for that key in each of the key-value
%    lists.  If a key appears only in one of the key-value lists, its value for
%    the other list is considered to be [].
%
%      zip_by_key_aux(['Key1'-A1], ['Key1'-A2], 'Key1', A1, A2) :- true.
%      zip_by_key_aux(['Key1'-A1], [],          'Key1', A1, []) :- true.
%      zip_by_key_aux([],          ['Key1'-A2], 'Key1', [], A2) :- true.
zip_by_key_aux(KeyValues1, KeyValues2, NS, Values1, []) :-
    member(NS-Values1, KeyValues1), \+ member(NS-_, KeyValues2).
zip_by_key_aux(KeyValues1, KeyValues2, NS, [], Values2) :-
    \+ member(NS-_, KeyValues1), member(NS-Values2, KeyValues2).
zip_by_key_aux(KeyValues1, KeyValues2, NS, Values1, Values2) :-
    member(NS-Values1, KeyValues1), member(NS-Values2, KeyValues2).

%! zip_by_key(+KeyValuesIn1, +KeyValuesIn2, -KeyValuesOut) is det.
%
%    Takes two keyed collections of lists:
%
%      KeyValuesIn1 = ['Key1'-['V1-a', 'V1-b', 'V1-c'], 'Key2'-['V2-a']]
%      KeyValuesIn2 = ['Key1'-['V1-d', 'V1-e'], 'Key3'-['V3-a', 'V3-b']]
%
%    and returns keyed triples, where the second component comes from
%    KeyValuesIn1, and the third component comes from KeyValuesIn2, and any
%    missing component is replaced by [] i.e.
%
%      KeyValuesOut = [ 'Key1'-['V1-a', 'V1-b', 'V1-c']-['V1-d', 'V1-e']
%                    , 'Key2-['V2-a']-[]
%                    , 'Key3'-[]-['V3-a']
%                    ]
%
%    This lets us take a keyed list of classes, and a keyed list of properties,
%    where the keys overlap, and obtained a keyed list of triples
%    (Key-Classes-Properties), so that we can generate the ontology file for
%    that Key in one go.
%
%    (NOTE: if needs ever be, the default value could become a parameter)
zip_by_key(KeyValuesIn1, KeyValuesIn2, KeyValuesOut) :-
    setof(
     NS-Values1-Values2,
     zip_by_key_aux(KeyValuesIn1, KeyValuesIn2, NS, Values1, Values2),
     KeyValuesOut
    ).

%! ontology_directory(-Dir) is det.
%
%    Computes the path to the `ontology` directory.
ontology_directory(Dir) :-
    paths_dir(PathsDir),
    directory_file_path(PathsDir, 'ontology', Dir).

%! ontology_file_path(+Namespace, -FilePath) is det.
%
%    Returns the file path to the ontology file corresponding to a namespace.
ontology_file_path(Namespace, FilePath) :-
    string_lower(Namespace, Module),
    ontology_directory(OntologyDir),
    atom_concat(Module, '.pl', FileName),
    directory_file_path(OntologyDir, FileName, FilePath).

%! write_export_for_thing(+Handle, +Thing) is det.
%
%    Writes the module export line for a given ontology thing.
write_export_for_thing(Handle, Thing) :-
    string_lower(Thing, LowercaseThing),
    write(Handle, '            '),
    write(Handle, LowercaseThing),
    writeln(Handle, '/1,').

%! write_export_for_things(+Handle, +Things) is det.
%
%    Writes the module export lines for all given ontology things.
write_exports_for_things(Handle, Things) :-
    forall(member(Thing, Things), write_export_for_thing(Handle, Thing)).

%! write_export_for_properties(+Handle, +Thing) is det.
%
%    Writes the module export lines for all given ontology properties.
write_exports_for_properties(_, []).
write_exports_for_properties(Handle, [Property|Properties]) :-
    write(Handle, '            '),
    write(Handle, Property),
    write(Handle, '/2'),
    % No comma after the last property
    ( length(Properties, 0)
    -> nl(Handle)
    ; writeln(Handle, ','), write_exports_for_properties(Handle, Properties)
    ).

%! class_declaration(+Namespace, +Class, -Declaration) is det.
%
%    Computes the text declaration for a given class in some namespace.
class_declaration(Namespace, Class, Declaration) :-
    string_lower(Class, Predicate),
    atomic_list_concat(
     [ Predicate, '(C) :- rack_instance(\'', Namespace, '#', Class, '\', C).'],
     Declaration
    ).

%! write_class(+Handle, +Namespace, +Class) is det.
%
%    Writes the text declaration for a given class in some namespace.
write_class(Handle, Namespace, Class) :-
    class_declaration(Namespace, Class, Declaration),
    writeln(Handle, Declaration).

%! property_declaration(Namespace, Property, Declaration) is det.
%
%    Computes the text declaration for a given property in some namespace.
property_declaration(Namespace, Property, Declaration) :-
    atomic_list_concat(
     [ Property, '(A, B) :- rdf(A, rack:\'', Namespace, '#', Property, '\', B).'],
     Declaration
    ).

%! write_property(+Handle, +Namespace, +Property) is det.
%
%    Writes the text declaration for a given property in some namespace.
write_property(Handle, Namespace, Property) :-
    property_declaration(Namespace, Property, Declaration),
    writeln(Handle, Declaration).

%! write_ontolofy_file(+Handle, +Namespace, +Things, +Properties) is det.
%
%    Writes the ontology file for a given namespace, list of ontology things,
%    and list of ontology properties.
write_ontology_file(Namespace, Things, Properties) :-
    ontology_file_path(Namespace, FilePath),
    open(FilePath, write, Handle),
    string_lower(Namespace, Module),
    writeln(Handle, '% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README'),
    nl(Handle),
    atomic_list_concat([':- module(', Module, ','], FirstLine),
    writeln(Handle, FirstLine),
    writeln(Handle, '          ['),
    write_exports_for_things(Handle, Things),
    write_exports_for_properties(Handle, Properties),
    writeln(Handle, '          ]).'),
    nl(Handle),
    writeln(Handle, ':- ensure_loaded(\'../paths\').'),
    nl(Handle),
    writeln(Handle, ':- use_module(library(semweb/rdf11)).'),
    writeln(Handle, ':- use_module(rack(model)).'),
    nl(Handle),
    forall(member(Thing, Things), write_class(Handle, Namespace, Thing)),
    nl(Handle),
    forall(member(Property, Properties), write_property(Handle, Namespace, Property)),
    close(Handle).

%! write_ontology() is det.
%
%    Writes the ontology files for all things and properties in the in-memory
%    model, outputting Prolog files in the ontology folder.
write_ontology() :-
    rack_classes(ClassesGroups),
    rack_properties(PropertiesGroups),
    zip_by_key(ClassesGroups, PropertiesGroups, Groups),
    forall(
     member(Namespace-Classes-Properties, Groups),
     write_ontology_file(Namespace, Classes, Properties)
    ).
