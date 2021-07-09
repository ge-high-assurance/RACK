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

:- module(
    write_ontology,
    [
        write_ontology/0
    ]
).

:- ensure_loaded('../paths').

:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).
:- use_module(utils(zip_by_key)).

prolog:message(wrote_ontology(FilePath)) -->
    [ 'Wrote ~w'-[FilePath] ].

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
%    qualified name, stripped of the 'http://arcos.rack' URI prefix.
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

rack_property(Property) :-
    rdf(Qualified, rdf:type, owl:'DatatypeProperty'), rack_ref(Property, Qualified).
rack_property(Property) :-
    rdf(Qualified, rdf:type, owl:'ObjectProperty'), rack_ref(Property, Qualified).

rack_property_decomposed(Namespace, Name) :-
    rack_property(Property), rack_qualified(Namespace, Name, Property).

rack_properties(Properties) :-
    setof(Namespace-Name, rack_property_decomposed(Namespace, Name), Pairs),
    group_pairs_by_key(Pairs, Properties).

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


%! export_class(+Class, -Export) is det.
%
%    Gives the export string for RDF query of a class.
export_class(Class, Export) :-
    string_lower(Class, LowercaseClass),
    atomic_list_concat([LowercaseClass, '/1'], Export).


%! export_assert_class(+Class, -Export) is det.
%
%    Gives the export string for RDF assertion of a class.
export_assert_class(Class, Export) :-
    string_lower(Class, LowercaseClass),
    atomic_list_concat(['assert_', LowercaseClass, '/1'], Export).


%! export_property(+Property, -Export) is det.
%
%    Gives the export string for RDF query of a property.
export_property(Property, Export) :-
    atomic_list_concat([Property, '/2'], Export).


%! export_assert_property(+Property, -Export) is det.
%
%    Gives the export string for RDF assertion of a property.
export_assert_property(Property, Export) :-
    atomic_list_concat(['assert_', Property, '/2'], Export).


replicate(Item, Count, List) :-
    length(List, Count),
    maplist(=(Item), List).

indentation_spaces(Spaces) :- Spaces is 4.

increase_indentation(OldIndentation, NewIndentation) :-
    indentation_spaces(Spaces),
    replicate(' ', Spaces, IndentationList),
    atomic_list_concat(IndentationList, Indentation),
    atomic_list_concat([OldIndentation, Indentation], NewIndentation).

decrease_indentation(OldIndentation, NewIndentation) :-
    string_length(OldIndentation, OldLength),
    indentation_spaces(Spaces),
    NewLength is OldLength - Spaces,
    sub_string(OldIndentation, 0, NewLength, _, NewIndentation).

writeln_indented(Handle, Indentation, Contents) :-
    atomic_list_concat([Indentation, Contents], WhatToWrite),
    writeln(Handle, WhatToWrite).

%! write_exports(+Handle, +Indent, +Classes, +Properties) is det.
%
%    Writes the module export lines for all given ontology classes and
%    properties.
write_exports(Handle, Indent, Classes, Properties) :-
    atomic_list_concat([',\n', Indent], Separator),
    maplist(export_assert_class, Classes, ExportAssertClasses),
    maplist(export_class, Classes, ExportClasses),
    maplist(export_assert_property, Properties, ExportAssertProperties),
    maplist(export_property, Properties, ExportProperties),
    append([
        ExportAssertClasses,
        ExportClasses,
        ExportAssertProperties,
        ExportProperties
    ], AllExports),
    atomic_list_concat(AllExports, Separator, Exports),
    write(Handle, Indent),
    writeln(Handle, Exports).


%! class_query_declaration(+Namespace, +Class, -Declaration) is det.
%
%    Computes the text declaration for the RDF query of a given class in some
%    namespace.
class_query_declaration(Namespace, Class, Declaration) :-
    string_lower(Class, Predicate),
    atomic_list_concat(
        [ Predicate, '(C) :- rack_instance(\'', Namespace, '#', Class, '\', C).'],
        Declaration
    ).


%! class_assert_declaration(+Namespace, +Class, -Declaration) is det.
%
%    Computes the text declaration for the RDF assertion of a given class in
%    some namespace.
class_assert_declaration(Namespace, Class, Declaration) :-
    string_lower(Class, PredicateSuffix),
    atomic_list_concat(['assert_', PredicateSuffix], Predicate),
    atomic_list_concat(
        [ Predicate, '(C) :- rack_instance_assert(\'', Namespace, '#', Class, '\', C).'],
        Declaration
    ).


%! write_class_query(+Handle, +Namespace, +Class) is det.
%
%    Writes the text declaration for the RDF query of a given class in some
%    namespace.
write_class_query(Handle, Namespace, Class) :-
    class_query_declaration(Namespace, Class, Declaration),
    writeln(Handle, Declaration).


%! write_class_assert(+Handle, +Namespace, +Class) is det.
%
%    Writes the text declaration for the RDF query of a given class in some
%    namespace.
write_class_assert(Handle, Namespace, Class) :-
    class_assert_declaration(Namespace, Class, Declaration),
    writeln(Handle, Declaration).


%! property_query_declaration(Namespace, Property, Declaration) is det.
%
%    Computes the text declaration for a given property in some namespace.
property_query_declaration(Namespace, Property, Declaration) :-
    atomic_list_concat(
        [ Property, '(A, B) :- rdf(A, rack:\'', Namespace, '#', Property, '\', B).'],
        Declaration
    ).

%! property_assert_declaration(Namespace, Property, Declaration) is det.
%
%    Computes the text declaration for a given property in some namespace.
property_assert_declaration(Namespace, Property, Declaration) :-
    atomic_list_concat(['assert_', Property], AssertProperty),
    atomic_list_concat(
        [
            AssertProperty,
            '(A, B) :- rack_property_assert(A, rack:\'', Namespace, '#', Property, '\', B).'
        ],
        Declaration
    ).

%! write_property_query(+Handle, +Namespace, +Property) is det.
%
%    Writes the text declaration for a given property in some namespace.
write_property_query(Handle, Namespace, Property) :-
    property_query_declaration(Namespace, Property, Declaration),
    writeln(Handle, Declaration).


%! write_property_assert(+Handle, +Namespace, +Property) is det.
%
%    Writes the text declaration for a given property in some namespace.
write_property_assert(Handle, Namespace, Property) :-
    property_assert_declaration(Namespace, Property, Declaration),
    writeln(Handle, Declaration).


%! use_quotes_if_contains_dashes(+In, -Out) is det.
%
%    Adds single quotes around input string if it contains dashes.
use_quotes_if_contains_dashes(Good, Good) :-
    \+ sub_string(Good, _, 1, _, '-').
use_quotes_if_contains_dashes(Bad, Good) :-
    sub_string(Bad, _, 1, _, '-'), atomic_list_concat(['\'', Bad, '\''], Good).

% Prolog gets unhappy when we try to name a module 'system'.  It works fine with
% 'use_module', but the documentation is generated using 'load_files', and
% modules named 'system' cause permission errors.
special_case_for_system("system", "'rack-system'") :- !.
special_case_for_system(In, In) :- \+ In = "system".

%! write_ontolofy_file(+Handle, +Namespace, +Classes, +Properties) is det.
%
%    Writes the ontology file for a given namespace, list of ontology classes,
%    and list of ontology properties.
write_ontology_file(Namespace, Classes, Properties) :-
    ontology_file_path(Namespace, FilePath),
    open(FilePath, write, Handle),
    % trick for "function composition"
    foldl(
        call,
        [
            string_lower,
            use_quotes_if_contains_dashes,
            special_case_for_system
        ],
        Namespace,
        Module
    ),
    writeln(Handle, '% THIS FILE WAS AUTOMATICALLY GENERATED, SEE README'),
    nl(Handle),
    writeln(Handle, ':- module('),
    NoIndentation = '',
    increase_indentation(NoIndentation, OneIndentation),
    increase_indentation(OneIndentation, TwoIndentation),
    atomic_list_concat(['\'', Module, '\','], ModuleLine),
    writeln_indented(Handle, OneIndentation, ModuleLine),
    writeln_indented(Handle, OneIndentation, '['),
    write_exports(Handle, TwoIndentation, Classes, Properties),
    writeln_indented(Handle, OneIndentation, ']).'),
    nl(Handle),
    writeln(Handle, ':- ensure_loaded(\'../paths\').'),
    nl(Handle),
    writeln(Handle, ':- use_module(library(semweb/rdf11)).'),
    writeln(Handle, ':- use_module('),
    writeln(Handle, '    rack(model),'),
    writeln(Handle, '    ['),
    writeln(Handle, '        rack_instance/2,'),
    writeln(Handle, '        rack_instance_assert/2,'),
    writeln(Handle, '        rack_property_assert/3'),
    writeln(Handle, '    ]'),
    writeln(Handle, ').'),
    nl(Handle),
    writeln(Handle, '% Assert RACK classes: always succeeds, adding to the database.'),
    forall(member(Class, Classes), write_class_assert(Handle, Namespace, Class)),
    nl(Handle),
    writeln(Handle, '% Query RACK classes: returns existing instances in the database, if any.'),
    forall(member(Class, Classes), write_class_query(Handle, Namespace, Class)),
    nl(Handle),
    writeln(Handle, '% Assert RACK properties: always succeeds, adding to the database.'),
    forall(member(Property, Properties), write_property_assert(Handle, Namespace, Property)),
    nl(Handle),
    writeln(Handle, '% Query RACK properties: returns existing instances in the database, if any.'),
    forall(member(Property, Properties), write_property_query(Handle, Namespace, Property)),
    close(Handle),
    % let the user know which files are being written
    print_message(informational, wrote_ontology(FilePath)).

%! write_ontology is det.
%
%    Writes the ontology files for all classes and properties in the in-memory
%    model, outputting Prolog files in the ontology folder.
write_ontology :-
    rack_classes(ClassesGroups),
    rack_properties(PropertiesGroups),
    zip_by_key(ClassesGroups, [], PropertiesGroups, [], Groups),
    forall(
        member(Namespace-Classes-Properties, Groups),
        write_ontology_file(Namespace, Classes, Properties)
    ).
