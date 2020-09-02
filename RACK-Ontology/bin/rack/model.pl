/** <module> RACK utility

This module provides facilities for working with the ARCOS RACK
service, including working with modes loaded from (and saved to) local
files or network triple stores, and the ability to convert a data
description DSL into instances in the model.

*/

:- module(rack,
          [
              % Loading and saving RDF/OWL triples
              load_local_model/1,
              save_model_to_file/1,
              save_model_to_file/2,

              load_model_from_url/1,
              upload_model_to_url/1,

              load_model_from_rack/0,
              upload_model_to_rack/0,

              % Ontology relationship predicates
              rack_ref/2,
              ns_ref/3,
              append_fld/3,
              prefix_shorten/2,
              is_owl_class/1,
              owl_list/2,
              entity/1,
              entity/2,
              property/3,
              property_target/5,
              rack_instance/2,
              rack_entity_instance/1,
              rack_entity_instance/3,
              rack_ontology_node/3,
              rdf_literal_val_type/3,

              % Importing user data into the model
              load_data/2,
              rdf_dataref/3,
              load_recognizer/1
          ]).

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdf_http_plugin)).
:- use_module(library(semweb/rdfa)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(semweb/turtle)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_client)).

%% ----------------------------------------------------------------------
%% Support functions

fs_path(Dir, FName, Path) :-
    string_length(Dir, DirLen),
    ( (string_code(DirLen, Dir, 0'/),
       string_concat(Dir, FName, Spec),
       prolog_to_os_filename(Spec, Path)) ;
      (string_concat(Dir, "/", DirPath),
       string_concat(DirPath, FName, Spec),
       prolog_to_os_filename(Spec, Path)) ).

subdir(Dir, Subdir) :-
    directory_files(Dir, ES),
    member(E, ES),
    \+ member(E, ['.', './', './.', '././',
                  '..', '../', './..', './../']),
    fs_path(Dir, E, Subdir),
    exists_directory(Subdir).

file_to_fpath(File, _, File) :- is_absolute_file_name(File), !.
file_to_fpath(File, DirPath, FilePath) :-
    atom_concat(DirPath, '/', X),
    atom_concat(X, File, Y),
    prolog_to_os_filename(Y, FilePath).


%% ----------------------------------------------------------------------
%% OWL data load/store

%! load_local_model(+Dir:string) is semidet.
%! load_local_model(+Dir:atom) is semidet.
%
%  Load an OWL model from the OWL files in the specified directory.
%  This is commonly used as a main entrypoint when importing
%  pre-generated OWL files present in a local directory.
%
%  To import from a web service (e.g. Jena/Fuseki, see
%  load_model_from_url/1 or load_model_from_rack/0.

load_local_model(Dir) :-
    (atom(Dir), ! ; string(Dir), ! ;
     print_message(error, invalid_directory_spec(Dir)), !, fail),
    % rdf_retractall(_,_,_), !,
    directory_files(Dir, AllFiles),
    findall(F, (member(E, AllFiles),
                fs_path(Dir, E, F),
                owlfile(F)),
            Files),
    load_local_model_files(Files),
    findall(D, subdir(Dir, D), Subdirs),
    load_local_model_dirs(Subdirs).

load_local_model_files([]).
load_local_model_files([F|FS]) :-
    load_local_model_file(F),
    load_local_model_files(FS).

load_local_model_file(FP) :-
    rdf_load(FP).

load_local_model_dirs([]).
load_local_model_dirs([D|DS]) :-
    load_local_model(D),
    load_local_model_dirs(DS).

owlfile(F) :- file_name_extension(_, ".owl", F).


%! load_model_from_url(+URL:atom) is semidet.
%
%  Load an OWL model from an HTTP triple-store
%  (e.g. Jena/Fuseki). This defaults to Turtle (trig) format.
%
%  See also load_model_from_rack/0.
%
%  To import from a directory containing =.owl= files, see load_local_model/1.

load_model_from_url(URL) :-
    atom_concat(URL, 'RACK', Src),
    rdf_load(Src, [register_namespaces(true), format(trig)]).


%! load_model_from_rack is semidet.
%
% Load an OWL model from a RACK server's Fuseki endpoint, using Turtle
% (trig) format.
%
% See also load_local_model/1 and load_model_from_url/1.

load_model_from_rack :-
    load_model_from_url('http://localhost:3030/').


%! upload_model_to_url(+URL:atom) is semidet.
%
% Uploads the local RDF/OWL triples to the specified triple-store HTTP
% URL.

upload_model_to_url(URL) :-
    atom_concat(URL, 'RACK/upload', Tgt),
    with_output_to(string(C), (current_output(S), rdf_save(stream(S),[]))),
    % n.b. Fuseki requires a filename for the upload, but subsequently
    % seems to ignore it, so supply a dummy.  The response is HTML
    % code, but the http_post will fail if the HTTP response is not in
    % the 200 range, so the _Response is not useful except for
    % debugging.
    http_post(Tgt,
              form_data(
                  ['data";filename="foo.owl'=mime([type(application/rdf+xml)],
                                                  C, [])]),
              _Response, []).


%! upload_model_to_rack is semidet.
%
% Uploads the local RDF/OWL triples to the RACK fuseki endpoint.
%
% See also upload_model_to_url/1 and save_model_to_file/1.

upload_model_to_rack :-
    upload_model_to_url('http://localhost:3030/').


%! save_model_to_file(+Filename:string) is semidet.
%! save_model_to_file(+Filename:atom) is semidet.
%
% Saves the local RDF/OWL triples to a local Owl file (XML format).
% Like save_model_to_file/2 but saves *all* RDF triples.

save_model_to_file(Filename) :-
    open(Filename, write, Out),
    rdf_save(stream(Out)),
    close(Out).


%! save_model_to_file(+Filename:string, +Namespace:atom) is semidet.
%! save_model_to_file(+Filename:atom, +Namespace:atom) is semidet.
%
% Saves the local RDF/OWL triples for the specified namespace to a
% local Owl file (XML format).
%
% See save_model_to_file/1 to save *all* RDF triples instead.

save_model_to_file(Filename, NS) :-
    open(Filename, write, Out),
    rdf_save(stream(Out), NS),
    close(Out).


%% ----------------------------------------------------------------------
%% Ontology relationships

% Declares a prefix so the term rack:SOFTWARE#FILE is rdf_equal to
% 'http://arcos.rack/SOFTWARE#FILE' to allow shorthand references.

:- initialization(rdf_register_prefix(rack, 'http://arcos.rack/'), now).

%! rack_ref(+Name:atom, -URI:atom) is semidet.
%! rack_ref(-Name:atom, +URI:atom) is semidet.
%
% Used for bi-directional conversions between a Name in a namespace
% and the fully qualified URI reference to that object.  For example,
%
%     rack_ref('SOFTWARE#FILE', rack:SOFTWARE#FILE) :- true.
%     rack_ref('SOFTWARE#FILE', 'http://arcos.rack/SOFTWARE#FILE') :- true
%

rack_ref(Name, URI) :- atom_concat('http://arcos.rack/', Name, URI).


%! ns_ref(+Namespace:atom, ?Target:atom, ?URL:atom) is semidet.
%
% Used to join a Namespace to a Target for a full URL reference, or to
% determine the Target for a URL given the Namespace prefix.

ns_ref(NS, Target, Ref) :- atom_concat(NS, '#', P),
                           atom_concat(P, Target, Ref).

%! append_fld(+Base:atom, +Fld:atom, -Result:atom) is det.
%
% Used to construct an RDF node reference from a base and a suffix
% field name using a regular syntax.

append_fld(Base, Fld, Result) :- atom_concat(Base, '.>', B),
                                 atom_concat(B, Fld, Result).

%! prefix_shorten(+URI:atom, -ShortOrURI) is det
%
% Used to convert a full URI into a shortened version using a prefix
% if one is known, or return the original URI if there is no prefixed
% version available.
%
% ===
% :- prefix_shorten('http://www.w3.org/1999/02/22-rdf-syntax-ns#type', S).
% S = 'rdf:type'.
% :- prefix-shorten('http://something.org/with/no#prefix', S).
% S = 'http://something.org/with/no#prefix'.
% ===
prefix_shorten(URI, ShortOrURI) :-
    rdf_current_prefix(Prefix, Exp), atom_concat(Exp, Local, URI), !,
    atom_concat(Prefix, ':', A),
    atom_concat(A, Local, ShortOrURI).
prefix_shorten(URI, URI).


is_owl_class(E) :-
    rdf(E, rdf:type, owl:'Class').

owl_list(B, PL) :-
    is_owl_class(B),
    rdf_bnode(B),
    rdf_literal(B),
    rdf(B,owl:unionOf,L),
    rdf_list(L),
    rdf_list(L,PL).

%! rack_ontology_node(+Node:atom, -Area:atom, -Item:atom)
%
% True if the Node is defined by RACK and if so, returns the Area and
% Item.
%
% ==
% :- rack_ontology_node('http://arcos.rack/DOCUMENT#DOC_STATUS', Area, Item).
% Area = 'DOCUMENT#'
% Item = 'DOC_STATUS'
% :- rack_ontology_node(owl:'Class', Area, Item).
% false.
% ==

rack_ontology_node(Node, Area, Item) :-
    rdf_split_url(E, Item, Node),
    rack_ref(Area, E).

entity(E) :-
    is_owl_class(E),
    rdf(E, rdfs:subClassOf, rack:'PROV-S#ENTITY'),
    \+ rdf_bnode(E).

entity(E, C) :-
    entity(E),
    rdf(E, rdfs:comment, C).

% TODO: this is a WIP
enumerationOf(E, C) :-
    % atom_length(E, LE),
    % RE is LE - 18,
    % sub_atom(E, 0, 18, RE, 'http://arcos.rack/'),
    rdf(E, rdf:type, C),
    is_owl_class(C).
    % sub_atom(C, 0, 18, _, 'http://arcos.rack/').
% enumerationOf(E, C) :-
%     rack_ref(E, R),
%     rdf(R, rdf:type, C),
%     rack_ref(_AbbrevC, C).
% enumerationOf(E, C) :-
%     rack_ref(_AbbrevE, E),
%     rdf(E, rdf:type, C).
%     rack_ref(_AbbrevC, C).

enumerations(E, ES) :-
    enumerationOf(E, C),
    findall(S, rdf(S, rdf:type, C), ES).


%! property(+Class, -Property, -PropUsage) is multi.
%
%   Used to retrieve a Property and its uniqueness to that Class.
%
%   The PropUsage will be either =unique= or =shared= to indicate if
%   this Property is shared with another Class.

property(Class, Property, unique) :-
    % Property is unique to this class and directly associated
    rdf(Property, rdfs:domain, Class).
property(Class, Property, shared) :-
    % Property is shared with multiple classes, specified in a list.
    rdf(Property, rdfs:domain, Intermediary),
    rdf(Intermediary, owl:unionOf, DomainList),
    rdf_list(DomainList),
    rdf_list(DomainList, Classes),
    member(Class, Classes).

%! property_target(+Class, ?Property, -PropUsage, -Target, -Restrictions) is multi.
%
%   Used to retrieve a Property and Target object for a Class.
%
%   The PropUsage will be either =unique= or =shared= to indicate if
%   this Property is shared with another Class.
%
%   The Restrictions will be either =normal= for no restrictions or
%   =canonical(-Value:Int)= to specify a canonical constraint on the
%   property instances.

property_target(Class, Property, PropUsage, Target, Restrictions) :-
    property(Class, Property, PropUsage),
    rdf(Property, rdfs:range, Target),
    property_extra(Class, Property, Target, Restrictions).
property_target(Class, Property, PropUsage, Target, Restrictions) :-
    rdf(Class, rdfs:subClassOf, Parent),
    property_target(Parent, Property, PropUsage, Target, Restrictions).

property_extra(Class, Property, _Target, cardinality(N)) :-
    rdf(Class, rdfs:subClassOf, B),
    rdf_bnode(B),
    rdf(B, owl:onProperty, Property),
    rdf(B, rdf:type, owl:'Restriction'),
    rdf(B, owl:cardinality, I),
    rdf_literal(I),
    rdf_numeric(I, N).
property_extra(Class, _Property, _Target, normal) :-
    findall(P, (rdf(Class, rdfs:subClassOf, P),
                rdf(P, rdf:type, owl:'Restriction')),  PS),
    length(PS, 0).

rdf_numeric(Value, Num) :- rdf_equal(Value, Num^^xsd:int).
rdf_numeric(Value, Num) :- rdf_equal(Value, Num^^xsd:integer).

%! rdf_literal_val_type(+Literal:atom, -Value:atom, -Type:Atom) is semidet.
%! rdf_literal_val_type(-Literal:atom, +Value:atom, +Type:Atom) is semidet.
%
% Bi-directional conversion between the internal Prolog/RDF
% representation of literal values and the actual value and type.
% There is no validation of the types and values, this is a simple
% join/split equality comparison.
%
% ==
% :- rdf_literal_val_type(42^^'http://www.w3.org/2001/XMLSchema#int', V, T).
% V = 42,
% T = 'http://www.w3.org/2001/XMLSchema#int'
% :- rdf_literal_val_type(L, 42, 'http://www.w3.org/2001/XMLSchema#int').
% L = 42^^'http://www.w3.org/2001/XMLSchema#int'
% ==

rdf_literal_val_type(Literal, Value, Type) :-
    rdf_equal(Literal, Value^^Type).


%! rack_instance(+OntologyClassName:atom, -InstanceURL:atom) is nondet
%
% Used to return instances of the corresponding ontology class (by name).
%
% ==
% :- rack_instance('SOFTWARE#FILE', I).
% I = 'http://TurnstileSystem/CounterApplication/counter.c' ;
% ...
% ==

rack_instance(OntologyClassName, InstanceURL) :-
    rack_ref(OntologyClassName, Ref),
    rdf(InstanceURL, rdf:type, Ref).


%! rack_entity_instance(-InstanceURL:atom) is nondet
%
% Used to return instances of ontology ENTITY objects (or subclasses
% thereof).

rack_entity_instance(InstanceURL) :-
    entity(E),
    rdf(InstanceURL, rdf:type, E).

%! rack_entity_instance(+Namespace:atom, ?ClassName:atom, -InstanceURL:atom) is nondet
%
% Used to return instances of ontology ENTITY objects existing in the
% specified namespace.
%
% ==
% :- rack_entity_instance('HTTP://TurnstileSystem/CounterApplication', I).
% I = 'http://TurnstileSystem/CounterApplication/counter.c'
% ==

rack_entity_instance(Namespace, ClassName, InstanceURL) :-
    entity(E),
    rdf(InstanceURL, rdf:type, E),
    % rack_entity_instance(InstanceURL),
    ns_ref(Namespace, _, InstanceURL),
    rack_ref(ClassName, E).

% TODO: rack_activity_instance, rack_agent_instance

%% ----------------------------------------------------------------------
%% Loading generated data from .rack files

:- dynamic rack_namespace/1.

%! load_data(+Namespace:atom, +Dir:atom) is semidet.
%
% load_data/2 is the main entrypoint called to load data declarations
% from =.rack= files in or below the specified directory.  The =.rack=
% files are usually generated or user-created to describe entities or
% activities.

load_data(Namespace, Dir) :-
    % Globally asserts current namespace so that this doesn't need to
    % be threaded as an argument through all the rules.
    assert(rack_namespace(Namespace), SetNS),
    % Find all files admitted by 'rack_datafile' recursively starting at 'Dir'
    load_data_dir(Dir),
    % Instantiate all the loaded data
    realize_loaded_data,
    % Remove the global namespace assertion
    erase(SetNS).


realize_loaded_data :-
    % n.b. use findall to force all backtracking here, otherwise
    % =erase(SetNS)= in load_data/2 will run after the first success
    % and no more instances will be instantiated.
    findall(Instance, rdf_dataref(_RDFClass, load_data, Instance), Instances),
    length(Instances, Count),
    rack_namespace(Namespace),
    print_message(informational, loaded_data_instances(Namespace, Count)).


load_data_dir(Dir) :-
    directory_files(Dir, AllFiles),
    findall(F, (member(E, AllFiles),
                fs_path(Dir, E, F),
                rack_datafile(F)),
            Files),
    load_data_files(Files),
    findall(D, subdir(Dir, D), Subdirs),
    load_data_dirs(Subdirs).

load_data_dirs([]).
load_data_dirs([D|DS]) :-
    load_data_dir(D),
    load_data_dirs(DS).

load_data_files([]).
load_data_files([F|FS]) :-
    load_data_file(F),
    load_data_files(FS).

load_data_file(F) :-
    rack_namespace(Namespace),
    print_message(informational, loading_rack_datafile(Namespace, F)),
    consult(F).

rack_datafile(F) :- file_name_extension(_, ".rack", F).

prolog:message(invalid_directory_spec(_Dir)) -->
    [ 'invalid directory specification, must be an atom or a string' ].
prolog:message(loading_rack_datafile(Namespace, FP)) -->
    [ 'loading data into ~w from ~w ... '-[Namespace, FP] ].
prolog:message(loaded_data_instances(Namespace, Count)) -->
    [ 'loaded ~d data instances into namespace ~w'-[Count, Namespace] ].


%% ----------------------------------------------------------------------
%% Conversion of loaded data into RACK ontology RDF triples


% display triples in debug mode.  Enable and disable via
%
%     :- debug(triples).
%     :- nodebug(triples).

show_triples :- rdf(S,P,O), show_triple(S,P,O).
show_triple(S,P,O) :-
    debug(triples, 'Triple: ~w --~w--> ~w~n', [S, P, O]).


% Convenience routine to add a new triple with a debug statement (if
% enabled) and assigning it to a graph corresponding to the defined
% rack_namespace (if any).

add_triple(S,P,O) :-
    show_triple(S, P, O),
    (rack_namespace(NS), rdf_assert(S, P, O, NS)) ;
    (\+ rack_namespace(_), rdf_assert(S, P, O)).


%! rdf_dataref(-RDFClass, +Data, -Instance) is semidet.
%! rdf_dataref(+RDFClass, +Data, -Instance) is semidet.
%
% Top-level rule to determine instances of an RDFClass given the
% imported descriptive Data (or a derivation thereof).  Used by
% load_data/2.

rdf_dataref(RDFClass, Data, Instance) :-
    rdf(RDFClass, rdf:type, owl:'Class'),
    rack_ref(ShortC, RDFClass),
    data_instance(ShortC, Data, InstanceSuffix, InstanceData),
    rack_namespace(NS),
    ns_ref(NS, InstanceSuffix, Instance),
    add_triple(Instance, rdf:type, RDFClass),
    (add_rdfdata(RDFClass, RDFClass, Instance, InstanceData); true).

add_rdfdata(RDFClass, Class, DataRef, Data) :-
    (rdf(Class, rdfs:subClassOf, Parent), !,
     (add_rdfdata(RDFClass, Parent, DataRef, Data);true) ;
     true),
    property(Class, Property, _),
    rack_ref(ShortC, RDFClass),
    rack_ref(ShortP, Property),
    add_rdfproperty(ShortC, ShortP, RDFClass, Property, DataRef, Data).

add_rdfproperty(ShortC, ShortP, _RDFClass, Property, DataRef, Data) :-
    data_get(ShortC, ShortP, Data, Value),
    (atom(Value), !, rack_namespace(NS), ns_ref(NS, Value, TargetRef),
     ((rdf(DataRef, Property, TargetRef), ! ;
       rdf(Property, rdfs:range, TargetType),
       add_triple(TargetRef, rdf:type, TargetType)));
     TargetRef = Value),
    add_triple(DataRef, Property, TargetRef).


%% ----------------------------------------------------------------------
%% ----+ Recognizers for Loaded Data

%! load_recognizer(+FPath:path) is semidet
%
% Used to load a set of Data recognizers from a file.  This can be
% used with multiple files to aggregate recognizers.
%
% Data recognizers are invoked by the load_data/2 and rdf_dataref/3
% process to recognize the relationship between information in the
% input =.rack= data files and specific ontology elements.
%
% Data recognizers are an evolving active mapping between RACK user
% data and the RACK core ontology; new user data should be
% accompanied by recognizers for that data.
%
% The recognizers are only needed for discrete, singular components;
% the relationships between the components identified by the
% recognizers is automatically identified by using the RACK ontology
% model to determine those relationships.
%
% There are two recognizers: data_instance/4 and data_get/4.
%
%   * data_instance(+OntologyObjectName:atom, +Data, -Instance, -InstanceData)
%     The data_instance recognizer is used to associate Data with the
%     OntologyObjectName.  The returned Instance is the rdf Subject URL
%     to be used to describe this instance, and the InstanceData is
%     used to perform associated field (and transitive object)
%     recognition via the data_get/4 recognizer.
%
%   * data_get(+OntologyObjectName:atom, +OntologyPropertyName:atom, +Data, -Value)
%     The data_get recognizer is used to determine the value for a the
%     specified property of the specified object, based on the passed
%     Data (which is usually the InstanceData returned by the
%     data_instance/4 for the object).  The value may be a scalar value
%     (like xsd:string) or it may be another data object Instance.
%
%
% The OntologyObjectName and OntologyPropertyName passed to both
% data_instance/4 and data_get/4 are the short names for the RACK
% ontology item.  They are both expanded to a full URI via the
% rack_ref/2 rule.

load_recognizer(FPath) :-
    consult([FPath]).

% Some common data recognizers are provided below, but each data
% tool/format will usually be accompanied by a set of recognizers for
% that data.

:- multifile data_instance/4, data_get/4.

data_get('SOFTWARE#FILE', 'SOFTWARE#filename', sw_file_data(_, Dir, NameOrPath), Value) :-
    file_to_fpath(NameOrPath, Dir, Path),
    atom_string(Path, Value).
