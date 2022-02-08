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

% Common command-line option handling for RACK prolog utilities

:- ensure_loaded('./paths').

:- use_module(rack(model)).
:- use_module(library(url)).

opts_spec(Spec) :-
    paths_dir(Dir),
    file_directory_name(Dir, AssistDir),
    file_directory_name(AssistDir, RACKRootDir),
    atom_concat(RACKRootDir, '/RACK-Ontology/OwlModels', OwlDir),
    atom_concat(AssistDir, '/databin', DataBinDir),
    atom_concat(DataBinDir, '/databin.rack', DBRack),
    Spec =
    [ [opt(verbose), type(boolean), default(false),
       shortflags([v]), longflags(['verbose']),
       help('Enable verbose output')],

      [opt(declare), type(boolean), default(false),
       shortflags(['D']), longflags(['declare']),
       help('Declare generated RDF triples to stdout')],

      [opt(ontology_dir), meta('DIR_OR_URL'), type(atom),
       shortflags([m]), longflags(['ontology', 'model']),
       default(OwlDir),
       help('Where to load ontology .Owl files from')],

      [opt(recognizers), meta('FILE'), type(atom),
       shortflags([r]), longflags(['recognizer', 'recognizers']),
       default(DBRack),
       help(['File containing data recognizers to use for',
             'loading data from tool generated output and',
             'converting it to ontology elements.'])],

      [opt(data_dir), meta('DIR'), type(atom),
       shortflags([d]), longflags(['data']),
       default(skip),
       help(['Directory root to load tool-generated data from.',
             'All tool generated files located in this tree',
             'will be imported and converted to ontology elements.'])],

      [opt(data_namespace), meta('NS'), type(atom),
       shortflags([n]), longflags(['namespace']),
       default('http://testdata.org/test'),
       help('Namespace to load tool-generated data into when importing from the data directory (see --data).')],

      [opt(help), type(boolean), default(false),
       shortflags([h]), longflags(['help']),
       help('Display this help')]

    ].

parse_args(ExtraArgs, Opts, PosArgs, HelpBanner) :-
    opts_spec(OptsSpec),
    append(OptsSpec, ExtraArgs, OptSpec),
    opt_arguments(OptSpec, Opts, PosArgs),
    % write('Opts: '), write(Opts), nl,
    % write('PosArgs: '), write(PosArgs), nl,
    display_help(Opts, OptSpec, HelpBanner),
    set_verbosity(Opts),
    set_declaration_level(Opts),
    get_ontology_dir(Opts, ODir),
    print_message(informational, loading_ontology_dir(ODir)),

    ((catch(exists_directory(ODir),
            % If the above throws the following error, run the
            % following step instead.  This looks wrong... one would
            % expect the iri_scheme error to be thrown by an attempt
            % to use the ODir as a URL, but oddly this is the error
            % that exists_directory returns if given input like
            % 'http://something...', and the web-based access does
            % *not* throw this error.
            error(existence_error(iri_scheme,http),_),
            fail),
      !, % The directory exists, so commit to loading from it
      load_local_model(ODir)) ;
     load_model_from_url(ODir)
    ),
    load_recognizers(Opts),
    load_data_from_dir(Opts).

display_help(Opts, OptSpec, HelpBanner) :-
    member(help(true), Opts),
    help_abort(OptSpec, HelpBanner).
display_help(Opts, _, _) :- member(help(false), Opts).

help_abort(none, HelpBanner) :-
    opts_spec(Spec),
    !,
    help_abort(Spec, HelpBanner).

help_abort(OptSpec, HelpBanner) :-
    display_banner(HelpBanner),
    opt_help(OptSpec, Help),
    write(Help),
    halt.

display_banner([]).
display_banner([L|Ls]) :- writeln(L), display_banner(Ls).

get_ontology_dir(Opts, Path) :-
    member(ontology_dir(Spec), Opts), !,
    fix_ontology_path(Spec, Path).
get_ontology_dir(_, '.').

% A common mistake is to specify "-m http://localhost:3030" and omit
% the trailing slash, which will fail.  The following attempts to
% auto-correct for this mistake.
fix_ontology_path(Spec, Path) :-
    is_absolute_url(Spec),
    parse_url(Spec, Parts),
    member(path('/'), Parts),  % true for http://localhost:3030 and http://localhost:3030/, but not http://localhost:3030/something
    \+ member(fragment(_), Parts),
    \+ member(search(_), Parts),
    % OK, this is a plain URL that should have a / as a path
    !,
    member(host(Host), Parts),
    member(protocol(Proto), Parts),
    member(port(Port), Parts),
    parse_url(Path, [protocol(Proto), host(Host), port(Port), path('/')]).
fix_ontology_path(Spec, Spec).

load_recognizers(Opts) :-
    member(recognizers(R), Opts),
    load_recognizer(R).

load_data_from_dir(Opts) :-
    member(data_dir(D), Opts),
    (D == skip, !;
     member(data_namespace(NS), Opts),
     print_message(informational, loading_data(NS, D)),
     load_data(NS, D)
    ).

set_verbosity([]).
set_verbosity([verbose(true)|_]) :- set_prolog_flag(verbose, normal).
set_verbosity([verbose(false)|_]) :- set_prolog_flag(verbose, silent).
set_verbosity([_|Opts]) :- set_verbosity(Opts).

set_declaration_level(Opts) :-
    member(declare(true), Opts), !,
    debug(triples).
set_declaration_level(_).


prolog:message(loading_ontology_dir(D)) -->
    [ 'loading ontology from ~w'-[D] ].
prolog:message(loading_data(NS, D)) -->
    [ 'loading data from ~w into ~w'-[D, NS] ].
prolog:message(bad_arguments) -->
    [ 'Bad argument specification!' ].
