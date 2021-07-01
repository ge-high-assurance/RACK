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

:- ensure_loaded('../paths').

:- doc_server(4040).
:- portray_text(true).

% Documentation for ontology generated files, if any.
:-
    paths_dir(Paths),
    directory_file_path(Paths, 'ontology', OntologyDir),
    directory_files(OntologyDir, Files),
    forall(
        (
            member(OntologyFile, Files),
            sub_atom(OntologyFile, DotPosition, _, _, '.pl'),
            sub_atom(OntologyFile, 0, DotPosition, _, ModuleName)
        ),
        load_files(ontology(ModuleName))
    ).

:- load_files(rack(analyze)).
:- load_files(rack(check)).
:- load_files(rack(model)).
:- load_files(rack(write_ontology)).

:- load_files(utils(float_equality)).
:- load_files(utils(transitive_closure)).
:- load_files(utils(zip_by_key)).

% These are currently not maintained, so rather not advertised.
% :- load_files(visualization(my_dot_arc)).
% :- load_files(visualization(my_dot_node)).
% :- load_files(visualization(view_neighbors)).
