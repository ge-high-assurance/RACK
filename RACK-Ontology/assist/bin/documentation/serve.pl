% Copyright (c) 2020, General Electric Company and Galois, Inc.
:- ensure_loaded('../paths').

:- doc_server(4040).
:- portray_text(true).

:- load_files(checks(bdu)).

:- load_files(ontology(hazard)).
:- load_files(ontology(requirements)).
:- load_files(ontology(testing)).

:- load_files(queries(hazard_structure)).

:- load_files(rack(analyze)).
:- load_files(rack(check)).
:- load_files(rack(write_ontology)).
:- load_files(rack(model)).

:- load_files(utils(transitive_closure)).
:- load_files(utils(zip_by_key)).

:- load_files(visualization(my_dot_arc)).
:- load_files(visualization(my_dot_node)).
:- load_files(visualization(view_neighbors)).
