:- ensure_loaded('../paths').

:- doc_server(4040).
:- portray_text(true).

:- load_files(ontology(hazard)).
:- load_files(ontology(requirements)).
:- load_files(ontology(testing)).
:- load_files(queries(hazard_structure)).

:- load_files(rack(analyze)).
:- load_files(rack(check)).
:- load_files(rack(write_ontology)).
:- load_files(rack(model)).
