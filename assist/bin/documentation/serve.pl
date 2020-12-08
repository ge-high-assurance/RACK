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
