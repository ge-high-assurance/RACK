% Copyright (c) 2021, Galois, Inc.
%
% All Rights Reserved
%
% This material is based upon work supported by the Defense Advanced Research
% Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
%
% Any opinions, findings and conclusions or recommendations expressed in this
% material are those of the author(s) and do not necessarily reflect the views
% of the Defense Advanced Research Projects Agency (DARPA).

:- module(owl,
    [
        write_owl/3
    ]).

:- use_module(library(semweb/rdf11)).


prolog:message(bad_ns_spec(N)) -->
    [ 'Invalid namespace specification: ~w'-[N] ].


valid_nslist([]).
valid_nslist([N|Ns]) :-
    (
        rdf_current_prefix(N,_), ! ;
        print_message(error, bad_ns_spec(N)), fail
    ),
    valid_nslist(Ns).

:- rdf_register_prefix(syn, 'http://synthetic.data/', [force(true)]).

% Writes the OWL definitions to the specified file, with the provided
% BaseURI and additional namespace declarations.  Note that only the
% definitions from this SADL file are emitted (specified by
% 'graph(G)') to avoid emitting any imported externals.
write_owl(File, BaseURI, [none]) :-
    rdf_default_graph(G), !,
    rdf_save(
        File,
        [
            graph(G), base_uri(BaseURI),
            sorted(true),
            % xml_attributes(false),
            document_language(en)
        ]
    ).
write_owl(File, BaseURI, NS) :-
    rdf_default_graph(G),
    writeln(G),
    valid_nslist(NS),
    rdf_save(
        File,
        [
            graph(G), base_uri(BaseURI),
            sorted(true),
            % xml_attributes(false),
            document_language(en),
            namespaces([owl,xsd|NS])
        ]
    ).
