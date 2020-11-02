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

:- module(view_neighbors, [view_neighbors/1]).

:- ensure_loaded('../paths').

:- use_module(library(gv)).
:- use_module(library(semweb/rdf11)).
:- use_module(rack(model)).
:- use_module(visualization(my_dot_arc)).
:- use_module(visualization(my_dot_node)).

compute_neighbors(Class, Neighbors) :-
    setof(Other, Other^Prop^(rdf(Class, Prop, Other);rdf(Other, Prop, Class)), Neighbors).

%! graph_neighbors(+Out, +Class) is det.
%
%    Computes the dot graph of the given node along with all its neighbors.
graph_neighbors(Out, Class) :-
    my_dot_node(Out, Class),
    compute_neighbors(Class, Neighbors),
    forall(member(N, Neighbors), my_dot_node(Out, N)),
    forall(rdf(Class, Prop, Other), my_dot_arc(Out, Class, Prop, Other)),
    forall(rdf(Other, Prop, Class), my_dot_arc(Out, Other, Prop, Class)).

%! view_neighbors(+Class) is det.
%
%    Displays the neighbors of the given node in the current RDF model.
view_neighbors(Class) :-
    gv_view(
        {Class}/[Out]>>graph_neighbors(Out, Class),
        [
            directed(true),
            method(circo)
        ]
    ).
