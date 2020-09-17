:- module(my_dot_node, [my_dot_node/2]).

:- ensure_loaded('../paths').

:- use_module(library(gv)).

%! my_dot_node(+Out, +Class) is det.
%
%    Wrapper around dot_node to have uniform options.
my_dot_node(Out, Class) :-
    Class = ^^(A, _),
    dot_node(Out, Class, [label(A)]).
my_dot_node(Out, Class) :-
    \+ Class = ^^(_, _),
    % Classes tend to have very long names, let's just keep the end
    split_string(Class, "/", "", Decomposed),
    last(Decomposed, Label),
    dot_node(Out, Class, [label(Label)]).
