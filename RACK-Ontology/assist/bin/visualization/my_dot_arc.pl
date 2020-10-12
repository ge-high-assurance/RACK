% Copyright (c) 2020, General Electric Company and Galois, Inc.
:- module(my_dot_arc, [my_dot_arc/4]).

:- ensure_loaded('../paths').

:- use_module(library(gv)).

%! my_dot_arc(+Out, +Class, +Prop, +Class) is det.
%
%    Wrapper around dot_arc to have uniform options.
my_dot_arc(Out, Class, Prop, Other) :-
    % Props tend to have very long names, let's just keep the end
    split_string(Prop, "/", "", Decomposed),
    last(Decomposed, Label),
    dot_arc(
        Out, Class, Other,
        [
            label(Label)
        ]
    ).
