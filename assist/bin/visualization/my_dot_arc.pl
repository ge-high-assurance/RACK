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
