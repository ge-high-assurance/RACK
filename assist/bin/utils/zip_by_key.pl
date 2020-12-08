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

:- module(zip_by_key,
          [
           zip_by_key/5
          ]).

:- ensure_loaded('../paths').

% NOTE (val) I feel like there is a smarter way of doing this
%! zip_by_key_aux(+KeyValuesL, +DefaultL, +KeyValuesR, +DefaultR, ?Key, -ValueL, -ValueR) is det.
%
%    Enumerates all keys from the union of the keys of KeyValuesL and KeyValueR,
%    and for each key, outputs the left and right value for that key, using the
%    left or right default value if the key is absent from one key-value list.
%
%      zip_by_key_aux([K1-L1], DL, [K1-R1], DR, K1, L1, R1) :- true.
%      zip_by_key_aux([K1-L1], DL, [],      DR, K1, L1, DR) :- true.
%      zip_by_key_aux([],      DL, [K1-R1], DR, K1, DL, R1) :- true.
zip_by_key_aux(KeyValuesL, _, KeyValuesR, DefaultR, Key, ValueL, DefaultR) :-
    member(Key-ValueL, KeyValuesL), \+ member(Key-_, KeyValuesR).
zip_by_key_aux(KeyValuesL, DefaultL, KeyValuesR, _, Key, DefaultL, ValueR) :-
    \+ member(Key-_, KeyValuesL), member(Key-ValueR, KeyValuesR).
zip_by_key_aux(KeyValuesL, _, KeyValuesR, _, Key, ValueL, ValueR) :-
    member(Key-ValueL, KeyValuesL), member(Key-ValueR, KeyValuesR).

%! zip_by_key(+KeyValuesL, +DefaultL, +KeyValuesR, +DefaultR, -KeyValuesOut) is det.
%
%    Takes two keyed collections of lists:
%
%      KeyValuesL = [K1-L1, K2-L2, K3-L3]
%      KeyValuesR = [K1-R1, K4-R4]
%
%    and returns keyed triples for the union of all keys, where the second
%    component comes from KeyValuesL (or is DefaultL if the key is absent in
%    KeyValuesL), and the third component comes from KeyValuesR (or is DefaultR
%    if the key is absent in KeyValuesR), e.g.
%
%      KeyValuesOut = [ K1-L1-R1, K2-L2-DefaultR, K3-L3-DefaultR, K4-DefaultL-R4 ]
zip_by_key(KeyValuesL, DefaultL, KeyValuesR, DefaultR, KeyValuesOut) :-
    setof(
     Key-ValueL-ValueR,
     zip_by_key_aux(KeyValuesL, DefaultL, KeyValuesR, DefaultR, Key, ValueL, ValueR),
     KeyValuesOut
    ).
