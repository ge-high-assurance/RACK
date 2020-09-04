% Converts the subset of SADL used for the ARCOS RACK project into
% Owl.  The top-level function is sadl_file_to_owl/2, which reads the
% SADL file and writes an OWL file.
%
% There is no support for SADL 'Ask', 'Rule', or 'Test' directives, or
% expressions not currently in the RACK SADL files.  SADL parsing is
% based on the original document (see below) and the currently defined
% files; updates/extensions may be needed here for previously unseen
% syntax.
%
% For reporting internals during a run, enable 'parsing' or
% 'importing' debug flags:
%
%     :- debug(parsing).
%     :- debug(importing).
%
% or
%
%     swipl -g 'debug(parsing)' ... rack_sadl.pl
%
% This script will attempt to display useful error messages for
% showing what syntax it has encountered that it doesn't support
% (recommended to use with the parsing debug), but it assumes
% well-formed SADL and does *not* provide SADL "compilation errors"
% that help the user identify SADL input errors.  The expected
% operation is that the user uses the GE-developed SADL Eclipse plugin
% to develop valid SADL files and that this tool is used for
% subsequent automation, such as CI tasks.

:- use_module(library(semweb/rdf11)).
:- use_module(library(semweb/rdfs)).

sadl_file_to_owl(SADLFile, OwlFile) :-
    import_sadl_from_file(SADLFile, SADL, URI, Aliases),
    length(SADL, SLen),
    print_message(informational, sadl_imported(SADLFile, SLen, URI, Aliases)),
    write_owl(OwlFile, URI, Aliases).

import_sadl_from_file(File, SADL, BaseURI, Aliases) :-
    read_file_to_string(File, Contents, []),
    import_sadl_from_string(File, Contents, SADL, BaseURI, Aliases).

import_sadl_from_string(FName, SADLStr, SADL, BaseURI, Aliases) :-
    sadl_statement(SADLStr, SADL),
    (sadl_to_rdf(FName, SADL, BaseURI, Aliases), !;
     print_message(error, sadl_to_rdf_failed), fail).

sadl_statement(Stmt, Parsed) :-
    string_chars(Stmt, Chars),
    phrase(lexical_analysis(Tokens), Chars),
    debug(parsing, 'Tokens: ~w', [Tokens]),
    parse_sadl(Tokens, Parsed).

parse_sadl(Tokens, Parsed) :-
    phrase(s(Parsed), Tokens), !,
    debug(parsing, 'Parsed: ~w', [Parsed]).
parse_sadl(Tokens, Parsed) :-
    phrase(s(Parsed), Tokens, Remainder),
    !, % failed the parse, don't try other (failure) combinations
    print_message(error, no_parse_for_sadl(Remainder)), fail.

sadl_to_rdf(FName, SADL, URI, Aliases) :-
    debug(parsing, 'Typechecking', []),
    phrase(sadl_types(URI,Kinds), [file(FName)|SADL], []),
    debug(parsing, 'Kinds: ~w', [Kinds]),
    debug(parsing, 'Emitting RDF', []),
    % n.b. sadl_rdf has side effects of asserting RDF triples
    phrase(sadl_rdf(URI, Aliases, Kinds), [file(FName)|SADL], Remainder),
    (length(Remainder, 0), !, true;
     !, % failed the parse, don't try other (failure) combinations
     print_message(error, no_rdf_for_sadl(Remainder)), fail).

% Writes the OWL definitions to the specified file, with the provided
% BaseURI and additional namespace declarations.  Note that only the
% definitions from this SADL file are emitted (specified by
% 'graph(G)') to avoid emitting any imported externals.
write_owl(File, BaseURI, [none]) :-
    rdf_default_graph(G), !,
    rdf_save(File, [graph(G), base_uri(BaseURI),
                    sorted(true),
                    % xml_attributes(false),
                    document_language(en)
                   ]).
write_owl(File, BaseURI, NS) :-
    rdf_default_graph(G),
    valid_nslist(NS),
    rdf_save(File, [graph(G), base_uri(BaseURI),
                    sorted(true),
                    % xml_attributes(false),
                    document_language(en),
                    namespaces([owl,xsd|NS])
                   ]).

valid_nslist([]).
valid_nslist([N|Ns]) :-
    (rdf_current_prefix(N,_), ! ;
     print_message(error, bad_ns_spec(N)), fail),
    valid_nslist(Ns).


% ----------------------------------------------------------------------
% User Messaging

prolog:message(sadl_imported(SADLFile, Cnt, BaseURI, none)) --> !,
    [ 'Parsed ~d SADL statements from ~w for ~w'-[Cnt, SADLFile, BaseURI] ].
prolog:message(sadl_imported(SADLFile, Cnt, BaseURI, Alias)) -->
    [ 'Parsed ~d SADL statements from ~w for ~w (~w)'-[
          Cnt, SADLFile, BaseURI, Alias] ].

prolog:message(sadl_to_rdf_failed) -->
    [ 'SADL parsed but conversion to RDF failed.' ].

prolog:message(no_parse_for_sadl(SL)) -->
    { prefix(SLS, SL), length(SLS, 8),
      length(SL, SLLen), RLen is SLLen - 8 },
    [ 'No parse for SADL statement "~w ..."  [~d following statements].'-[
          SLS, RLen] ].
prolog:message(no_parse_for_sadl(SL)) -->
    { length(SL, SLLen) },
    [ 'No parse for SADL statement "~w"  [~d following statements].'-[
          SL, SLLen] ].
prolog:message(no_rdf_for_sadl([S|Ss])) -->
    { length(Ss, SsLen) },
    [ 'No RDF translation for SADL IR element "~w" (see: sr(..)) [~d following statements].'-[
          S,SsLen] ].

prolog:message(bad_ns_spec(N)) -->
    [ 'Invalid namespace specification: ~w'-[N] ].

prolog:message(missing_property_handling(Subject, Props)) -->
    [ 'Unrecognized property specification (see: pr(Kinds,U,Subj)) for ~w: ~w'-[
          Subject, Props] ].

prolog:message(no_is_a_for(Subject, Rel, Object)) -->
    [ 'No is_a(..) definitoin for a ~w with ~w relation to ~w'-[
          Subject, Rel, Object] ].

prolog:message(unrecognized_term(What, Phase)) -->
    [ 'Unrecognized term during the ~w phase: ~w'-[Phase, What] ].

prolog:message(import_not_found(URL)) -->
    [ 'No SADL import file supplying URI ~w could be found.'-[URL] ].

% ----------------------------------------------------------------------
% Tokenize a character stream read from a file.

lexical_analysis([T|Ts]) --> blnks, token(T), lstream(Ts).

lstream([T|Ts]) --> blnks, token(T), !, lstream(Ts).
lstream(['.']) --> blnks, ['.'].  % Last char in file is the final .
lstream([]) --> blnks, [].  % Blanks at the end of the file
lstream([]) --> [].

comment(S) --> ['/', '/'], c2eol(CS), { atom_chars(S, CS) }.

token(T)   --> ['"'], qc(S), ['"'], { atom_chars(T,S) }.
token('(') --> ['('].
token(')') --> [')'].
token('{') --> ['{'].
token('}') --> ['}'].
token(',') --> [','].
% A period (.) is allowed to be embedded in an un-quoted word as a
% normal character.  It only has significance if it is the end of a
% sentence, which seems to imply it must be followed by some
% whitespace.
token('.') --> ['.',N], { char_type(N, space) ; char_type(N, white) }.
token(T)   --> ['^'], w(W), { atom_chars(T,W) }.  % see Note "carat" below.
token(T)   --> w(W), { atom_chars(T,W) }.

word_char(C) :- \+ char_type(C, space),
                \+ member(C, ['(', ')', '.', ',', '{', '}', '^']).

w([C|Cs]) --> [C], { word_char(C) }, wc(Cs).
wc([C|Cs]) --> [C], { word_char(C) }, !, wc(Cs).
wc(['.',C|Cs]) --> ['.',C], { word_char(C) }, !, wc(Cs).
wc([]) --> [].

qc([]) --> [].
qc([C|Cs]) --> [C], { C \= '"' }, qc(Cs).

c2eol([C|Cs]) --> [C], { \+ char_type(C, end_of_line) }, c2eol(Cs).
c2eol([]) --> [].

blnks --> [C], {char_type(C, space)}, !, blnks.
blnks --> comment(_), !, blnks.
blnks --> [].

% Note "carat": Sometimes a name will be prefixed by a carat ("^") in
% a SADL file.  It's unclear what this is (someone mentioned something
% about "disambiguating a private reference"), but it's not legal in
% the output XML Owl files, and indeed the Eclipse SADL allows the
% carat but strips it before writing Owl output.  The tokenization
% process here strips the leading carat as well but makes no other
% syntactic differentiation based on its presence; this may need to be
% reworked if other significance to the carat is identified.

% ----------------------------------------------------------------------
% Parse tokenized SADL into a SADL IR.

% The first statement must be the URI statement, followed by the
% remaining statements.

s([U|S]) --> uri_spec(U), s_(S).
s([])    --> [].                         % Failure for URI start line.

s_([T|Ss]) --> term(T), ['.'], s_(Ss).
s_([])     --> [].                       % Failure for term matching

uri_spec(set_uri(URI, Alias, A)) --> [uri, URI, alias, Alias],
                                     annots(A), ['.'].
uri_spec(set_uri(URI, A)) --> [uri, URI], annots(A), ['.'].


%% The srN or %[N] references below refer to the "Toward a Unified
%% English-like Representation of Semantic Models, Data, and Graph
%% Patterns for Subject Matter Experts, by A.W. Crapo and Abha Moitra,
%% GE Global Research.  Those prefixed with "new-" are not represented
%% in that document.  These references are ignored and used primarily
%% for debugging.
%%
%% The prop first argument is similarly ignored but used to
%% distinguish the applied rule for debugging.
%%
%% Additional notes:
%%
%% ^1 - English would capitalize the beginning-of-sentence determiner,
%%      but [SADL] allows lower-case.
%%
%% ^2 - Eclipse SADL implementation does not allow annotations on
%%      these names, but this emitter does.


term(import(URL)) --> [import, URL].
term(propinv(I,P)) --> subj(subj(I,[])), [is], dl,
                       [inverse, of], obj(P).                         %[new-3]
term(valenum(I,VS)) --> subj(I), [is], obj(class),
                        [',', must, be, one, of], valenum(VS).    %[new-2, ^2]
term(classdef(sr1_2,I,C,PTS)) --> subj(I), [is], dl, [type,of], obj(C), %[1,2]
                                  propterms(simple,PTS).
term(classdef(sr1_2,I,class,PTS)) --> subj(I), [is], obj(class),        %[1,2]
                                  propterms(simple,PTS).
term(instance(sr1_2,I,C,PTS)) --> subj(I), [is], obj(C),                %[1,2]
                                  propterms(simple,PTS).
term(propdef(S,P,V)) --> property(P), [describes], obj(S),
                         % this rule must come before sr3 to
                         % disambiguate when there is no note
                         % annotations on P.
                         [with], valphrs(simple,V).                   %[new-1]

term(propval(I,P,V)) -->
    det(cap,_), property(P), [of], obj(I), [is], valterm(simple,V).       %[4]
term(propval(I,P,V)) -->
    property(P), [of], obj(I), [is], valterm(simple,V).               %[new-4]
term(instance(sr5,I,[prop(p2,P,V)])) --> valterm(simple,V), [is],
                                         det(lc,_), [P, of, I].           %[5]
term(instance(sr3,I,PTS)) --> subj(I), propterms(simple,PTS).             %[3]
% term(unnamedI(sr6_7,C,PTS)) --> det(_,indef), [C], propterms(_,PTS).    %[6,7]

% The following are listed in the [SADL] document, but it's expected
% that they are fully handled by the above.
term(triplerep(sr8,S,P,V)) --> [S], owns, [P], valterm(simple,V).         %[8]
term(triplerep(sr9,S,P,V)) --> [P, of, S, is], valterm(simple,V).         %[9]
term(triplerep(sr10,S,P,V)) --> valterm(simple,V), [is, P, of, S].       %[10]


propterms(T,[P|PTs]) --> pterm(T,P), propterms(T,PTs).
propterms(_,[]) --> [].

pterm(T,prop(p3,P,V)) --> [','], owns, property(P), [with], valphrs(T,V).
pterm(T,prop(p4,P,V)) --> owns, property(P), [with], valphrs(T,V).
pterm(T,prop(p5,P,V)) --> [','], owns, property(P), valterm(T,V).
pterm(T,prop(p6,P,V)) --> owns, property(P), valterm(T,V).

property(propId(P,Anns)) --> word(P), annots(Anns).


subj(subj(I,Anns)) --> det(_,_), [I], annots(Anns).     %^1
subj(subj(I,Anns)) --> [I], annots(Anns).


obj(O) --> det(lc,_), [O].
obj(O) --> [O].


valphrs(PC, val(none,V)) --> [values, of, type], valterm_(PC,V).
valphrs(PC, val(num(1),V)) --> det(lc,_), [single, value, of, type],
                               valterm_(PC,V).

valterm(M,val(none,V)) --> valterm_(M,V).

valterm_(bnode,bnode(C,PTS)) --> ['(', a, C], propterms(simple,PTS), [')']. %[7]
valterm_(simple,V) --> [V].
% n.b. for %7 it's unspec if property terms can be recursive bnodes.
% Current choice: no.

valenum(VS) --> ['{'], valenum_(VS), ['}'].
valenum_([V|Vs]) --> [V, ','], valenum_(Vs).
valenum_([V]) --> [V].


owns --> [has].
owns --> [with].
owns --> [described, by].
owns --> [].


dl --> det(lc,_).
det(cap,def) --> ['The'].
det(lc,def)  --> [the].
det(cap,indef) --> ['A'].
det(cap,indef) --> ['An'].
det(lc,indef)  --> [a].
det(lc,indef)  --> [an].


% KWQ: include the curly braces?
word(P) --> [P], { \+ member(P, ['(', ')', '.', ',']) }. % , '{', '}']) }.


annots(AS) --> ['('], annots_(AS), [')'].
annots([]) --> [].
annots_([A|As]) --> annot(A), [','], annots_(As).
annots_([A]) --> annot(A).
annots_([]) --> [].
annot(note(NoteStr)) --> [note, A], { atom_string(A, NoteStr) }.


% ----------------------------------------------------------------------
% TYPING PHASE: Process parsed SADL IR to collect type information,
% including performing full imports.

:- op(500,xfy,::).


sadl_types(U,Kinds) --> [file(F), set_uri(U, _)],
                        { set_graph_for_file(F, _G) },
                        sadlty(U, [F::U], _FileURLs, RKinds),
                        { resolve(RKinds, RKinds, Kinds) }.
sadl_types(U,Kinds) --> [file(F), set_uri(U, _, _)],
                        { set_graph_for_file(F, _G) },
                        sadlty(U, [F::U], _FileURLs, RKinds),
                        { resolve(RKinds, RKinds, Kinds) }.


resolve(_, [], []).
resolve(All, [N::clsOrInst|Ks], Rs) :-
    (member(N::class, All)), !,
    resolve(All, Ks, Rs).
resolve(All, [N::clsOrInst|Ks], Rs) :-
    member(N::instance, All), !,
    resolve(All, Ks, Rs).
resolve(All, [K|Ks], [K|Rs]) :- resolve(All, Ks, Rs).


set_graph_for_file(File, Graph) :-
    file_base_name(File, Base),
    file_name_extension(Graph,_,Base),
    % There is a slight conflation between "graph", "file", and
    % the root element after the URI here.  This distinction could
    % be made in the future if needed.
    rdf_create_graph(Graph),
    % rdf_set_graph(Graph, source(U)), Removed! Not supported by
    % Prolog/RDF: source is apparently read-only, and set by rdf_load.
    rdf_default_graph(_, Graph).

sadlty(U,InFURLs,OutFURLs,Kinds) -->
    sty(U, InFURLs, FileURLs, StmtKind),
    sadlty(U, FileURLs, OutFURLs, RemKinds),
    {
        append(StmtKind, RemKinds, LKinds),
        length(LKinds, _N),  % no lazy lists?!
        sort(LKinds, Kinds)
    }.
sadlty(_,F,F,[])   --> [].


sty(U,IFs,OFs,[]) --> [ import(URL) ],
                      {
                          debug(importing, 'importing ~w', [URL]),
                          % Need to import the .sadl file associated with this
                          % import so that the definitions there can be used to
                          % resolve "external" parent classes and properties in
                          % the main file.
                          rdf_default_graph(OrigG),
                          rdf_assert(U, owl:imports, URL, OrigG),
                          do_import_of(U, URL, IFs, OFs),
                          debug(importing, 'import of ~w completed', [URL]),
                          rdf_default_graph(_, OrigG)
                      }.
sty(_,F,F, [N::clsOrInst|PKs]) --> [ instance(_ID, subj(N,_), Props) ],
                                   { phrase(proptys(propval, PKs), Props) }.
sty(_,F,F, [N::class|PKs]) --> [ classdef(_ID, subj(N,_), _, Props) ],
                               { phrase(proptys(propdef, PKs), Props) }.
sty(_,F,F, [N::instance|PKs]) --> [ instance(_ID, subj(N,_), _Inst, Props) ],
                                  { phrase(proptys(propval, PKs), Props) }.
sty(_,F,F, [S::clsOrInst,P::property]) --> [ propdef(S,propId(P,_),_) ].
sty(_,F,F, [S::instance,P::propval]) --> [ propval(S,propId(P,_),_) ].
sty(_,F,F, []) --> [ propinv(_,_) ].
sty(_,F,F, [I::class]) --> [ valenum(subj(I,_),_) ].
sty(_,_,_,_) --> [ What ],
                 { print_message(error, unrecognized_term(What, typing)),
                   fail
                 }.

proptys(PT, [P::PT|PKinds]) --> [prop(_ID, propId(P,_), _)], proptys(PT, PKinds).
proptys(_, []) --> [].

% ----------------------------------------------------------------------
% Import management

% :- op(500,xfy,::).

do_import_of(U,URL,IFU,OFU) :- new_do_import_of(U,URL,IFU,OFU).

old_do_import_of(U, URL, FURLs, FURLs) :-
    % The SADL import specifies the import by URL, not file,
    % so assume that if the BaseURL of the import is the
    % same as the current URL, that the portion after the
    % URL specifies the SADL file to import (with a .sadl
    % extension).
    rdf_default_graph(OrigG),
    atom_concat(BaseURL, OrigG, U),
    atom_concat(BaseURL, ImportName, URL),
    atom_concat(ImportName, '.sadl', ImportFile),
    import_sadl_from_file(ImportFile, _ImportSADL, URL, _Alias).

new_do_import_of(_U, URL, InFURLs, InFURLs) :-
    % The SADL import specifies the import by URL, not file, so import
    % all SADL files in the vicinity to find one that declares that it
    % provides the uri being imported.  Can move upward in the
    % directory tree, stopping at (and not importing from) a directory
    % containing an 'ImplicitModel' directory (Eclipse SADL creates
    % this), and descending into all subdirectories thereof.
    %
    % The InFURLs is used to keep track of what mappings have already
    % been located, and the OutFURLs is updated with any new mappings.
    % These are convenience to help minimize duplicated work in the
    % presence of multiple imports.
    member(F::URL, InFURLs), !,
    import_sadl_from_file(F, _SADL, URL, _Aliases).

new_do_import_of(U, URL, InFURLs, OutFURLs) :-
    % Handles the case where the FURL is not already known
    member(F::U, InFURLs),  % get main file
    find_file_for_url(F, URL, InFURLs, OutFURLs),
    (member(InpF::URL, OutFURLs), !,
     import_sadl_from_file(InpF, _SADL, URL, _Aliases);
     print_message(error, import_not_found(URL))).

find_file_for_url(StartFile, TargetURL, InFURLs, OutFURLs) :-
    file_directory_name(StartFile, FDir),
    absolute_file_name(FDir, Dir),
    find_url_in_dir_or_sub(TargetURL, InFURLs, Dir, HereFURLs),
    (member(_::TargetURL, HereFURLs), !, OutFURLs = HereFURLs;
     find_url_in_parent(TargetURL, Dir, HereFURLs, OutFURLs)).

find_url_in_dir_or_sub(TargetURL, InFURLs, Dir, HereFURLs) :-
    directory_files(Dir, Entries),
    add_furls(TargetURL, Dir, Entries, InFURLs, DirFURLs),
    (member(_::TargetURL, DirFURLs), !, HereFURLs = DirFURLs;
     add_subdir_furls(TargetURL, DirFURLs, Dir, Entries, HereFURLs)).

add_furls(TargetURL, Dir, Entries, InFURLs, DirFURLs) :-
    phrase(get_furl(TargetURL, Dir, InFURLs, DirFURLs), Entries).

get_furl(TargetURL, _Dir, InFURLs, InFURLs) -->
    [_File],
    {
        member(_::TargetURL, InFURLs), !
        % already found the needed import, do nothing
    },
    get_furl(TargetURL, _, _, _).
get_furl(TargetURL, Dir, InFURLs, OutFURLs) -->
    [File],
    {
        \+ file_name_extension(_, ".sadl", File), !
        % Not a sadl file, skip it
    },
    get_furl(TargetURL, Dir, InFURLs, OutFURLs).
get_furl(TargetURL, Dir, InFURLs, FURLs) -->
    [SADLFile],
    {
        atom_concat(Dir, '/', DS),
        atom_concat(DS, SADLFile, FPath),
        member(FPath::_, InFURLs), !
        % already processed this file, don't repeat it
    },
    get_furl(TargetURL, Dir, InFURLs, FURLs).
get_furl(TargetURL, Dir, InFURLs, FURLs) -->
    [SADLFile],
    {
        atom_concat(Dir, '/', DS),
        atom_concat(DS, SADLFile, FPath),
        read_file_to_string(FPath, Contents, []),
        sadl_statement(Contents, SADL),
        (member(set_uri(U,_), SADL) ; member(set_uri(U,_,_), SADL)), !,
        (
         %%    U = TargetURL,
         %% write('s2rdf '),write(FPath),write(' @ '),write(U),nl,
         %% sadl_to_rdf(FPath, SADL, U, _Aliases);
         true)
    }, !,
    get_furl(TargetURL, Dir, [FPath::U|InFURLs], FURLs).
get_furl(TargetURL, Dir, InF, OutF) --> [_File],  % not useful, skip it
                                        get_furl(TargetURL, Dir, InF, OutF).
get_furl(_,_,F,F) --> [].

add_subdir_furls(TargetURL, DirFURLs, Dir, Entries, SubFURLs) :-
    phrase(get_subfurl(TargetURL, DirFURLs, Dir, SubFURLs), Entries).

get_subfurl(U, InFURLs, Dir, FURLs) -->
    [SubDir],
    {
        \+ member(SubDir, [ '.', './', '..', '../' ]),
        atom_concat(Dir, '/', DS), atom_concat(DS, SubDir, SubPath),
        exists_directory(SubPath),
        find_url_in_dir_or_sub(U, InFURLs, SubPath, SubDirFURLs)
    }, !,
    get_subfurl(U, SubDirFURLs, Dir, FURLs).
get_subfurl(U, InFURLs, Dir, FURLs) --> [_NotSubdir],
                                        get_subfurl(U, InFURLs, Dir, FURLs).
get_subfurl(_,F,_,F) --> [].

find_url_in_parent(TargetURL, Dir, HereFURLs, OutFURLs) :-
    absolute_file_name(Dir, FD),
    file_directory_name(FD, PD),
    fuip(TargetURL, PD, HereFURLs, OutFURLs).
fuip(_,'/',H,H) :- !.
fuip(_,'/home',H,H) :- !.
fuip(_,'/Users',H,H) :- !.
fuip(_,D,H,H) :-
    directory_files(D, Es),
    member('ImplicitModel', Es), !.
fuip(U,D,H,O) :-
    find_url_in_dir_or_sub(U, H, D, HereFURLs),
    % n.b. above re-processes subdir tree we just came from; could add
    % a "processed dirs" list to avoid this if it becomes problematic.
    (member(_::U, HereFURLs), !, O = HereFURLs;
     find_url_in_parent(U, D, HereFURLs, O)).


% ----------------------------------------------------------------------
% EMITTING PHASE: Process parsed SADL IR to generate RDF triples and
% assoc. definitions

sadl_rdf(U,Pfxs,Kinds) --> [file(F), set_uri(U, Annots)],
                           { setup_rdf(F,U,none,Annots) },
                           sar(U, Kinds, Pfxs).
sadl_rdf(U,[Alias|Pfxs],Kinds) --> [file(F), set_uri(U, Alias, Annots)],
                                   { setup_rdf(F,U,Alias,Annots) },
                                   sar(U, Kinds, Pfxs).

setup_rdf(File,BaseURI,Alias,Annots) :-
    set_graph_for_file(File, G),
    (Alias = none, !, true;
     rdf_register_prefix(Alias, BaseURI, [force(true)])),
    string_concat("Do not edit: generated by sadl.pl from file: ",
                  File, Comment),
    rdf_assert(BaseURI, rdf:type, owl:'Ontology'),
    rdf_assert(BaseURI, owl:imports, 'http://sadl.org/builtinfunctions'),
    rdf_assert(BaseURI, owl:imports, 'http://sadl.org/sadlimplicitmodel'),
    rdf_assert(BaseURI, owl:imports, 'http://sadl.org/sadlbasemodel'),
    rdf_assert(BaseURI, rdfs:comment, Comment@en, G),
    phrase(nt(BaseURI), Annots).


sar(U,Kinds,Pfxs) --> sr(U, Kinds, ThisPfxs), sar(U, Kinds, OtherPfxs),
                      {
                          append(ThisPfxs, OtherPfxs, AllPfxs),
                          sort(AllPfxs, Pfxs)
                      }.
sar(_,_,[]) --> [].  % nothing matched: halts processing, "remainder" shown as error.

sr(_,_,[]) --> [ import(_) ].  % handled in typing pass
sr(U,Kinds,Pfxs) --> [ instance(_ID, Subj, Props) ],
                     { inst_props(Kinds, U, Subj, Props, _, _, Pfxs) }.
sr(U,Kinds,Pfxs) --> [ classdef(_ID, Subj, ParentClass, Props) ],
                     {
                         class_props(Kinds, U, Subj, Props, SubjRef, G, P_Pfxs),
                         is_a(Kinds, U, SubjRef, classdef, ParentClass, G, I_Pfxs),
                         append(I_Pfxs, P_Pfxs, Pfxs)
                         % , write('classdef '),write(Subj),write(' props is_a '),write(ParentClass),nl
                     }.
sr(U,Kinds,Pfxs) --> [ instance(_ID, Subj, ClassDef, Props) ],
                     {
                         inst_props(Kinds, U, Subj, Props, SubjRef, G, P_Pfxs),
                         is_a(Kinds, U, SubjRef, instance, ClassDef, G, I_Pfxs),
                         append(I_Pfxs, P_Pfxs, Pfxs)
                         % , write('inst1 '),write(Subj),write(' is_a '),write(ClassDef),nl
                     }.
sr(U,Kinds,Pfxs) --> [ propdef(S,P,V) ], { def_prop(Kinds,U,S,P,V,Pfxs) }.
sr(U,Kinds,Pfxs) --> [ propval(S,P,V) ], { def_propval(Kinds,U,S,P,V,Pfxs) }.
sr(U,_,[]) --> [ propinv(I,P) ],
               {
                   rdf_default_graph(G),
                   ns_ref(U,I,Inv),
                   ns_ref(U,P,Prop),
                   rdf_assert(Inv, owl:inverseOf, Prop, G)
               }.
sr(U,Kinds,Pfxs) --> [ valenum(subj(I,Annots),VS) ],
                     {
                         subj_ref(U, I, G, Subject),
                         is_a(Kinds, U, Subject, classdef, class, G, Pfxs),
                         phrase(nt(Subject), Annots),
                         all_are_type(U, G, Subject, VS, VSRefs),
                         rdf_create_bnode(ListOneOf),
                         rdf_assert(ListOneOf, rdf:type, owl:'Class', G),
                         rdf_assert_list(VSRefs, VSList, G),
                         rdf_assert(ListOneOf, owl:oneOf, VSList, G),
                         rdf_assert(Subject, owl:equivalentClass, ListOneOf, G)
                     }.
sr(_,_,_) --> [What],
              { print_message(error, unrecognized_term(What, emitting)),
                fail
              }.

% sr(_,_,L,L) :- write('No match for: '),write(L),nl,fail.

class_props(Kinds, U, subj(N, SubjAnnots), Props, Subject, G, Pfxs) :-
    subj_ref(U, N, G, Subject),
    phrase(nt(Subject), SubjAnnots),
    ((phrase(pr(Kinds, U, N, classdef, Pfxs), Props), !) ;
     (print_message(error, missing_property_handling(Subject, Props)),
      fail)).

inst_props(Kinds, U, subj(N, SubjAnnots), Props, Subject, G, Pfxs) :-
    subj_ref(U, N, G, Subject),
    phrase(nt(Subject), SubjAnnots),
    ((phrase(pr(Kinds, U, N, instance, Pfxs), Props), !) ;
     (print_message(error, missing_property_handling(Subject, Props)),
      fail)).

all_are_type(_, _, _, [], []).
all_are_type(URL, G, TType, [T|Ts], [TRef|TRefs]) :-
    ns_ref(URL, T, TRef),
    rdf_assert(TRef, rdf:type, TType, G),
    all_are_type(URL, G, TType, Ts, TRefs).


% Define the existence of a thing.  The same statement
% (e.g. instance(sr1_2, ...)) could be made for a sub-class or a
% sub-property.
is_a(_Kinds, _URL, Subject, classdef, class, G, []) :-
    rdf_assert(Subject, rdf:type, owl:'Class', G), !.
is_a(Kinds, URL, Subject, classdef, C, G, []) :-
    obj_ref(Kinds, URL, G, C, class, CRef),
    rdf_assert(Subject, rdf:type, owl:'Class', G),
    rdf_assert(Subject, rdfs:subClassOf, CRef, G), !.
is_a(Kinds, URL, Subject, classdef, C, G, []) :-
    obj_ref(Kinds, URL, G, C, property, CRef),
    rdf_assert(Subject, rdf:type, owl:'ObjectProperty', G),
    rdf_assert(Subject, rdfs:subPropertyOf, CRef, G), !.
is_a(Kinds, URL, Subject, classdef, C, G, []) :-
    obj_ref(Kinds, URL, G, C, literal, Lit),
    rdf_assert(Subject, rdf:type, Lit, G), !.
is_a(_Kinds, _URL, Subject, instance, C, G, [Pfx]) :-
    % write('is_a '),write(URL), write(' '), write(Subject),write(' instance of '),write(C),nl,
    atom_concat('v.', G, Pfx),
    atom_concat(Pfx, ':', VGC),
    atom_concat(VGC, C, PfxRef),
    atom_concat(G, '#', PfxOf),
    rdf_register_prefix(Pfx, PfxOf, [force(true)]),
    rdf_assert(Subject, rdf:type, PfxRef, G).
    %% ns_ref(URL, C, CRef),
    %% rdf_assert(Subject, rdf:type, CRef, G).
is_a(_Kinds, _URL, Subject, What, C, _G) :-
    print_message(error, no_is_a_for(Subject, What, C)), fail.


obj_ref(_Kinds, _URL, _G, string, literal, String) :-
    rdf_equal(xsd:string, String), !.
obj_ref(_Kinds, _URL, _G, date, literal, Date) :-
    rdf_equal(xsd:date, Date), !.
obj_ref(_Kinds, _URL, _G, dateTime, literal, DateTime) :-
    rdf_equal(xsd:dateTime, DateTime), !.
obj_ref(_Kinds, _URL, _G, float, literal, Float) :-
    rdf_equal(xsd:float, Float), !.
obj_ref(Kinds, URL, _G, C, Kind, Reference) :-
    ns_ref(URL, C, Reference),
    member(C::Kind, Kinds), !.
obj_ref(_Kinds, URL, G, C, Kind, Reference) :-
    % The ns_ref didn't generate a valid local reference, so this is
    % probably defined in a separate file and was realized by an
    % import statement.
    extern_ref(URL, G, C, Kind, Reference).
obj_ref(_Kinds, _URL, _G, V, value, V) :- !.

% Note "External References": In SADL, one can say:
%
%     import "http://my.url/ANIMALS"
%
%     FOO is a type of class described by fools with values of class.
%     BAR is a type of FOO described by bar with values of class.
%     bar is a type of fools.
%
%     COW is a type of ANIMAL described by sound with values of class.
%     sounds is a type of noise.
%
% Here, the first stanza of clauses is all locally satisfied, but the
% second stanza references ANIMAL and noise, which are defined in the
% ANIMALS import.  There is no marker to distinguish local definitions
% from imported definitions: imports instantiate into the "local"
% namespace.  The is_a/5 rules above will detect that the requested
% target type is not present in the namespace currently being defined,
% and divert to here.  The extern_ref/5 code here is responsible for
% looking through the definitions in other imported spaces to find a
% likely match.  Note that there is no guarantee which import is
% selected if imports have matching names (does Eclipse SADL have a
% heuristic for this?).
extern_ref(URL, G, C, class, ExternClass) :-
    % This version handles the case where C just the name, like 'OBJECT'
    atom_concat(BaseURL, G, URL),  % get BaseURL
    atom_concat('#', C, Fragment), % get target class fragment
    rdf(ERef, rdf:type, owl:'Class'),  % find a defined class
    atom_concat(_From, Fragment, ERef),
    (atom_concat(BaseURL, ExternClass, ERef),
     atom_concat(_Here, Fragment, ExternClass);
     ExternClass=ERef).
extern_ref(URL, _G, C, class, ExternClass) :-
    % This version handles the case where C is like 'FILE#OBJECT'
    rdf(ExternClass, rdf:type, owl:'Class'),
    atom_concat(BaseURL, C, ExternClass),
    BaseURL \= URL.
extern_ref(URL, G, P, property, ExternProp) :-
    % This version handles the case where C is just the name, like 'PROP_A'
    atom_concat(BaseURL, G, URL),  % get BaseURL
    atom_concat('#', P, Fragment), % get target class fragment
    (rdf(ERef, rdf:type, owl:'ObjectProperty');
     rdf(ERef, rdf:type, owl:'DatatypeProperty')),  % find a defined Property
    atom_concat(_From, Fragment, ERef),
    (atom_concat(BaseURL, ExternProp, ERef), % property without BaseURL
     atom_concat(_Here, Fragment, ExternProp);  % right local URL target?
    ExternProp=ERef).  % remote URL target
extern_ref(URL, _G, P, property, ExternProp) :-
    % This version handles the case where P is like 'FILE#PROP_A'
    (rdf(ExternProp, rdf:type, owl:'ObjectProperty');
     rdf(ExternProp, rdf:type, owl:'DatatypeProperty')),
    atom_concat(BaseURL, P, ExternProp),
    BaseURL \= URL.


subj_ref(BaseURI, SubjName, G, SubjRef) :-
    rdf_default_graph(G),
    ns_ref(BaseURI, SubjName, SubjRef).

ns_ref(BaseURI, Obj, Ref) :-
    atom_concat(BaseURI, '#', UG), atom_concat(UG, Obj, Ref).


nt(Subject) --> [note(N)],
                {
                    rdf_default_graph(G),
                    rdf_assert(Subject, rdfs:comment, N@en, G)
                }.
nt(_) --> [].


pr(Kinds,U,Subj,classdef, Pfxs) -->
    [prop(_ID, P, val(R,T))], !,
    { def_prop(Kinds, U, Subj, P, val(R,T), NewPfxs) },
    pr(Kinds,U,Subj,classdef,OtherPfxs),
    {
        append(NewPfxs, OtherPfxs, JoinPfxs),
        sort(JoinPfxs, Pfxs)
    }.
pr(Kinds,U,Subj,instance, Pfxs) -->
    [prop(_ID, P, val(R,T))], !,
    { def_propval(Kinds, U, Subj, P, val(R,T), NewPfxs) },
    pr(Kinds,U,Subj,instance,OtherPfxs),
    {
        append(NewPfxs, OtherPfxs, JoinPfxs),
        sort(JoinPfxs, Pfxs)
    }.
pr(_,_,_,_,[]) --> [].


def_propval(Kinds, URL, Subject, propId(Prop,_PropAnn),val(_Restr,Tgt),[Pfx]) :-
    subj_ref(URL, Subject, G, C),
    obj_ref(Kinds, URL, G, Tgt, _TgtTy, _T),
    obj_ref(Kinds, URL, G, Prop, propval, P), !,
    % Get the definition of this property, where the range will
    % determine the type of the property value Tgt to set.
    obj_ref(Kinds, URL, G, Prop, property, PDef),
    rdf(PDef, rdfs:range, PDType),
    % Values require a prefixed reference
    val_prefix_ref(URL, P, Prop, Pfx, PRef),
    % Convert the Tgt value to the type specified by the property
    typed_val(Kinds, URL, G, Tgt, PDType, TyVal),
    rdf_assert(C, PRef, TyVal).

typed_val(_Kinds, _URL, _G, V, Date, date(Y,M,D)^^Date) :-
    rdf_equal(xsd:date, Date), !,
    atom_string(V, S),
    ( split_string(S, "/", "", [MS,DS,YS]);     % 12/25/2010
      split_string(S, "-", "", [YS,MS]), DS=1), % 2010-12
    atom_string(YA,YS), atom_number(YA,Y),
    atom_string(MA,MS), atom_number(MA,M),
    atom_string(DA,DS), atom_number(DA,D).
typed_val(_Kinds, _URL, _G, V, Int, I^^Int) :-
    rdf_equal(xsd:int, Int), !, atom_number(V, I).
typed_val(_Kinds, _URL, _G, V, Integer, I^^Integer) :-
    rdf_equal(xsd:integer, Integer), !, atom_number(V, I).
typed_val(_Kinds, _URL, _G, V, Float, F^^Float) :-
    rdf_equal(xsd:float, Float), !, atom_number(V, F).
typed_val(Kinds, URL, G, V, Ty, VRef) :-
    obj_ref(Kinds, URL, G, Ty, class, _PDC), !,
    ns_ref(URL, V, VRef).
typed_val(_Kinds, _URL, _G, V, _, literal(V)).


def_prop(Kinds, URL, Subject, propId(Prop,PropAnn),val(Restr,Tgt), []) :-
    subj_ref(URL, Prop, G, P),
    obj_ref(Kinds, URL, G, Subject, class, C),
    obj_ref(Kinds, URL, G, Tgt, Kind, T),
    def_prop_type(G, P, Kind, T),
    add_prop_domain(P, C, G),
    rdf_assert(P, rdfs:range, T, G),
    def_restr(G,C,P,Restr),
    phrase(nt(P), PropAnn).

def_prop_type(G, P, class, _T) :-
    rdf_assert(P, rdf:type, owl:'ObjectProperty', G).
def_prop_type(G, P, property, _T) :-
    rdf_assert(P, rdf:type, owl:'ObjectProperty', G).
def_prop_type(G, P, literal, _T) :-
    rdf_assert(P, rdf:type, owl:'DatatypeProperty', G).

add_prop_domain(Prop, Class, G) :-
    rdf(Prop, rdfs:domain, OtherClass),
    rdf_is_bnode(OtherClass), !,
    % Already a shared domain, add this class to the union
    rdf(OtherClass, owl:unionOf, OldCList),
    rdf_list(OldCList, OldList),
    rdf_retractall(OtherClass, owl:unionOf, OldCList, G),
    rdf_retract_list(OldCList),
    rdf_assert_list([Class|OldList], NewCList, G),
    rdf_assert(OtherClass, owl:unionOf, NewCList, G).
add_prop_domain(Prop, Class, G) :-
    rdf(Prop, rdfs:domain, OtherClass), !,
    % This property already has another domain, so create a bnode
    % union of domains.
    rdf_retractall(Prop, rdfs:domain, OtherClass, G),
    rdf_create_bnode(DomUnion),
    rdf_assert(DomUnion, rdf:type, owl:'Class', G),
    rdf_assert_list([OtherClass, Class], DomList, G),
    rdf_assert(DomUnion, owl:unionOf, DomList, G),
    rdf_assert(Prop, rdfs:domain, DomUnion).
add_prop_domain(Prop, Class, G) :-
    rdf_assert(Prop, rdfs:domain, Class, G).

def_restr(_,_,_,none).
def_restr(G,C,P,num(N)) :-
    rdf_create_bnode(Res),
    rdf_assert(Res, rdf:type, owl:'Restriction', G),
    rdf_assert(Res, owl:cardinality, N^^xsd:int, G),
    rdf_assert(Res, owl:onProperty, P, G),
    rdf_assert(C, rdfs:subClassOf, Res, G).

val_prefix_ref(URL, Ref, Name, Pfx, PfxRef) :-
    atom_concat(GHash, Name, Ref),
    atom_concat(G, '#', GHash),
    (URL = G, !, rdf_default_graph(UseG) ; UseG = G),
    atom_concat('v.', UseG, Pfx),
    atom_concat(Pfx, ':', VGC),
    atom_concat(VGC, Name, PfxRef),
    ( % if a Pfx in *this* file, might not be an RDF yet
        URL = G, !, PfxOf = GHash ;
        % otherwise, find a defined reference to get the URL graph#
        rdf(URLRef, rdf:type, _),
        atom_concat(SrcURL, Ref, URLRef),
        atom_concat(SrcURL, GHash, PfxOf)),
    rdf_register_prefix(Pfx, PfxOf, [force(true)]).
