% Copyright (c) 2022, Galois, Inc.
%
% All Rights Reserved
%
% This material is based upon work supported by the Defense Advanced Research
% Projects Agency (DARPA) under Contract No. FA8750-20-C-0203.
%
% Any opinions, findings and conclusions or recommendations expressed in this
% material are those of the author(s) and do not necessarily reflect the views
% of the Defense Advanced Research Projects Agency (DARPA).

:- module(srs_checks,
          [
              check_SRS/1
          ]).

:- ensure_loaded('../paths').
:- use_module(rack(model)).


%! check_SRS_insertion_source is det.
%
%    Checks that no SRS_Req is inserted by any activity other than
%    "SRS Data Ingestion".  Always succeeds, emits warnings.
%
% Similar to "nodegroups/query/query dataVer SRS_Req dataInsertedBy other than SRS Data Ingestion.json"
%
check_SRS_insertion_source(I) :-
    T = 'http://arcos.AH-64D/Boeing#SRS_Req',
    rack_data_instance(T, I),
    rdf(I, 'http://arcos.rack/PROV-S#dataInsertedBy', A),
    rack_instance_ident(A, AName),
    \+ AName = 'SRS Data Ingestion',
    rack_instance_ident(I, IN),
    rdf(A, rdf:type, ATy),
    print_message(warning, invalid_srs_req_inserter(T, I, IN, ATy, A, AName)).

%! check_SRS_Req_CSID_or_PIDS is det.
%
%    Checks every SRS_Req satisfies only a CSID or PIDS.
%    Always succeeds, emits warnings.
%
% Similar to "nodegroups/query/query dataVer SRS_Req without CSID or PIDS.json"
%
check_SRS_Req_CSID_or_PIDS(I) :-
    T = 'http://arcos.AH-64D/Boeing#SRS_Req',
    rack_data_instance(T, I),
    rdf(I, 'http://arcos.rack/REQUIREMENTS#satisfies', R),
    \+ is_CSID_or_PIDS(R),
    rack_instance_ident(I, Ident),
    rack_instance_ident(R, RIdent),
    rdf(R, rdf:type, RTy),
    print_message(warning, invalid_srs_req_satisfies(T, I, Ident, RTy, R, RIdent)).

is_CSID_or_PIDS(Inst) :- rdf(Inst, rdf:type, 'http://arcos.AH-64D/Boeing#PIDS_Req').
is_CSID_or_PIDS(Inst) :- rdf(Inst, rdf:type, 'http://arcos.AH-64D/Boeing#CSID_Req').


%! check_SRS_Req_description is det.
%
%    Checks every SRS_Req has a PROV-S description
%    Always succeeds, emits warnings.
%
% Similar to "nodegroups/query/query dataVer SRS_Req without description.json"
%
check_SRS_Req_description(I) :-
    check_has_no_rel('http://arcos.AH-64D/Boeing#SRS_Req',
                     'http://arcos.rack/PROV-S#description',
                     I).


%! check_SubDD_Req_satisifies_SRS_Req is det.
%
%    Checks every SubDD_Req satisifes an SRS_Req.
%    Always succeeds, emits warnings.
%
% Similar to "nodegroups/query/query dataVer SubDD_Req without satisfies SRS_Req.json"
%
check_SubDD_Req_satisfies_SRS_Req(I) :-
    check_has_no_rel('http://arcos.AH-64D/Boeing#SubDD_Req',
                     'http://arcos.rack/TESTING#satisifes',
                     'http://arcos.AH-64D/Boeing#SRS_Req',
                     I).


prolog:message(invalid_srs_req_inserter(ITy, Inst, InstIdent, InsTy, InsI, InsN)) -->
    { prefix_shorten(ITy, SIT),
      prefix_shorten(Inst, SII),
      prefix_shorten(InsTy, STT),
      prefix_shorten(InsI, STI)
    },
    [ '~w instance ~w (~w) inserted by invalid ACTIVITY: ~w ~w (~w)'-[
          SIT, SII, InstIdent, STT, STI, InsN ] ].
prolog:message(invalid_srs_req_satisfies(ITy, Inst, InstIdent, TgtTy, Tgt, TgtIdent)) -->
    { prefix_shorten(Inst, SI),
      prefix_shorten(ITy, ST),
      prefix_shorten(Tgt, SR),
      prefix_shorten(TgtTy, SRT)
    },
    [ '~w instance ~w (~w) satisifes something not a PIDS_Req or CSID_Req: ~w ~w (~w)'-[
          ST, SI, InstIdent, SRT, SR, TgtIdent ] ].


%! check_SRS is det.
%
%    Performs all checks for SRS classes.  Always succeeds, emits warnings.
check_SRS(SRS) :-
    check_SRS_insertion_source(SRS);
    check_SRS_Req_CSID_or_PIDS(SRS);
    check_SRS_Req_description(SRS);
    check_SubDD_Req_satisfies_SRS_Req(SRS).
