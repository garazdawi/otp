%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%% =====================================================================
%%% T2-Full P0 "G1" fidelity gate: AOT beam_ssa vs. tier-2 reconstruction.
%%%
%%% For each corpus function this harness compares:
%%%
%%%   (a) the AOT SSA the compiler produces at a chosen listing point, against
%%%   (b) the tier-2 reconstruction that t2_hir_builder rebuilds from the
%%%       *post-`beam_ssa_codegen'* generic BEAM ops retained at load, fetched
%%%       with `erts_debug:get_internal_state({t2_build_ssa,M,F,A})'.
%%%
%%% The AOT reference point is selectable (compare_module/3, run/2), because
%%% *where* on the compiler pipeline we sample the AOT SSA determines how much
%%% of the divergence is legitimate lowering vs. reconstruction infidelity:
%%%
%%%   * `dssaopt' -- after beam_ssa_opt, before beam_ssa_pre_codegen.  This is
%%%     the optimiser's output, one lowering *above* codegen.  Every synthetic
%%%     failure edge is still a `match_fail' scaffold: a `match_fail' body op
%%%     immediately followed by `succeeded:body' and a branch to a dead block
%%%     that calls `erlang:error/1'.  Those dead `erlang:error' blocks (and the
%%%     un-lowered binary-match/`update_tuple' pseudo-ops) inflate the AOT op
%%%     multiset with content the loaded code -- and hence the reconstruction
%%%     -- never carries.
%%%   * `dprecg' -- after beam_ssa_pre_codegen, *immediately before* codegen
%%%     (the `{listing,"precodegen"}' point).  beam_ssa_pre_codegen has already
%%%     run `sanitize' (drops the dead `succeeded'/`erlang:error' scaffold
%%%     behind a `match_fail'), `expand_match_fail' (badmatch/case_end/if_end/
%%%     function_clause stay `match_fail'; a general `match_fail' becomes
%%%     `put_tuple' + `call erlang:error/1'; an inlined function_clause becomes
%%%     a local stub `call'), `fix_bs' (bs_match+bs_extract -> bs_get) and
%%%     register allocation.  This is the closest SSA to the loaded code the
%%%     compiler ever materialises, so it is the honest reference point for a
%%%     reconstruction that is rebuilt one lowering *below* it.
%%%
%%% Both points return the live `#b_module{}' term: `compile:forms(Abstr,
%%% [dssaopt,binary|Opts])' resp. `[dprecg,binary|Opts]' -- the `{listing,_}'
%%% pass truncates the pass pipeline and, with `binary' set, `compile' hands
%%% back the intermediate SSA as the "object code".
%%%
%%% The two representations sit on opposite sides of code generation, so an
%%% exact match is neither expected nor the goal (PLAN/T2FULL/07 §"G1 corpus
%%% and fidelity comparator": "compare structure modulo codegen expansions
%%% ... not byte-identity").  The harness therefore reports two rates and a
%%% ranked list of divergence classes:
%%%
%%%   * CFG-shape pass  -- identical block count, edge-degree multiset
%%%                        (an isomorphism proxy; see cfg_shape/1) and phi
%%%                        count.  This is the map's literal criterion and is
%%%                        expected to be low: guard-BIF `succeeded' blocks,
%%%                        `match_fail' clause tails and tuple/list build get
%%%                        restructured by codegen.
%%%   * Semantic pass   -- identical *canonical op multiset* (see the
%%%                        equivalence table below), the codegen-invariant
%%%                        content: which functions/BIFs are called, which
%%%                        arithmetic/bitwise/data ops occur, which type tests
%%%                        and comparisons guard the flow.  This is the
%%%                        "modulo codegen" fidelity signal.
%%%
%%% Reconstruction failures (`not_eligible' / `build_failed' / `not_found')
%%% are tallied separately -- they are not comparison divergences.
%%%
%%% ---------------------------------------------------------------------
%%% Equivalence table (canonical token per op, applied to *both* sides).
%%% Tokens that carry no cross-representation meaning are dropped
%%% (`ignore'); everything else folds to one canonical vocabulary so the
%%% AOT `#b_set{}.op' vocabulary and the T2 `T2OpKind' vocabulary compare:
%%%
%%%   canonical            AOT beam_ssa op           T2 op kind
%%%   -------------------  ------------------------  ------------------------
%%%   {call,M,F,A}         call w/ b_local/b_remote  call, call_ext,
%%%                                                  tail_call, tail_call_ext
%%%   {call_fun,A}         call w/ var target        call_fun, tail_call_fun
%%%   {arith,Op,Ar}        {bif,'+'|'-'|'*'|...}     add,sub,mul,idiv,rem,
%%%                                                  band,bor,bxor,bsl,bsr,
%%%                                                  bnot,neg
%%%   {cmp,Op}             {bif,'=:='|'<'|...}       cmp_eq_exact,cmp_lt,...
%%%   {test,Kind}          {bif,is_tuple|...},       is_integer..is_function,
%%%                        is_nonempty_list,         is_nil,is_nonempty_list,
%%%                        is_tagged_tuple           is_tagged_tuple,test_arity
%%%   {bif,N,Ar}           {bif,element|tuple_size|  bif,guard_bif (via mfa)
%%%                        length|hd|...}
%%%   {data,X}             get_hd,get_tl,            get_hd,get_tl,
%%%                        get_tuple_element,        get_tuple_element,
%%%                        put_tuple,put_list,       make_tuple,make_list,
%%%                        get_map_element,make_fun  get_map_element,make_fun
%%%   {bs,X}               bs_match,bs_create_bin..  (P0 builder: not emitted)
%%%   ignore               phi,{succeeded,_},        phi,param,const_*,
%%%                        match_fail,executable_line succeeded,gc_test,
%%%                        debug_line,...            reduction_check,...
%%%
%%% Known-benign lowering divergences (documented, not chased):
%%%   * AOT `{bif,'=:='}' vs literal `[]'   <->  T2 `is_nil'
%%%   * AOT `{bif,tuple_size}'+`{bif,'=:='}' <->  T2 `test_arity'
%%%   * AOT non-tail `call'+`ret'            <->  T2 `tail_call*' (codegen LCO)
%%%   * T2 materialises constants/params as ops; AOT inlines them as operands.
%%%   * Error exits are spelled differently on the two sides.  On the AOT side
%%%     a clause/guard failure is a `match_fail' (badmatch/case_end/if_end/
%%%     function_clause) that lowers, at codegen, to a dedicated BEAM op; the
%%%     T2 builder reads that BEAM op back as an `erlang:error' tail call
%%%     (`translate_error_exit'/`get_error_block', t2_hir_builder.cpp), the
%%%     function_clause edges collapsed to one shared error block.  So the
%%%     canonical `call' multiset differs by `{call,erlang,error,any}' entries
%%%     wherever a function has error exits -- a codegen spelling, not lost
%%%     content.  `classify_calls/1' separates this class from a genuinely
%%%     missing/extra *user-level* call so the gate can be judged honestly.
%%%
%%% dprecg-only ops folded here (introduced by beam_ssa_pre_codegen, so they
%%% never appear at dssaopt and leave the dssaopt numbers untouched):
%%%   * bare `succeeded' (sanitize rewrites `{succeeded,_}' -> `succeeded')
%%%   * `copy' (y-register save scaffolding)              -> ignore
%%%   * `bs_get'/`bs_*' (fix_bs lowering of bs_match)      -> {bs,_}
%%%   * `set_tuple_element' (update_tuple/record lowering) -> {data,_}
%%% =====================================================================

-module(t2_g1_SUITE).

%% Common Test API
-export([suite/0, all/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2]).
-export([experiment_subjects/1, stdlib_slice/1]).

%% Standalone driver (runs on a node started with T2_RETAIN=1). Handy for
%% manual runs: `T2_RETAIN=1 erl ... -run t2_g1_SUITE run -s init stop'.
-export([run/0, run/1, run/2,
         compare_module/2, compare_module/3,
         format_report/1, classify_calls/1]).

%% Called by name over erpc from the controlling node onto the T2 peer.
-export([module_exports/1, load_plan_module/2]).

-include_lib("common_test/include/ct.hrl").

%% AOT reference points, closest-to-loaded last.  Both legs report both.
-define(MODES, [dssaopt, dprecg]).

suite() ->
    [{timetrap, {minutes, 10}}].

all() ->
    [experiment_subjects, stdlib_slice].

groups() ->
    [].

%% ---------------------------------------------------------------------
%% Corpus (PLAN/T2FULL/07 §"Corpus = experiment subjects + stdlib slice")
%% ---------------------------------------------------------------------

%% Experiment subjects: {Module, [{F,A}], SourceHint}. SourceHint locates
%% the PLAN kit sources for the in-tree experiment modules; `stdlib'/`app'
%% subjects come from the loaded beam's abstract_code.
experiment_corpus() ->
    [{t2_mvp,    [{total,2},{diff,2}],                    {plan,"mvp"}},
     {json,      [{number,7},{number_frac_cont,7},{string_ascii,7}], stdlib},
     {t2_gmap,   [{sum_scores,2}],                        {plan,"verification"}},
     {t2_g31,    [{dispatch,2},{handle_call,3}],          {plan,"verification"}},
     {erl_types, [{is_limited,2},{are_all_limited,2}],    app}].

%% Stdlib slice: all exported functions of each module (loop / bs_match /
%% tuple / list / map shapes the tier targets).
stdlib_corpus() ->
    [lists, maps, binary, string, base64, json, unicode].

%% ---------------------------------------------------------------------
%% Common Test wiring -- runs the comparison on a peer node booted with
%% T2_RETAIN=1 so freshly loaded modules retain their decoded code.
%% ---------------------------------------------------------------------

init_per_suite(Config) ->
    case erlang:system_info(emu_flavor) of
        jit ->
            case start_t2_peer() of
                {ok, Peer, Node} ->
                    [{peer, Peer}, {peer_node, Node} | Config];
                {error, Reason} ->
                    {skip, {no_t2_peer, Reason}}
            end;
        _ ->
            {skip, "tier-2 reconstruction requires the JIT emulator"}
    end.

end_per_suite(Config) ->
    case proplists:get_value(peer, Config) of
        undefined -> ok;
        Peer -> try peer:stop(Peer) catch _:_ -> ok end
    end,
    ok.

init_per_group(_G, Config) -> Config.
end_per_group(_G, _Config) -> ok.

start_t2_peer() ->
    PA = filename:dirname(code:which(?MODULE)),
    try
        {ok, Peer, Node} =
            peer:start(#{name => peer:random_name(?MODULE),
                         env => [{"T2_RETAIN", "1"}],
                         args => ["-pa", PA]}),
        %% Confirm the reconstruction BIF is actually reachable; if this
        %% emulator predates the commit-5 wiring, skip rather than error.
        _ = erpc:call(Node, erts_debug, set_internal_state,
                      [available_internal_state, true]),
        undefined = erpc:call(Node, erts_debug, get_internal_state,
                              [{t2_build_ssa, '$nomod$', f, 0}]),
        {ok, Peer, Node}
    catch
        Class:CaughtReason ->
            {error, {Class, CaughtReason}}
    end.

experiment_subjects(Config) ->
    Node = ?config(peer_node, Config),
    Root = erpc:call(Node, code, root_dir, []),
    %% Loading is mode-independent -- do it once, then compare at every point.
    _ = [ok = ensure_corpus_module(Node, M, Src, Root)
         || {M, _FAs, Src} <- experiment_corpus()],
    run_leg(experiment_subjects,
            fun(Mode) ->
                    [erpc:call(Node, ?MODULE, compare_module, [M, FAs, Mode])
                     || {M, FAs, _Src} <- experiment_corpus()]
            end).

stdlib_slice(Config) ->
    Node = ?config(peer_node, Config),
    _ = [erpc:call(Node, code, ensure_loaded, [M]) || M <- stdlib_corpus()],
    run_leg(stdlib_slice,
            fun(Mode) ->
                    [begin
                         FAs = erpc:call(Node, ?MODULE, module_exports, [M]),
                         erpc:call(Node, ?MODULE, compare_module, [M, FAs, Mode])
                     end || M <- stdlib_corpus()]
            end).

%% Compare the corpus at every reference point (?MODES) and log a report per
%% point.  The gate is "runnable / emits a diff report" -- divergences are the
%% expected output, not a failure.  We only fail if the machinery is broken:
%% the reconstruction BIF produced *nothing* usable at some reference point.
run_leg(Tag, MkReports) ->
    Results = [{Mode, aggregate(MkReports(Mode))} || Mode <- ?MODES],
    _ = [ct:log("~ts", [format_report({{Tag, Mode}, Agg})])
         || {Mode, Agg} <- Results],
    _ = [case Recon of
             0 -> ct:fail({no_reconstructions, Tag, Mode});
             _ -> ok
         end || {Mode, #{reconstructed := Recon}} <- Results],
    {comment, legs_comment(Results)}.

legs_comment(Results) ->
    lists:flatten(
      lists:join("; ",
                 [io_lib:format("~w: ~ts", [Mode, report_comment(Agg)])
                  || {Mode, Agg} <- Results])).

%% Exported so the peer can call them by name over erpc.
module_exports(M) ->
    [{F, A} || {F, A} <- M:module_info(exports), F =/= module_info].

%% ---------------------------------------------------------------------
%% Standalone driver
%% ---------------------------------------------------------------------

run() ->
    run(all).

%% Drive the corpus at every reference point, print a report per point, and
%% return `[{Mode, Agg}]'.  Each `Agg' keeps the full per-function diff
%% (`diverged'), so a caller can post-process (see classify_calls/1).
run(Which) ->
    [{Mode, run(Which, Mode)} || Mode <- ?MODES].

run(Which, Mode) ->
    _ = erts_debug:set_internal_state(available_internal_state, true),
    Root = code:root_dir(),
    Experiment =
        case Which of
            stdlib -> [];
            _ ->
                [begin
                     ok = ensure_corpus_module(node(), M, Src, Root),
                     compare_module(M, FAs, Mode)
                 end || {M, FAs, Src} <- experiment_corpus()]
        end,
    Stdlib =
        case Which of
            experiment -> [];
            _ ->
                [begin
                     _ = code:ensure_loaded(M),
                     compare_module(M, module_exports(M), Mode)
                 end || M <- stdlib_corpus()]
        end,
    Reports = Experiment ++ Stdlib,
    Agg = aggregate(Reports),
    io:format("~ts~n", [format_report({{run, Mode}, Agg})]),
    Agg.

%% ---------------------------------------------------------------------
%% Corpus module loading (must run on the T2_RETAIN node)
%% ---------------------------------------------------------------------

ensure_corpus_module(Node, M, stdlib, _Root) ->
    _ = erpc:call(Node, code, ensure_loaded, [M]),
    ok;
ensure_corpus_module(Node, M, app, _Root) ->
    _ = erpc:call(Node, code, ensure_loaded, [M]),
    ok;
ensure_corpus_module(Node, M, {plan, Sub}, Root) ->
    Src = filename:join([Root, "PLAN", Sub, atom_to_list(M) ++ ".erl"]),
    case erpc:call(Node, filelib, is_file, [Src]) of
        true ->
            ok = erpc:call(Node, ?MODULE, load_plan_module, [M, Src]);
        false ->
            %% Source not shipped (e.g. a release tree); leave whatever is
            %% loaded. compare_module/2 will report `not_found'/`no_aot'.
            ok
    end.

%% Compile a PLAN kit source with debug_info and load it so it is retained.
%% Exported for the erpc bounce. Returns ok.
load_plan_module(M, Src) ->
    {ok, M, Bin} = compile:file(Src, [binary, debug_info, report_errors]),
    {module, M} = code:load_binary(M, Src, Bin),
    ok.

%% ---------------------------------------------------------------------
%% Per-module comparison
%% ---------------------------------------------------------------------

compare_module(M, FAs) ->
    compare_module(M, FAs, dssaopt).

compare_module(M, FAs, Mode) ->
    Aot = aot_functions(M, Mode),
    Fns = [compare_fn(M, FA, Aot) || FA <- FAs],
    {M, Fns}.

compare_fn(M, {F, A} = FA, Aot) ->
    T2 = fetch_t2(M, F, A),
    case T2 of
        {error, Reason} ->
            #{mfa => {M, F, A}, status => Reason};
        {ok, T2Sum} ->
            case maps:find(FA, Aot) of
                {ok, AotSum} ->
                    diff(M, FA, AotSum, T2Sum);
                error ->
                    #{mfa => {M, F, A}, status => no_aot}
            end
    end.

%% ---------------------------------------------------------------------
%% Tier-2 side: fetch + summarise the reconstruction
%% ---------------------------------------------------------------------

fetch_t2(M, F, A) ->
    case erts_debug:get_internal_state({t2_build_ssa, M, F, A}) of
        {ok, {_Mfa, Counts, Blocks}} ->
            {ok, t2_summarise(Counts, Blocks)};
        undefined            -> {error, not_retained};
        {error, not_found}   -> {error, not_found};
        {error, not_eligible}-> {error, not_eligible};
        {error, build_failed, _} -> {error, build_failed};
        Other                -> {error, {bad_reply, Other}}
    end.

t2_summarise({NBlocks, _NValues, NPhis, _NBody, _NTerm}, Blocks) ->
    Edges = lists:append([[{Id, S} || S <- usort_ints(Succs)]
                          || {Id, _Preds, Succs, _Phis, _Body, _Term} <- Blocks]),
    Canon = lists:append([t2_block_canon(B) || B <- Blocks]),
    #{nblocks => NBlocks,
      nedges  => length(Edges),
      degrees => cfg_shape_from_edges(Blocks, Edges, fun t2_block_id/1),
      nphi    => NPhis,
      canon   => bag(Canon)}.

t2_block_id({Id, _, _, _, _, _}) -> Id.

t2_block_canon({_Id, _Preds, _Succs, _Phis, Body, Term}) ->
    %% Body ops plus the terminator: T2 folds tail calls into the block
    %% terminator (tail_call/tail_call_ext/tail_call_fun), whereas the AOT
    %% SSA still carries them as `call' body ops one lowering above LCO.
    %% Canonicalising the terminator too keeps the call multisets aligned;
    %% branch/jump/switch/return terminators canonicalise to `ignore'.
    Ops = Body ++ [Term || Term =/= none],
    [C || Op <- Ops, C <- [canon_t2_op(Op)], C =/= ignore].

canon_t2_op({Kind, _Result, _Type, Operands, Attrs}) ->
    canon_t2(Kind, Operands, Attrs).

%% ---------------------------------------------------------------------
%% AOT side: compile to dssaopt and summarise every function
%% ---------------------------------------------------------------------

aot_functions(M, Mode) ->
    case aot_module(M, Mode) of
        {ok, Body} ->
            maps:from_list(
              [{fn_key(Anno), aot_summarise(M, Bs)}
               || {b_function, Anno, _Args, Bs, _Cnt} <- Body]);
        error ->
            #{}
    end.

%% Obtain the AOT beam_ssa at the reference point `Mode' (dssaopt | dprecg).
%% Prefer the loaded beam's abstract_code; for the PLAN experiment modules
%% `code:which/1' resolves to the `.erl' source they were loaded from
%% (code:load_binary), so compile that directly.
%%
%% The listing pass unconditionally writes `<Mod>.<ext>' (compile.erl:
%% listing/3), so it is directed to a scratch dir and removed.
aot_module(M, Mode) ->
    Result = aot_module_1(M, Mode),
    _ = file:delete(filename:join(ssaopt_tmp(),
                                  atom_to_list(M) ++ listing_ext(Mode))),
    Result.

aot_module_1(M, Mode) ->
    case code:which(M) of
        Path when is_list(Path) ->
            case filename:extension(Path) of
                ".erl" ->
                    aot_forms(fun() -> compile:file(Path, aot_opts(M, Mode)) end, M);
                ".beam" ->
                    case beam_lib:chunks(Path, [abstract_code]) of
                        {ok, {M, [{abstract_code, {raw_abstract_v1, Abstr}}]}} ->
                            aot_forms(fun() -> compile:forms(Abstr, aot_opts(M, Mode)) end, M);
                        _ ->
                            error
                    end;
                _ ->
                    error
            end;
        _ ->
            error
    end.

%% The `{listing,_}' pass at the chosen point truncates the pipeline; with
%% `binary' set, `compile' returns the intermediate `#b_module{}' as the
%% "object code" (aot_forms/2 unpacks it).
aot_opts(M, Mode) ->
    [listing_flag(Mode), binary, return_errors,
     {outdir, ssaopt_tmp()} | opt_opts(M)].

listing_flag(dssaopt) -> dssaopt;
listing_flag(dprecg)  -> dprecg.

listing_ext(dssaopt) -> ".ssaopt";
listing_ext(dprecg)  -> ".precodegen".

ssaopt_tmp() ->
    Base = case os:getenv("TMPDIR") of
               false -> "/tmp";
               "" -> "/tmp";
               T -> T
           end,
    Dir = filename:join(Base, "t2_g1_ssaopt"),
    _ = file:make_dir(Dir),
    Dir.

aot_forms(F, M) ->
    try F() of
        {ok, M, {b_module, _, _, _, _, Body}} -> {ok, Body};
        _ -> error
    catch
        _:_ -> error
    end.

fn_key(Anno) ->
    {_, F, A} = maps:get(func_info, Anno),
    {F, A}.

aot_summarise(SelfMod, Bs) ->
    Blocks = [{L, maps:get(L, Bs)} || L <- lists:sort(maps:keys(Bs))],
    Edges = lists:append([aot_edges(L, Blk) || {L, Blk} <- Blocks]),
    NPhi = lists:sum([aot_phi_count(Blk) || {_, Blk} <- Blocks]),
    Canon = lists:append([aot_block_canon(SelfMod, Blk) || {_, Blk} <- Blocks]),
    #{nblocks => length(Blocks),
      nedges  => length(Edges),
      degrees => cfg_shape_from_edges([L || {L, _} <- Blocks], Edges, fun(X) -> X end),
      nphi    => NPhi,
      canon   => bag(Canon)}.

aot_edges(L, {b_blk, _, _Is, Last}) ->
    [{L, T} || T <- usort_ints(aot_succs(Last))].

aot_succs({b_br, _, _Bool, Succ, Fail}) -> [Succ, Fail];
aot_succs({b_ret, _, _Arg})             -> [];
aot_succs({b_switch, _, _Arg, Fail, List}) -> [Fail | [T || {_, T} <- List]];
aot_succs(_)                            -> [].

aot_phi_count({b_blk, _, Is, _Last}) ->
    length([1 || {b_set, _, _Dst, phi, _Args} <- Is]).

aot_block_canon(SelfMod, {b_blk, _, Is, _Last}) ->
    [C || {b_set, _, _Dst, Op, Args} <- Is,
          C <- [canon_aot(Op, Args, SelfMod)], C =/= ignore].

%% ---------------------------------------------------------------------
%% Diff two summaries -> per-function report
%% ---------------------------------------------------------------------

diff(M, {F, A}, Aot, T2) ->
    #{nblocks := AB, nedges := AE, degrees := AD, nphi := AP, canon := AC} = Aot,
    #{nblocks := TB, nedges := TE, degrees := TD, nphi := TP, canon := TC} = T2,

    CfgClasses =
        [block_count || AB =/= TB] ++
        [edge_count  || AB =:= TB, AE =/= TE] ++
        [edge_shape  || AB =:= TB, AE =:= TE, AD =/= TD] ++
        [phi_count   || AP =/= TP],
    CfgPass = (CfgClasses =:= []),

    SemClasses = canon_divergence_classes(AC, TC),
    SemPass = (SemClasses =:= []),

    #{mfa       => {M, F, A},
      status    => reconstructed,
      cfg_pass  => CfgPass,
      sem_pass  => SemPass,
      classes   => lists:usort(CfgClasses ++ SemClasses),
      metrics   => #{aot => {AB, AE, AP}, t2 => {TB, TE, TP}},
      canon_diff=> canon_delta(AC, TC)}.

%% Which canonical *categories* differ (for classification / ranking).
canon_divergence_classes(AC, TC) ->
    Cats = [call, call_fun, arith, cmp, test, bif, data, bs],
    [Cat || Cat <- Cats,
            category_bag(Cat, AC) =/= category_bag(Cat, TC)].

category_bag(Cat, Bag) ->
    maps:filter(fun(Tok, _) -> element(1, Tok) =:= Cat end, Bag).

%% Symmetric multiset delta {OnlyAot, OnlyT2} for reporting detail.
canon_delta(AC, TC) ->
    OnlyA = bag_subtract(AC, TC),
    OnlyT = bag_subtract(TC, AC),
    {OnlyA, OnlyT}.

%% ---------------------------------------------------------------------
%% Canonicalisation (the equivalence table)
%% ---------------------------------------------------------------------

canon_aot(call, [Target | _], SelfMod) ->
    canon_call_target(Target, SelfMod);
%% `=:= []' / `== []' is the source form of a nil test; codegen lowers the
%% equivalent tier-2 check to a dedicated nil test. Fold to match T2's
%% `is_nil' (documented benign lowering).
canon_aot({bif, N}, Args, _Self) when N =:= '=:='; N =:= '==' ->
    case lists:member({b_literal, []}, Args) of
        true -> {test, is_nil};
        false -> canon_bif(N, length(Args))
    end;
canon_aot({bif, N}, Args, _Self) ->
    canon_bif(N, length(Args));
canon_aot(is_nonempty_list, _, _) -> {test, is_nonempty_list};
canon_aot(is_tagged_tuple, _, _)  -> {test, is_tagged_tuple};
canon_aot(get_hd, _, _)           -> {data, get_hd};
canon_aot(get_tl, _, _)           -> {data, get_tl};
canon_aot(get_tuple_element, _, _)-> {data, get_tuple_element};
canon_aot(put_tuple, _, _)        -> {data, make_tuple};
canon_aot(put_list, _, _)         -> {data, make_list};
canon_aot(put_map, _, _)          -> {data, make_map};
canon_aot(get_map_element, _, _)  -> {data, get_map_element};
canon_aot(has_map_field, _, _)    -> {test, has_map_field};
canon_aot(update_record, _, _)    -> {data, update_record};
canon_aot(update_tuple, _, _)     -> {data, update_tuple};
canon_aot(make_fun, _, _)         -> {data, make_fun};
canon_aot(old_make_fun, _, _)     -> {data, make_fun};
canon_aot(bs_match, _, _)         -> {bs, bs_match};
canon_aot(bs_start_match, _, _)   -> {bs, bs_start_match};
canon_aot(bs_get_tail, _, _)      -> {bs, bs_get_tail};
canon_aot(bs_get_position, _, _)  -> {bs, bs_get_position};
canon_aot(bs_set_position, _, _)  -> {bs, bs_set_position};
canon_aot(bs_create_bin, _, _)    -> {bs, bs_create_bin};
canon_aot(bs_extract, _, _)       -> {bs, bs_extract};
canon_aot(bs_ensure, _, _)        -> {bs, bs_ensure};
canon_aot(bs_ensured_match, _, _) -> {bs, bs_match};
canon_aot(bs_ensured_skip, _, _)  -> {bs, bs_skip};
canon_aot(bs_skip, _, _)          -> {bs, bs_skip};
canon_aot(set_tuple_element, _, _)-> {data, set_tuple_element};
%% Structural / scaffolding ops with no cross-representation counterpart.
canon_aot(phi, _, _)              -> ignore;
canon_aot({succeeded, _}, _, _)   -> ignore;
%% dprecg: `sanitize' rewrites `{succeeded,_}' -> bare `succeeded'.
canon_aot(succeeded, _, _)        -> ignore;
%% dprecg: `copy' saves a value to a y register before a call; pure
%% register-allocation scaffolding with no semantic content.
canon_aot(copy, _, _)             -> ignore;
canon_aot(match_fail, _, _)       -> ignore;
canon_aot(executable_line, _, _)  -> ignore;
canon_aot(debug_line, _, _)       -> ignore;
canon_aot(nif_start, _, _)        -> ignore;
canon_aot({float, _}, _, _)       -> ignore;
canon_aot(new_try_tag, _, _)      -> ignore;
canon_aot(kill_try_tag, _, _)     -> ignore;
canon_aot(extract, _, _)          -> ignore;
canon_aot(landingpad, _, _)       -> ignore;
canon_aot(resume, _, _)           -> ignore;
canon_aot(catch_end, _, _)        -> ignore;
%% dprecg: `fix_bs' lowers bs_match+bs_extract to bs_get and friends. Keep
%% every binary-match op in the `bs' bucket so bs functions stay counted at
%% both reference points (the P0 T2 builder emits no bs ops, so `bs' is a
%% known-diverging bucket regardless of where we sample the AOT side).
canon_aot(Op, _Args, _Self) when is_atom(Op) ->
    case atom_to_list(Op) of
        "bs_" ++ _ -> {bs, Op};
        _          -> {other, Op}
    end;
canon_aot(Op, _Args, _Self)       -> {other, Op}.

canon_call_target({b_local, {b_literal, F}, A}, SelfMod) -> {call, SelfMod, F, A};
%% The clause-failure / badarg edge is `erlang:error/1' in the AOT SSA but a
%% shared synthetic `tail_call_ext erlang:error/0' block in the T2 builder
%% (get_error_block, t2_hir_builder.cpp). Same edge, placeholder arity ->
%% fold both sides to an arity-agnostic token (documented benign lowering).
canon_call_target({b_remote, {b_literal, erlang}, {b_literal, error}, _}, _) ->
    {call, erlang, error, any};
canon_call_target({b_remote, {b_literal, Mod}, {b_literal, F}, A}, _) -> {call, Mod, F, A};
canon_call_target(_Var, _Self) -> {call_fun, dynamic}.

canon_bif(N, Ar) ->
    case bif_class(N) of
        test -> {test, N};
        cmp  -> {cmp, N};
        arith-> {arith, N, Ar};
        other-> {bif, N, Ar}
    end.

bif_class(N) ->
    TypeTests = [is_atom, is_boolean, is_integer, is_float, is_number,
                 is_tuple, is_list, is_map, is_binary, is_bitstring,
                 is_function, is_pid, is_port, is_reference],
    Cmps = ['==', '/=', '=:=', '=/=', '<', '>', '=<', '>='],
    Ariths = ['+', '-', '*', '/', 'div', 'rem',
              'band', 'bor', 'bxor', 'bsl', 'bsr', 'bnot'],
    case {lists:member(N, TypeTests), lists:member(N, Cmps),
          lists:member(N, Ariths)} of
        {true, _, _} -> test;
        {_, true, _} -> cmp;
        {_, _, true} -> arith;
        _            -> other
    end.

%% T2OpKind -> canonical token (mirror of the AOT mapping above).
canon_t2(Kind, Operands, Attrs) ->
    case Kind of
        call          -> t2_call(Attrs);
        call_ext      -> t2_call(Attrs);
        tail_call     -> t2_call(Attrs);
        tail_call_ext -> t2_call(Attrs);
        call_fun      -> {call_fun, dynamic};
        tail_call_fun -> {call_fun, dynamic};
        make_fun      -> {data, make_fun};

        add    -> {arith, '+', length(Operands)};
        sub    -> {arith, '-', length(Operands)};
        mul    -> {arith, '*', length(Operands)};
        idiv   -> {arith, 'div', length(Operands)};
        'rem'  -> {arith, 'rem', length(Operands)};
        'band' -> {arith, 'band', length(Operands)};
        'bor'  -> {arith, 'bor', length(Operands)};
        'bxor' -> {arith, 'bxor', length(Operands)};
        'bsl'  -> {arith, 'bsl', length(Operands)};
        'bsr'  -> {arith, 'bsr', length(Operands)};
        'bnot' -> {arith, 'bnot', length(Operands)};
        neg    -> {arith, '-', 1};

        cmp_eq_exact -> {cmp, '=:='};
        cmp_ne_exact -> {cmp, '=/='};
        cmp_eq -> {cmp, '=='};
        cmp_ne -> {cmp, '/='};
        cmp_lt -> {cmp, '<'};
        cmp_le -> {cmp, '=<'};
        cmp_gt -> {cmp, '>'};
        cmp_ge -> {cmp, '>='};

        bif       -> t2_bif(Attrs, Operands);
        guard_bif -> t2_bif(Attrs, Operands);

        test_arity       -> {test, test_arity};
        is_nil           -> {test, is_nil};
        is_nonempty_list -> {test, is_nonempty_list};
        is_tagged_tuple  -> {test, is_tagged_tuple};
        is_integer -> {test, is_integer};
        is_float   -> {test, is_float};
        is_number  -> {test, is_number};
        is_atom    -> {test, is_atom};
        is_boolean -> {test, is_boolean};
        is_tuple   -> {test, is_tuple};
        is_list    -> {test, is_list};
        is_map     -> {test, is_map};
        is_binary  -> {test, is_binary};
        is_bitstring -> {test, is_bitstring};
        is_pid     -> {test, is_pid};
        is_port    -> {test, is_port};
        is_reference -> {test, is_reference};
        is_function  -> {test, is_function};

        get_hd -> {data, get_hd};
        get_tl -> {data, get_tl};
        get_tuple_element -> {data, get_tuple_element};
        make_tuple -> {data, make_tuple};
        make_list  -> {data, make_list};
        get_map_element -> {data, get_map_element};

        %% Materialisation / scaffolding: no AOT op counterpart.
        _ -> ignore
    end.

t2_call(Attrs) ->
    case proplists:get_value(mfa, Attrs) of
        {erlang, error, _} -> {call, erlang, error, any};
        {M, F, A} -> {call, M, F, A};
        _ -> {call_fun, dynamic}
    end.

t2_bif(Attrs, Operands) ->
    case proplists:get_value(mfa, Attrs) of
        {_M, F, A} -> canon_bif(F, A);
        _ -> {bif, unknown, length(Operands)}
    end.

%% ---------------------------------------------------------------------
%% CFG shape proxy: (block count, sorted out-degree bag, sorted in-degree
%% bag). Cheap stand-in for edge-set-modulo-renumber isomorphism; graphs
%% here are tiny, so collisions are rare and always surface as a genuine
%% shape signature rather than a false match.
%% ---------------------------------------------------------------------

cfg_shape_from_edges(BlocksOrIds, Edges, IdOf) ->
    Ids = [IdOf(B) || B <- BlocksOrIds],
    OutDeg = lists:sort([count_from(Id, Edges) || Id <- Ids]),
    InDeg  = lists:sort([count_to(Id, Edges) || Id <- Ids]),
    {OutDeg, InDeg}.

count_from(Id, Edges) -> length([1 || {F, _} <- Edges, F =:= Id]).
count_to(Id, Edges)   -> length([1 || {_, T} <- Edges, T =:= Id]).

%% ---------------------------------------------------------------------
%% Aggregation + reporting
%% ---------------------------------------------------------------------

aggregate(Reports) ->
    Fns = lists:append([Fns || {_M, Fns} <- Reports]),
    Recon = [R || #{status := reconstructed} = R <- Fns],
    Fail = [R || #{status := S} = R <- Fns, S =/= reconstructed],
    CfgPass = length([1 || #{cfg_pass := true} <- Recon]),
    SemPass = length([1 || #{sem_pass := true} <- Recon]),
    ClassTally = tally([C || #{classes := Cs} <- Recon, C <- Cs]),
    FailTally = tally([S || #{status := S} <- Fail]),
    #{modules      => length(Reports),
      functions    => length(Fns),
      reconstructed=> length(Recon),
      cfg_pass     => CfgPass,
      sem_pass     => SemPass,
      class_tally  => ClassTally,
      fail_tally   => FailTally,
      per_module   => [{M, module_summary(Fns2)} || {M, Fns2} <- Reports],
      diverged     => [R || #{status := reconstructed, sem_pass := false} = R <- Recon]}.

module_summary(Fns) ->
    Recon = [R || #{status := reconstructed} = R <- Fns],
    #{functions => length(Fns),
      reconstructed => length(Recon),
      cfg_pass => length([1 || #{cfg_pass := true} <- Recon]),
      sem_pass => length([1 || #{sem_pass := true} <- Recon])}.

report_comment(#{reconstructed := R, cfg_pass := C, sem_pass := S}) ->
    lists:flatten(io_lib:format("reconstructed=~p cfg_pass=~p sem_pass=~p",
                                [R, C, S])).

format_report({Tag, Agg}) ->
    #{modules := Mods, functions := Fns, reconstructed := Recon,
      cfg_pass := Cfg, sem_pass := Sem,
      class_tally := Classes, fail_tally := Fails,
      per_module := PerMod, diverged := Diverged} = Agg,
    [io_lib:format("==== T2 G1 fidelity report [~p] ====~n", [Tag]),
     io_lib:format("modules=~p functions=~p reconstructed=~p~n",
                   [Mods, Fns, Recon]),
     io_lib:format("CFG-shape pass : ~p/~p (~ts)~n",
                   [Cfg, Recon, pct(Cfg, Recon)]),
     io_lib:format("Semantic  pass : ~p/~p (~ts)~n",
                   [Sem, Recon, pct(Sem, Recon)]),
     "-- reconstruction outcomes (non-reconstructed) --\n",
     [io_lib:format("   ~-16w ~p~n", [S, N]) || {S, N} <- Fails],
     "-- top divergence classes (over reconstructed fns) --\n",
     [io_lib:format("   ~-16w ~p~n", [C, N])
      || {C, N} <- lists:reverse(lists:keysort(2, Classes))],
     "-- per module --\n",
     [io_lib:format("   ~-12w fns=~p recon=~p cfg=~p sem=~p~n",
                    [M, maps:get(functions, MS), maps:get(reconstructed, MS),
                     maps:get(cfg_pass, MS), maps:get(sem_pass, MS)])
      || {M, MS} <- PerMod],
     format_call_bucket(Diverged),
     "-- semantic divergences (sample, <=20) --\n",
     [format_diverged(D) || D <- lists:sublist(Diverged, 20)]].

%% Call-bucket decomposition: of the semantically-diverging functions, how
%% many differ *only* in the call bucket by benign lowering (error-exit
%% spelling, apply/fun materialisation, synthetic clause stubs) vs. a genuine
%% missing/extra user-level call (candidate reconstruction loss)?
format_call_bucket(Diverged) ->
    {Cats, Loss} = classify_calls(Diverged),
    ["-- call-bucket decomposition (over semantically-diverged fns) --\n",
     [io_lib:format("   ~-20w ~p~n", [Cat, N]) || {Cat, N} <- Cats],
     case Loss of
         [] -> "   (no genuine missing/extra user-level call found)\n";
         _  ->
             ["   !! candidate reconstruction loss:\n",
              [io_lib:format("      ~w:~w/~w  ~p~n", [M, F, A, Detail])
               || {{M, F, A}, Detail} <- Loss]]
     end].

format_diverged(#{mfa := {M, F, A}, classes := Cs, canon_diff := {OnlyA, OnlyT}}) ->
    io_lib:format("   ~w:~w/~w classes=~w~n"
                  "        only_aot=~s~n        only_t2 =~s~n",
                  [M, F, A, Cs, fmt_bag(OnlyA), fmt_bag(OnlyT)]).

fmt_bag(Bag) ->
    case maps:to_list(Bag) of
        [] -> "-";
        L -> lists:flatten([io_lib:format("~w*~p ", [T, N]) || {T, N} <- L])
    end.

pct(_, 0) -> "n/a";
pct(N, D) -> io_lib:format("~.1f%", [100.0 * N / D]).

%% ---------------------------------------------------------------------
%% Call-bucket classifier (analysis; supports the G1 gate decision)
%% ---------------------------------------------------------------------
%%
%% Given a run's `diverged' list (or the whole Agg), fold every diverging
%% function into one call-bucket category and separate benign lowering from a
%% candidate reconstruction loss.  Returns {[{Category, Count}], LossDetail}:
%%
%%   non_call            divergence is not in the call/call_fun bucket at all
%%   error_exit_only     only `{call,erlang,error,any}' counts differ
%%   apply_or_fun        only `{call_fun,dynamic}' counts differ (+/- errexit)
%%   synth_stub_only     only synthetic (`-inlined-'/compiler) stub calls differ
%%   missing_user_call   a concrete user M:F/A is on AOT but not T2   (LOSS?)
%%   extra_user_call     a concrete user M:F/A is on T2 but not AOT   (LOSS?)
%%   mixed_user_call     concrete user calls differ on both sides     (LOSS?)
classify_calls(#{diverged := Diverged}) ->
    classify_calls(Diverged);
classify_calls(Diverged) when is_list(Diverged) ->
    Tagged = [{maps:get(mfa, D), classify_one(D)} || D <- Diverged],
    Cats = tally([Cat || {_Mfa, {Cat, _}} <- Tagged]),
    LossCats = [missing_user_call, extra_user_call, mixed_user_call],
    Loss = [{Mfa, Detail} || {Mfa, {Cat, Detail}} <- Tagged,
                             lists:member(Cat, LossCats)],
    {lists:reverse(lists:keysort(2, Cats)), Loss}.

classify_one(#{canon_diff := {OnlyA, OnlyT}}) ->
    #{err := Ae, dyn := Ad, user := Au, synth := As} = call_tokens(OnlyA),
    #{err := Te, dyn := Td, user := Tu, synth := Ts} = call_tokens(OnlyT),
    AnyCall = (Ae + Ad + Te + Td) > 0
        orelse Au =/= [] orelse As =/= [] orelse Tu =/= [] orelse Ts =/= [],
    case {Au, Tu} of
        {[], []} when not AnyCall -> {non_call, []};
        {[], []} ->
            %% Only benign lowering differs.  Name the dominant sub-class.
            if Ad + Td > 0        -> {apply_or_fun, #{dyn => {Ad, Td},
                                                      err => {Ae, Te}}};
               As =/= [] orelse Ts =/= [] -> {synth_stub_only, #{synth => {As, Ts},
                                                                 err => {Ae, Te}}};
               true               -> {error_exit_only, #{err => {Ae, Te}}}
            end;
        {[_|_], []} -> {missing_user_call, Au};
        {[], [_|_]} -> {extra_user_call, Tu};
        {[_|_], [_|_]} -> {mixed_user_call, {Au, Tu}}
    end.

%% Split a canon-delta bag's call/call_fun tokens into: erlang:error count,
%% call_fun count, concrete *user* calls, and concrete *synthetic* (compiler-
%% generated `-...-' name) calls.
call_tokens(Bag) ->
    Toks = [{Tok, N} || {Tok, N} <- maps:to_list(Bag),
                        element(1, Tok) =:= call orelse
                            element(1, Tok) =:= call_fun],
    Err = lists:sum([N || {{call, erlang, error, any}, N} <- Toks]),
    Dyn = lists:sum([N || {{call_fun, dynamic}, N} <- Toks]),
    Concrete = [{Tok, N} || {{call, _M, _F, _A} = Tok, N} <- Toks,
                            Tok =/= {call, erlang, error, any}],
    {Synth, User} =
        lists:partition(fun({{call, _M, F, _A}, _N}) -> synthetic_name(F) end,
                        Concrete),
    #{err => Err, dyn => Dyn, user => User, synth => Synth}.

%% Compiler-synthesised names (inlined function_clause stubs, list/funs, etc.)
%% are spelled with a leading `-'; they are never user-level calls.
synthetic_name(F) when is_atom(F) ->
    case atom_to_list(F) of
        "-" ++ _ -> true;
        _        -> false
    end;
synthetic_name(_) -> false.

%% ---------------------------------------------------------------------
%% Small helpers
%% ---------------------------------------------------------------------

%% Extract the optimisation-relevant compile options the module was really
%% built with, so the AOT SSA matches the loaded code (mirrors
%% compiler test_lib:opt_opts/1).
opt_opts(Mod) ->
    Comp = Mod:module_info(compile),
    Opts = proplists:get_value(options, Comp, []),
    Keep = [beam_debug_info, debug_info, dialyzer, inline, line_coverage,
            no_badrecord, no_bool_opt, no_bs_create_bin, no_bsm_opt,
            no_bs_match, no_copt, no_fun_opt, no_min_max_bifs, no_module_opt,
            no_postopt, no_recv_opt, no_share_opt, no_ssa_opt_float,
            no_ssa_opt_ranges, no_ssa_opt, no_stack_trimming, no_type_opt],
    lists:filter(fun({feature, _, enable}) -> true;
                    ({feature, _, disable}) -> true;
                    (Opt) -> lists:member(Opt, Keep)
                 end, Opts).

usort_ints(L) -> lists:usort(L).

bag(List) ->
    lists:foldl(fun(X, Acc) -> maps:update_with(X, fun(N) -> N + 1 end, 1, Acc) end,
                #{}, List).

bag_subtract(A, B) ->
    maps:fold(fun(K, Na, Acc) ->
                      Nb = maps:get(K, B, 0),
                      case Na - Nb of
                          D when D > 0 -> Acc#{K => D};
                          _ -> Acc
                      end
              end, #{}, A).

tally(List) -> maps:to_list(bag(List)).
