%% sinkscan.erl — M0.2 sinkable-allocation classifier for the T2-Full JIT plan.
%%
%% Sibling to elimscan.erl (M0.1). Reuses the same SSA-extraction and
%% cross-module resolution machinery: compiles each .erl to *optimized*
%% SSA (core_to_ssa -> beam_ssa_opt, the representation the loaded-BEAM
%% Type chunk serializes), builds a whole-corpus function index, then for
%% every ALLOCATION SITE classifies whether escape analysis + allocation
%% sinking (PLAN/T2FULL/03_optimizer.md §6) could remove the allocation.
%%
%% Allocation ops classified (beam_ssa):
%%   put_tuple  (put_tuple2) -> tuple, words = arity+1
%%   put_list                -> cons,  words = 2
%%   update_record           -> record tuple, words = Size+1
%%   put_map                 -> map (own category; words variable/approx)
%%   make_fun                -> closure (own category; words = env+fixed)
%%   bs_create_bin           -> binary (own category; words unknown)
%%
%% Tiers (03 §6.1 escape-point list; conservative — unresolvable => esc):
%%
%%   t1  (T1-local): every SSA use of the allocated value is local
%%       consumption — get_tuple_element / get_hd / get_tl / get_map_element
%%       / type tests / comparisons / element / is_tagged_tuple / match_fail
%%       — or it flows into another constructor that is itself t1 (follow ONE
%%       level; deeper => esc). Never returned, passed to a call, sent,
%%       stored (ETS/pd/pt), thrown, or captured in a closure.
%%
%%   t2  (T2-narrow-inline): the only escape is (a) as a freshly-constructed
%%       RETURN value whose cross-module-resolvable callers all immediately
%%       destructure it (the {ok,X} construct/deconstruct pair), or (b) as an
%%       ARGUMENT to a resolvable callee that only destructures that param.
%%       Becomes t1 under the plan's narrow P3 inlining.
%%
%%   esc (escaped): everything else — sent, stored, thrown, returned to
%%       unknown callers, closure-captured, deep-flowing, or used by any op
%%       we cannot prove local (conservative default).
%%
%% Emits one #asite{} per allocation site (consultable term list); weighting
%% by GALLOC volume is done by the driver (sinkdrv.erl).

-module(sinkscan).
-export([main/1, scan/2, classify_mods/1, forms_to_ssa/2]).
-export([dump_fn/2]).                       % debug helper

-include_lib("compiler/src/beam_ssa.hrl").

%% One allocation site.
-record(asite, {mfa,          % {M,F,A} of the enclosing function
                op,           % raw ssa op (put_tuple | put_list | ...)
                cat,          % tuple | cons | record | map | fun | bin
                words,        % integer (computable) | approx | unknown
                tier,         % t1 | t2 | esc
                why}).        % short reason atom (spot-check aid)

%% ---- escript entry -------------------------------------------------
%% main([OutFile, IncSpec | Files]) — IncSpec = comma-separated include dirs.
main([OutFile, IncSpec | Files]) ->
    Incs = string:tokens(IncSpec, ","),
    Sites = scan(Files, Incs),
    ok = file:write_file(OutFile, term_to_binary(Sites)),
    io:format("wrote ~p allocation sites to ~s~n", [length(Sites), OutFile]),
    halt(0).

%% ---- driver-callable API ------------------------------------------
%% scan(Files, Incs) -> [#asite{}]  (as plain tuples)
scan(Files, Incs) ->
    Opts = [to_core, binary, no_spawn_compiler_process, return_errors]
        ++ [{i, I} || I <- Incs],
    Mods = lists:foldl(fun(F, Acc) -> load_module(F, Opts, Acc) end, [], Files),
    classify_mods(Mods).

%% classify_mods(Mods) -> [#asite{}].  Mods = [{ModName, [#b_function{}]}],
%% i.e. already-optimized SSA. Shared by the .erl path (scan/2) and the
%% Elixir/beam path (forms_to_ssa/2 -> classify_mods).
classify_mods(Mods) ->
    Corpus = build_corpus(Mods),                 % MFA -> #{destruct}
    CallerInfo = build_caller_info(Mods, Corpus), % MFA -> {NCallers, AllDestruct}
    lists:flatmap(
      fun({Mod, Body}) ->
              lists:flatmap(fun(Fn) -> classify_fn(Mod, Fn, Corpus, CallerInfo) end, Body)
      end, Mods).

%% forms_to_ssa(ModName, ErlAbstractForms) -> {ModName, Body} | error.
%% Compiles Erlang abstract forms (e.g. recovered from an Elixir .beam's
%% debug_info via elixir_erl:debug_info(erlang_v1, ...)) to optimized SSA.
forms_to_ssa(ModName, Forms) ->
    try
        {ok, _M, Core} = compile:forms(Forms, [to_core, binary, return_errors]),
        {ok, SSA0, _} = beam_core_to_ssa:module(Core, []),
        {ok, #b_module{name=Name, body=Body}} = beam_ssa_opt:module(SSA0, []),
        {Name, Body}
    catch C:E:St ->
            io:format(standard_error, "forms_to_ssa ~p: ~p:~p~n~p~n", [ModName, C, E, St]),
            error
    end.

load_module(F, Opts, Acc) ->
    try compile:file(F, Opts) of
        {ok, _Mod, Core} ->
            case beam_core_to_ssa:module(Core, []) of
                {ok, SSA0, _} ->
                    case beam_ssa_opt:module(SSA0, []) of
                        {ok, #b_module{name=Name, body=Body}} ->
                            [{Name, Body} | Acc];
                        Other ->
                            io:format(standard_error, "ssaopt fail ~s: ~P~n", [F, Other, 5]),
                            Acc
                    end;
                Other ->
                    io:format(standard_error, "core2ssa fail ~s: ~P~n", [F, Other, 5]),
                    Acc
            end;
        Err ->
            io:format(standard_error, "compile fail ~s: ~P~n", [F, Err, 5]),
            Acc
    catch C:E ->
            io:format(standard_error, "exn ~s: ~p:~p~n", [F, C, E]),
            Acc
    end.

%% ---- Phase 1: corpus signatures (callee-side destructuring) --------
%% For each MFA record only what the t2-narrow test needs:
%%   destruct — set of param indices the callee ONLY destructures.
build_corpus(Mods) ->
    lists:foldl(
      fun({Mod, Body}, Acc0) ->
              lists:foldl(
                fun(#b_function{anno=Anno, args=Args, bs=Bs}, Acc) ->
                        MFA = mfa(Anno, Mod),
                        Acc#{MFA => #{destruct => param_destructured(Args, Bs)}}
                end, Acc0, Body)
      end, #{}, Mods).

%% ---- Phase 1.5: caller-side destructuring of each MFA's result -----
%% CallerInfo :: MFA -> {NCallers, AllDestruct::bool}. A caller "destructs"
%% the result of a call if the result var is used only via destructuring
%% (only_destructuring). This is the reverse of elimscan's construct/
%% deconstruct part (a): the {ok,X} pair from the callee's side.
build_caller_info(Mods, Corpus) ->
    lists:foldl(
      fun({Mod, Body}, Acc0) ->
              lists:foldl(
                fun(#b_function{bs=Bs}, Acc) ->
                        Uses = collect_uses(Bs),
                        scan_calls_for_callerinfo(Mod, Bs, Uses, Corpus, Acc)
                end, Acc0, Body)
      end, #{}, Mods).

scan_calls_for_callerinfo(Mod, Bs, Uses, Corpus, Acc0) ->
    lists:foldl(
      fun(#b_blk{is=Is}, A0) ->
              lists:foldl(
                fun(#b_set{dst=Dst, op=call, args=[Callee | _]}, A) ->
                        case resolve_callee(Callee, Mod) of
                            {ok, MFA} ->
                                case maps:is_key(MFA, Corpus) of
                                    true ->
                                        Destr = only_destructuring(maps:get(Dst, Uses, [])),
                                        bump_caller(MFA, Destr, A);
                                    false -> A
                                end;
                            no -> A
                        end;
                   (_, A) -> A
                end, A0, Is)
      end, Acc0, maps:values(Bs)).

bump_caller(MFA, Destr, Acc) ->
    {N, All} = maps:get(MFA, Acc, {0, true}),
    Acc#{MFA => {N + 1, All andalso Destr}}.

resolve_callee(#b_local{name=#b_literal{val=F}, arity=A}, Mod) -> {ok, {Mod, F, A}};
resolve_callee(#b_remote{mod=#b_literal{val=M}, name=#b_literal{val=F}, arity=A}, _) -> {ok, {M, F, A}};
resolve_callee(_, _) -> no.

%% ---- Phase 2: per-function allocation classification ---------------

classify_fn(Mod, #b_function{anno=Anno, bs=Bs}, Corpus, CallerInfo) ->
    MFA = mfa(Anno, Mod),
    Defs = def_map(Bs),
    UseIdx = use_index(Bs),        % Var -> [use_ctx()]
    NarrowRet = narrow_return_eligible(MFA, CallerInfo),
    lists:flatmap(
      fun(#b_blk{is=Is}) ->
              lists:filtermap(
                fun(#b_set{dst=Dst, op=Op, args=As}) ->
                        case alloc_kind(Op, As) of
                            {Cat, Words} ->
                                {Tier, Why} = fate(Dst, 0, sets:new(), UseIdx, Defs,
                                                   Corpus, NarrowRet, Mod),
                                {true, #asite{mfa=MFA, op=Op, cat=Cat, words=Words,
                                              tier=Tier, why=Why}};
                            no -> false
                        end
                end, Is)
      end, maps:values(Bs)).

%% alloc_kind(Op, Args) -> {Category, Words} | no
alloc_kind(put_tuple, Es)               -> {tuple,  length(Es) + 1};
alloc_kind(put_list, _)                 -> {cons,   2};
alloc_kind(update_record, [_F, #b_literal{val=Sz} | _]) -> {record, Sz + 1};
alloc_kind(update_record, _)            -> {record, approx};
alloc_kind(put_map, [_Op, _Src | KVs])  -> {map, map_words(KVs)};
alloc_kind(make_fun, Args)              -> {'fun', 6 + max(0, length(Args) - 1)};
alloc_kind(bs_create_bin, _)            -> {bin, unknown};
alloc_kind(_, _)                        -> no.

%% put_map builds/extends a flatmap; a fresh N-pair flatmap is ~ 2N+2 words
%% (arityval header + values + the shared keys tuple). Marked 'approx'.
map_words(KVs) -> {approx, length(KVs) + 2}.

narrow_return_eligible(MFA, CallerInfo) ->
    case maps:get(MFA, CallerInfo, undefined) of
        {N, All} when N >= 1 -> All;
        _ -> false
    end.

%% ---- the fate lattice ---------------------------------------------
%% fate(Var, Depth, Visited, UseIdx, Defs, Corpus, NarrowRet, Mod)
%%   -> {t1|t2|esc, WhyAtom}
fate(_Var, Depth, _V, _U, _D, _C, _NR, _Mod) when Depth > 1 ->
    {esc, deep_flow};
fate(Var, Depth, Visited, UseIdx, Defs, Corpus, NarrowRet, Mod) ->
    case sets:is_element(Var, Visited) of
        true -> {esc, cyclic};             % loop-carried phi: conservative
        false ->
            V2 = sets:add_element(Var, Visited),
            Uses = maps:get(Var, UseIdx, []),
            case Uses of
                [] -> {t1, dead};           % dead allocation: DCE/trivially sinkable
                _ ->
                    Rs = [classify_use(U, Depth, V2, UseIdx, Defs, Corpus, NarrowRet, Mod)
                          || U <- Uses],
                    combine(Rs)
            end
    end.

%% combine the fates of all uses: any hard escape wins; else any narrow => t2.
combine(Rs) ->
    case lists:keyfind(esc, 1, Rs) of
        {esc, W} -> {esc, W};
        false ->
            case lists:keyfind(t2, 1, Rs) of
                {t2, W} -> {t2, W};
                false -> {t1, local}
            end
    end.

%% classify_use(UseCtx, Depth, Visited, ...) -> {t1|t2|esc, Why}
%% UseCtx :: {term, ret|br|switch} | {i, #b_set{}} | {carg, Callee, ArgIdx}.
classify_use({term, ret}, _D, _V, _U, _Ds, _C, NarrowRet, _Mod) ->
    case NarrowRet of true -> {t2, narrow_ret}; false -> {esc, ret_unknown} end;
classify_use({term, br}, _D, _V, _U, _Ds, _C, _NR, _Mod) ->
    {t1, br_bool};                          % used as branch condition = a test
classify_use({term, switch}, _D, _V, _U, _Ds, _C, _NR, _Mod) ->
    {t1, switched};                         % dispatched on = local
classify_use({carg, Callee, ArgIdx}, _Depth, _Visited, _UseIdx, _Defs,
             Corpus, _NarrowRet, Mod) ->
    %% D is the actual arg at 0-based ArgIdx of a call to Callee. Narrow iff
    %% the resolvable callee only destructures that param.
    case resolve_callee(Callee, Mod) of
        {ok, MFA} ->
            case maps:find(MFA, Corpus) of
                {ok, Rec} ->
                    case sets:is_element(ArgIdx, maps:get(destruct, Rec)) of
                        true -> {t2, narrow_arg};
                        false -> {esc, call_arg}
                    end;
                error -> {esc, call_extern}   % callee outside corpus: conservative
            end;
        no -> {esc, call_dyn}
    end;
classify_use({i, #b_set{op=UOp, dst=UD, args=_UArgs}}, Depth, Visited,
             UseIdx, Defs, Corpus, NarrowRet, Mod) ->
    case local_consume_op(UOp) of
        true -> {t1, local};
        false ->
            case UOp of
                phi ->
                    %% copy/merge: follow the phi result one level down
                    tier_to_use(fate(UD, Depth + 1, Visited, UseIdx, Defs,
                                     Corpus, NarrowRet, Mod));
                make_fun ->
                    {esc, closure_capture};    % captured in a closure env
                call ->
                    {esc, call_callee};        % D appears as the callee: opaque
                call_fun ->
                    {esc, call_fun};
                _ ->
                    case is_constructor(UOp) of
                        true ->
                            %% flows into another allocation: follow one level
                            tier_to_use(fate(UD, Depth + 1, Visited, UseIdx, Defs,
                                             Corpus, NarrowRet, Mod));
                        false ->
                            {esc, {opaque_use, UOp}}   % conservative default
                    end
            end
    end.

tier_to_use({t1, _}) -> {t1, via_ctor};
tier_to_use({t2, _}) -> {t2, via_ctor};
tier_to_use({esc, W}) -> {esc, W}.

is_constructor(put_tuple) -> true;
is_constructor(put_list) -> true;
is_constructor(put_map) -> true;
is_constructor(update_record) -> true;
is_constructor(bs_create_bin) -> true;
is_constructor(_) -> false.

%% Local-consumption ops: reading fields, type tests, comparisons, the
%% pattern-match trap. Everything here keeps the value region-local.
local_consume_op(get_tuple_element) -> true;
local_consume_op(get_hd) -> true;
local_consume_op(get_tl) -> true;
local_consume_op(get_map_element) -> true;
local_consume_op(is_nonempty_list) -> true;
local_consume_op(is_tagged_tuple) -> true;
local_consume_op(match_fail) -> true;
local_consume_op(kill_try_tag) -> true;
local_consume_op(succeeded) -> true;
local_consume_op({succeeded, _}) -> true;
local_consume_op({bif, B}) -> lists:member(B, guard_bifs());
local_consume_op(_) -> false.

guard_bifs() ->
    [is_list, is_integer, is_float, is_number, is_atom, is_boolean,
     is_tuple, is_map, is_binary, is_bitstring, is_function, is_pid,
     is_port, is_reference,
     '=:=', '=/=', '==', '/=', '<', '>', '=<', '>=',
     tuple_size, map_size, map_get, is_map_key, element,
     'and', 'or', 'not', 'xor', hd, tl].

%% ---- use_index: Var -> [use_ctx()] with precise call-arg positions -
%% use_ctx() = {term, ret|br|switch} | {i, #b_set{}} | {carg, #b_local/remote, Idx}
%% For call arguments we record {carg, Callee, ArgIdx} so the t2-narrow test
%% has the exact param position. All other instruction uses record {i, Set}.
use_index(Bs) ->
    lists:foldl(
      fun(#b_blk{is=Is, last=Last}, Acc0) ->
              Acc1 = lists:foldl(fun index_inset/2, Acc0, Is),
              index_last(Last, Acc1)
      end, #{}, maps:values(Bs)).

index_inset(#b_set{op=call, args=[Callee | Actuals]}=S, Acc) ->
    %% record callee-position uses as {i,S} (opaque) and arg-position uses as {carg,...}
    Acc1 = add_use(Callee, {i, S}, Acc),
    {_, Acc2} = lists:foldl(
                  fun(#b_var{}=V, {I, A}) -> {I + 1, add_use(V, {carg, Callee, I}, A)};
                     (_, {I, A}) -> {I + 1, A}
                  end, {0, Acc1}, Actuals),
    Acc2;
index_inset(#b_set{op=phi, args=As}=S, Acc) ->
    %% phi args are {Value, FromBlock} pairs — unwrap, or the use is invisible
    lists:foldl(fun({#b_var{}=V, _Blk}, A) -> add_use(V, {i, S}, A);
                   (_, A) -> A
                end, Acc, As);
index_inset(#b_set{args=As}=S, Acc) ->
    lists:foldl(fun(#b_var{}=V, A) -> add_use(V, {i, S}, A);
                   (_, A) -> A
                end, Acc, As).

index_last(#b_ret{arg=#b_var{}=V}, Acc) -> add_use(V, {term, ret}, Acc);
index_last(#b_br{bool=#b_var{}=V}, Acc) -> add_use(V, {term, br}, Acc);
index_last(#b_switch{arg=#b_var{}=V}, Acc) -> add_use(V, {term, switch}, Acc);
index_last(_, Acc) -> Acc.

add_use(#b_var{}=V, Ctx, Acc) -> Acc#{V => [Ctx | maps:get(V, Acc, [])]};
add_use(_, _, Acc) -> Acc.

%% ---- collect_uses (for callee/caller destructuring signatures) -----
%% Var -> [Op] (op the var is an argument of). Used by only_destructuring and
%% param_destructured, matching elimscan's semantics.
collect_uses(Bs) ->
    lists:foldl(
      fun(#b_blk{is=Is, last=Last}, Acc0) ->
              Acc1 = lists:foldl(
                       fun(#b_set{op=phi, args=As}, A) ->
                               %% unwrap {Value, FromBlock} pairs
                               add_ops(phi, [V || {V, _} <- As], A);
                          (#b_set{op=Op, args=As}, A) -> add_ops(Op, As, A)
                       end, Acc0, Is),
              add_ops('$last', last_args(Last), Acc1)
      end, #{}, maps:values(Bs)).

last_args(#b_ret{arg=A}) -> [A];
last_args(#b_br{bool=B}) -> [B];
last_args(#b_switch{arg=A}) -> [A];
last_args(_) -> [].

add_ops(Op, As, Acc) ->
    lists:foldl(fun(#b_var{}=V, A) -> A#{V => [Op | maps:get(V, A, [])]};
                   (_, A) -> A
                end, Acc, As).

only_destructuring([]) -> false;
only_destructuring(Ops) ->
    lists:all(fun(get_tuple_element) -> true;
                 (get_hd) -> true;
                 (get_tl) -> true;
                 (get_map_element) -> true;
                 ({bif, B}) -> lists:member(B, guard_bifs());
                 (is_nonempty_list) -> true;
                 (is_tagged_tuple) -> true;
                 ({succeeded, _}) -> true;
                 (match_fail) -> true;
                 (_) -> false
              end, Ops).

%% param_destructured(Args, Bs) -> set of ParamIdx the fn only destructures.
param_destructured(Args, Bs) ->
    PIdx = maps:from_list(lists:zip(Args, lists:seq(0, length(Args) - 1))),
    Uses = collect_uses(Bs),
    Ps = maps:keys(PIdx),
    Destructed = [maps:get(P, PIdx) || P <- Ps,
                                      only_destructuring(maps:get(P, Uses, []))],
    sets:from_list(Destructed).

%% ---- misc SSA helpers (shared shape with elimscan) -----------------
def_map(Bs) ->
    lists:foldl(
      fun(#b_blk{is=Is}, Acc) ->
              lists:foldl(fun(#b_set{dst=#b_var{}=D}=S, A) -> A#{D => S};
                             (_, A) -> A
                          end, Acc, Is)
      end, #{}, maps:values(Bs)).

mfa(Anno, Mod) ->
    case maps:get(func_info, Anno, undefined) of
        {M, F, A} -> {M, F, A};
        _ -> {Mod, '$unknown', 0}
    end.

%% ---- debug helper --------------------------------------------------
dump_fn(File, FnName) ->
    Opts = [to_core, binary, no_spawn_compiler_process, return_errors,
            {i, filename:dirname(File)}],
    {ok, _M, Core} = compile:file(File, Opts),
    {ok, SSA0, _} = beam_core_to_ssa:module(Core, []),
    {ok, #b_module{name=Mod, body=Body}} = beam_ssa_opt:module(SSA0, []),
    Corpus = build_corpus([{Mod, Body}]),
    CallerInfo = build_caller_info([{Mod, Body}], Corpus),
    Want = list_to_atom(FnName),
    [begin
         io:format("== ~p/~p ==~n", [element(2, mfa(A, Mod)), element(3, mfa(A, Mod))]),
         [io:format("  ~p~n", [S]) || S <- classify_fn(Mod, Fn, Corpus, CallerInfo)]
     end || #b_function{anno=A}=Fn <- Body, element(2, mfa(A, Mod)) =:= Want],
    ok.
