%% elimscan.erl — M0.1 eliminability classifier for the T2-Full JIT plan.
%%
%% Given a set of .erl source files, compiles each to *optimized* SSA
%% (core_to_ssa -> beam_ssa_opt, i.e. post-type-inference, the same
%% representation the loaded-BEAM Type chunk serializes), then for every
%% CALL SITE classifies whether cross-module inlining would ELIMINATE
%% work, into the categories of PLAN/T2FULL/03_optimizer.md §2.1:
%%
%%   guard-subsumable | constant-args (+ literal-fun) |
%%   construct/deconstruct | loop-unlock | none
%%
%% Sites classify into MULTIPLE categories; all are recorded. Opaque
%% sites (dynamic remote / fun / apply / callee not in corpus) are
%% counted separately, never as eliminable and never as 'none'.
%%
%% Design rules (from the plan, honest-methodology section):
%%  * CONSERVATIVE: when unsure, classify 'none'. Caller-side type facts
%%    come only from sound sources — a variable's defining op (holds
%%    everywhere, SSA) and dominating type-test guards (sound RPO
%%    dataflow with back-edge widening). Guard subsumption counts only
%%    KNOWN-TRUE subsumptions (caller type <= callee-tested type), not
%%    dead-clause elimination.
%%  * Cross-module resolution uses the whole compiled corpus (that is
%%    the point — cross-module inlining). Callees outside the corpus are
%%    opaque.
%%
%% Emits a consultable per-function stats dump; weighting/aggregation is
%% done by the driver (elimdrv.erl).

-module(elimscan).
-export([main/1, scan/2]).
-export([dbg_guards/1, dbg_env/2]).

-include_lib("compiler/src/beam_ssa.hrl").

%% Per-function stats record, emitted as a term. Fields prefixed 'r'
%% count REMOTE (cross-module) sites only — the M0.1 P3 pool, since
%% intra-module guard subsumption is already harvested by whole-module
%% optimization. Unprefixed fields count all resolvable sites.
-record(fstat, {mfa,
                nsites = 0,          % resolvable, non-opaque call sites (local+remote)
                opaque = 0,          % dynamic/fun/apply/out-of-corpus sites
                guard = 0,           % sites with >=1 known-true subsumed guard
                guardtests = 0,      % total subsumed guard tests (sum over sites)
                const = 0,           % sites with >=1 literal (non-fun) arg (LIBERAL upper bound)
                consteff = 0,        % sites with a literal arg on a DISPATCHED param (folds a branch)
                litfun = 0,          % sites with >=1 literal-fun arg (precise)
                cdcon = 0,           % construct/deconstruct sites (precise)
                loop = 0,            % loop-unlock sites (callee self-tail AND caller recursive)
                coreelim = 0,        % sites in >=1 of {guard,consteff,litfun,cdcon}
                precise = 0,         % sites in >=1 of {guard,litfun,cdcon} (excludes const)
                none = 0,            % resolvable sites in no category
                elim = 0,            % sites in >=1 elim category incl. loop (union)
                %% remote-only mirror:
                rsites = 0, rguard = 0, rconsteff = 0, rlitfun = 0, rcdcon = 0,
                rloop = 0, rcore = 0, rprecise = 0, rnone = 0, relim = 0}).

%% ---- escript entry -------------------------------------------------
%% main([OutFile, IncSpec | Files])
%%   IncSpec = comma-separated include dirs.
main([OutFile, IncSpec | Files]) ->
    Incs = string:tokens(IncSpec, ","),
    Stats = scan(Files, Incs),
    {ok, Fd} = file:open(OutFile, [write]),
    io:format(Fd, "%% elimscan per-function stats; ~p functions~n", [length(Stats)]),
    [io:format(Fd, "~p.~n", [S]) || S <- Stats],
    ok = file:close(Fd),
    io:format("wrote ~p function stats to ~s~n", [length(Stats), OutFile]),
    halt(0).

%% ---- driver-callable API ------------------------------------------
%% Returns list of #fstat{} as plain tuples.
scan(Files, Incs) ->
    Opts = [to_core, binary, no_spawn_compiler_process, return_errors]
        ++ [{i, I} || I <- Incs],
    %% Phase 1: compile everything, build corpus function index.
    Mods = lists:foldl(fun(F, Acc) -> load_module(F, Opts, Acc) end, [], Files),
    %% Corpus = #{{M,F,A} => FnRec}, FnRec = #{guards, retshape, selftail, module}
    Corpus = build_corpus(Mods),
    %% Phase 2: classify each function's call sites.
    lists:flatmap(fun({Mod, Body}) ->
                          [classify_fn(Mod, Fn, Corpus) || Fn <- Body]
                  end, Mods).

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

dbg_guards(#b_function{args=Args, bs=Bs}) -> entry_guards(Args, Bs).
dbg_env(#b_function{bs=Bs}, Blk) ->
    Defs = def_map(Bs),
    Env = dataflow(Bs, Defs),
    maps:get(Blk, Env, undefined).

%% ---- Phase 1: corpus signatures -----------------------------------

build_corpus(Mods) ->
    lists:foldl(
      fun({Mod, Body}, Acc0) ->
              lists:foldl(
                fun(#b_function{anno=Anno, args=Args, bs=Bs}, Acc) ->
                        MFA = mfa(Anno, Mod),
                        Rec = #{guards   => entry_guards(Args, Bs),
                                dispatch => entry_dispatch(Args, Bs),
                                retshape => return_shape(Bs),
                                selftail => self_tail(MFA, Mod, Bs),
                                destruct => param_destructured(Args, Bs),
                                module   => Mod},
                        Acc#{MFA => Rec}
                end, Acc0, Body)
      end, #{}, Mods).

mfa(Anno, Mod) ->
    case maps:get(func_info, Anno, undefined) of
        {M, F, A} -> {M, F, A};
        _ -> {Mod, '$unknown', 0}
    end.

%% param index map: Var -> index (0-based) of the function parameters.
param_index(Args) ->
    maps:from_list(lists:zip(Args, lists:seq(0, length(Args) - 1))).

%% Entry-guard signature: #{ParamIdx => [PosType]} — the type tests
%% applied to each parameter in the entry "dispatch region" (blocks
%% reachable from block 0 through guard-only blocks, before real work).
entry_guards(Args, Bs) ->
    PIdx = param_index(Args),
    Region = guard_region(Bs),
    lists:foldl(
      fun(Blk, Acc) ->
              #b_blk{is=Is} = maps:get(Blk, Bs),
              lists:foldl(
                fun(#b_set{op=Op, args=OpArgs}, A2) ->
                        case type_test_of(Op, OpArgs) of
                            {Var, PosType} ->
                                case maps:find(Var, PIdx) of
                                    {ok, I} ->
                                        Prev = maps:get(I, A2, []),
                                        case lists:member(PosType, Prev) of
                                            true -> A2;
                                            false -> A2#{I => [PosType | Prev]}
                                        end;
                                    error -> A2
                                end;
                            none -> A2
                        end
                end, Acc, Is)
      end, #{}, Region).

%% entry_dispatch: set of param indices the callee DISPATCHES on in its
%% entry region — appears in a type test, a shape match (get_tuple_element,
%% is_tagged_tuple, is_nonempty_list), a comparison, or a switch. A literal
%% argument landing on such a param provably folds a branch in the callee.
entry_dispatch(Args, Bs) ->
    PIdx = param_index(Args),
    Region = guard_region(Bs),
    lists:foldl(
      fun(Blk, Acc0) ->
              #b_blk{is=Is, last=Last} = maps:get(Blk, Bs),
              Acc1 = lists:foldl(
                       fun(#b_set{op=Op, args=OpArgs}, A) ->
                               case dispatch_op(Op) of
                                   true -> mark_params(OpArgs, PIdx, A);
                                   false -> A
                               end
                       end, Acc0, Is),
              case Last of
                  #b_switch{arg=SwArg} -> mark_params([SwArg], PIdx, Acc1);
                  _ -> Acc1
              end
      end, sets:new(), Region).

dispatch_op({bif, B}) -> lists:member(B, guard_bifs());
dispatch_op(is_nonempty_list) -> true;
dispatch_op(is_tagged_tuple) -> true;
dispatch_op(get_tuple_element) -> true;
dispatch_op(_) -> false.

mark_params(OpArgs, PIdx, Acc) ->
    lists:foldl(fun(#b_var{}=V, A) ->
                        case maps:find(V, PIdx) of
                            {ok, I} -> sets:add_element(I, A);
                            error -> A
                        end;
                   (_, A) -> A
                end, Acc, OpArgs).

%% A block is a "guard block" if all its non-terminator instructions are
%% dispatch-ish (type tests, tuple_size, comparisons, succeeded, phi).
%% The region is blocks reachable from 0 through guard blocks (capped).
guard_region(Bs) ->
    guard_region([0], Bs, sets:new(), 0).

guard_region([], _Bs, Seen, _N) -> sets:to_list(Seen);
guard_region(_Wl, _Bs, Seen, N) when N > 40 -> sets:to_list(Seen);
guard_region([B | Wl], Bs, Seen, N) ->
    case sets:is_element(B, Seen) orelse not maps:is_key(B, Bs) of
        true -> guard_region(Wl, Bs, Seen, N);
        false ->
            #b_blk{is=Is, last=Last} = maps:get(B, Bs),
            case is_guard_block(Is) of
                true ->
                    Seen1 = sets:add_element(B, Seen),
                    Succs = successors(Last),
                    guard_region(Succs ++ Wl, Bs, Seen1, N + 1);
                false ->
                    guard_region(Wl, Bs, Seen, N)
            end
    end.

is_guard_block(Is) ->
    lists:all(fun(#b_set{op=Op}) -> is_guard_op(Op) end, Is).

is_guard_op({bif, B}) -> lists:member(B, guard_bifs());
is_guard_op(is_nonempty_list) -> true;
is_guard_op(is_tagged_tuple) -> true;
is_guard_op({succeeded, _}) -> true;
is_guard_op(phi) -> true;
is_guard_op(_) -> false.

guard_bifs() ->
    [is_list, is_integer, is_float, is_number, is_atom, is_boolean,
     is_tuple, is_map, is_binary, is_bitstring, is_function, is_pid,
     is_port, is_reference,
     '=:=', '=/=', '==', '/=', '<', '>', '=<', '>=',
     tuple_size, map_get, is_map_key, element, 'and', 'or', 'not'].

%% Recognize a type test op; return {Var, PosType} where PosType is the
%% abstract type asserted about Var when the test succeeds, else 'none'.
type_test_of({bif, is_list}, [V])      -> tv(V, list);
type_test_of({bif, is_integer}, [V])   -> tv(V, integer);
type_test_of({bif, is_float}, [V])     -> tv(V, float);
type_test_of({bif, is_number}, [V])    -> tv(V, number);
type_test_of({bif, is_atom}, [V])      -> tv(V, atom);
type_test_of({bif, is_boolean}, [V])   -> tv(V, boolean);
type_test_of({bif, is_tuple}, [V])     -> tv(V, tuple);
type_test_of({bif, is_map}, [V])       -> tv(V, map);
type_test_of({bif, is_binary}, [V])    -> tv(V, binary);
type_test_of({bif, is_bitstring}, [V]) -> tv(V, bitstring);
type_test_of({bif, is_function}, [V])  -> tv(V, fun_);
type_test_of({bif, is_function}, [V,_])-> tv(V, fun_);
type_test_of({bif, is_pid}, [V])       -> tv(V, pid);
type_test_of({bif, is_port}, [V])      -> tv(V, port);
type_test_of({bif, is_reference}, [V]) -> tv(V, reference);
type_test_of(is_nonempty_list, [V])    -> tv(V, cons);
type_test_of(is_tagged_tuple, [V, #b_literal{val=A}, _Tag]) -> tv(V, {tuple, A});
type_test_of(_, _) -> none.

tv(#b_var{}=V, T) -> {V, T};
tv(_, _) -> none.

successors(#b_br{succ=S, fail=F}) -> [S, F];
successors(#b_switch{fail=F, list=L}) -> [F | [Lbl || {_, Lbl} <- L]];
successors(#b_ret{}) -> [].

%% return_shape(Bs) -> {construct, tuple} | {construct, cons} | other
%% "construct" means every return point returns a value freshly built by
%% put_tuple / put_list in this function (incl. phi of such).
return_shape(Bs) ->
    Defs = def_map(Bs),
    Rets = [V || #b_blk{last=#b_ret{arg=V}} <- maps:values(Bs)],
    case Rets of
        [] -> other;
        _ ->
            Kinds = [ret_val_kind(V, Defs, 0) || V <- Rets],
            case lists:usort(Kinds) of
                [tuple] -> {construct, tuple};
                [cons]  -> {construct, cons};
                _ -> other
            end
    end.

ret_val_kind(_, _, D) when D > 3 -> mixed;
ret_val_kind(#b_literal{}, _, _) -> mixed;
ret_val_kind(#b_var{}=V, Defs, D) ->
    case maps:find(V, Defs) of
        {ok, #b_set{op=put_tuple}} -> tuple;
        {ok, #b_set{op=put_list}} -> cons;
        {ok, #b_set{op=phi, args=Ins}} ->
            Ks = [ret_val_kind(IV, Defs, D + 1) || {IV, _} <- Ins],
            case lists:usort(Ks) of
                [tuple] -> tuple;
                [cons] -> cons;
                _ -> mixed
            end;
        _ -> mixed
    end.

%% self_tail(MFA, Mod, Bs) -> boolean(): does the function tail-call
%% itself? Detected as a call to itself whose dst is a return value.
self_tail({_M, F, A}, Mod, Bs) ->
    RetVars = sets:from_list([V || #b_blk{last=#b_ret{arg=#b_var{}=V}} <- maps:values(Bs)]),
    lists:any(
      fun(#b_blk{is=Is}) ->
              lists:any(
                fun(#b_set{dst=Dst, op=call, args=[Callee | _]}) ->
                        is_self(Callee, Mod, F, A) andalso sets:is_element(Dst, RetVars);
                   (_) -> false
                end, Is)
      end, maps:values(Bs)).

is_self(#b_local{name=#b_literal{val=F}, arity=A}, _Mod, F, A) -> true;
is_self(#b_remote{mod=#b_literal{val=M}, name=#b_literal{val=F}, arity=A}, M, F, A) -> true;
is_self(_, _, _, _) -> false.

%% param_destructured(Args, Bs) -> set of ParamIdx that the function
%% only ever *destructures* (get_tuple_element / get_hd / get_tl /
%% type-tests), used for construct/deconstruct (b).
param_destructured(Args, Bs) ->
    PIdx = param_index(Args),
    Uses = collect_uses(Bs),
    Ps = maps:keys(PIdx),
    Destructed = [maps:get(P, PIdx) || P <- Ps,
                                      only_destructuring(maps:get(P, Uses, []))],
    sets:from_list(Destructed).

%% Map Var -> [Op] where the var appears as an argument.
collect_uses(Bs) ->
    lists:foldl(
      fun(#b_blk{is=Is, last=Last}, Acc0) ->
              Acc1 = lists:foldl(
                       fun(#b_set{op=Op, args=As}, A) -> add_uses(Op, As, A) end,
                       Acc0, Is),
              add_uses('$last', last_args(Last), Acc1)
      end, #{}, maps:values(Bs)).

last_args(#b_ret{arg=A}) -> [A];
last_args(#b_br{bool=B}) -> [B];
last_args(#b_switch{arg=A}) -> [A];
last_args(_) -> [].

add_uses(Op, As, Acc) ->
    lists:foldl(fun(#b_var{}=V, A) ->
                        A#{V => [Op | maps:get(V, A, [])]};
                   (_, A) -> A
                end, Acc, As).

only_destructuring([]) -> false;   % unused param — not a destructure signal
only_destructuring(Ops) ->
    lists:all(fun(get_tuple_element) -> true;
                 (get_hd) -> true;
                 (get_tl) -> true;
                 ({bif, B}) -> lists:member(B, guard_bifs());
                 (is_nonempty_list) -> true;
                 (is_tagged_tuple) -> true;
                 ({succeeded, _}) -> true;
                 %% badmatch/clause-failure trap on the value: part of the
                 %% pattern match, not real consumption. Inlining a callee
                 %% that always returns the matched shape makes it dead.
                 (match_fail) -> true;
                 (_) -> false
              end, Ops).

%% ---- Phase 2: per-function classification -------------------------

classify_fn(Mod, #b_function{anno=Anno, args=_Args, bs=Bs}, Corpus) ->
    MFA = mfa(Anno, Mod),
    Defs = def_map(Bs),
    DefTypes = def_types(Bs),           % Var -> abstract type from its def op
    Uses = collect_uses(Bs),
    Env = dataflow(Bs, Defs),           % BlockId -> #{Var => refined type}
    CallerRec = caller_recursive(MFA, Mod, Bs),  % caller is itself a loop/recursion
    S0 = #fstat{mfa = MFA},
    Order = maps:keys(Bs),
    lists:foldl(
      fun(Blk, S) ->
              #b_blk{is=Is} = maps:get(Blk, Bs),
              BlkEnv = maps:get(Blk, Env, #{}),
              lists:foldl(
                fun(#b_set{dst=Dst, op=Op, args=OpArgs}, Sa) ->
                        case call_target(Op, OpArgs) of
                            {Kind, Spec, ActualArgs} when Kind =:= local; Kind =:= remote ->
                                classify_site(Sa, Mod, {Kind, Spec}, ActualArgs,
                                              Dst, BlkEnv, DefTypes, Defs, Uses,
                                              CallerRec, Corpus);
                            {opaque, _Why} ->
                                Sa#fstat{opaque = Sa#fstat.opaque + 1};
                            no ->
                                Sa
                        end
                end, S, Is)
      end, S0, Order).

%% Identify a call site. Returns:
%%   {resolved, CalleeMFA, ActualArgVars} | {opaque, Why} | no
%% Compiler-generated exception-raise trampolines (clause-failure traps,
%% guard failures) are not inlining candidates: skip them entirely
%% (neither a site nor opaque). Documented approximation.
call_target(call, [#b_remote{mod=#b_literal{val=erlang}, name=#b_literal{val=N}} | _])
  when N =:= error; N =:= exit; N =:= throw; N =:= nif_error; N =:= raise -> no;
call_target(call, [#b_local{name=#b_literal{val=F}, arity=A} | Rest]) ->
    {local, {F, A}, Rest};
call_target(call, [#b_remote{mod=#b_literal{val=M}, name=#b_literal{val=F}, arity=A} | Rest]) ->
    {remote, {M, F, A}, Rest};
call_target(call, [#b_remote{} | _]) ->
    {opaque, dyn_remote};
call_target(call, [#b_var{} | _]) ->
    {opaque, dyn_call};
call_target(call_fun, _) -> {opaque, call_fun};
call_target({bif, apply}, _) -> {opaque, apply};
call_target(_, _) -> no.

classify_site(Sa, Mod, TargetSpec, ActualArgs, Dst, BlkEnv, DefTypes, Defs, Uses,
              CallerRec, Corpus) ->
    MFA = case TargetSpec of
              {local, {F, A}} -> {Mod, F, A};
              {remote, {M, F, A}} -> {M, F, A}
          end,
    case maps:find(MFA, Corpus) of
        error ->
            %% Callee not in corpus — treat as opaque (conservative).
            Sa#fstat{opaque = Sa#fstat.opaque + 1};
        {ok, Rec} ->
            ArgTypes = [type_at(Arg, BlkEnv, DefTypes) || Arg <- ActualArgs],
            %% 1. guard-subsumable
            {IsGuard, NSub} = guard_subsumable(ArgTypes, maps:get(guards, Rec)),
            %% 2. constant-args (liberal + effective) + literal-fun
            {IsConst, IsConstEff, IsLitFun} =
                const_args(ActualArgs, Defs, maps:get(dispatch, Rec)),
            %% 3. construct/deconstruct
            IsCD = construct_deconstruct(MFA, Rec, Dst, Uses, ActualArgs, Defs, Corpus),
            %% 4. loop-unlock: callee self-tail-recursive AND site sits in a
            %%    loop (caller is itself recursive) — the plan's two conditions.
            IsLoop = maps:get(selftail, Rec) andalso CallerRec,
            Core = IsGuard orelse IsConstEff orelse IsLitFun orelse IsCD,
            Prec = IsGuard orelse IsLitFun orelse IsCD,
            Elim = Core orelse IsLoop,
            IsRemote = element(1, TargetSpec) =:= remote,
            S1 = Sa#fstat{
              nsites     = Sa#fstat.nsites + 1,
              guard      = Sa#fstat.guard + b2i(IsGuard),
              guardtests = Sa#fstat.guardtests + NSub,
              const      = Sa#fstat.const + b2i(IsConst),
              consteff   = Sa#fstat.consteff + b2i(IsConstEff),
              litfun     = Sa#fstat.litfun + b2i(IsLitFun),
              cdcon      = Sa#fstat.cdcon + b2i(IsCD),
              loop       = Sa#fstat.loop + b2i(IsLoop),
              coreelim   = Sa#fstat.coreelim + b2i(Core),
              precise    = Sa#fstat.precise + b2i(Prec),
              none       = Sa#fstat.none + b2i(not Elim),
              elim       = Sa#fstat.elim + b2i(Elim)},
            case IsRemote of
                false -> S1;
                true ->
                    S1#fstat{
                      rsites    = S1#fstat.rsites + 1,
                      rguard    = S1#fstat.rguard + b2i(IsGuard),
                      rconsteff = S1#fstat.rconsteff + b2i(IsConstEff),
                      rlitfun   = S1#fstat.rlitfun + b2i(IsLitFun),
                      rcdcon    = S1#fstat.rcdcon + b2i(IsCD),
                      rloop     = S1#fstat.rloop + b2i(IsLoop),
                      rcore     = S1#fstat.rcore + b2i(Core),
                      rprecise  = S1#fstat.rprecise + b2i(Prec),
                      rnone     = S1#fstat.rnone + b2i(not Elim),
                      relim     = S1#fstat.relim + b2i(Elim)}
            end
    end.

%% caller_recursive: does the caller call itself anywhere (any position)?
%% Proxy for "the call site sits in/feeds a loop".
caller_recursive({_M, F, A}, Mod, Bs) ->
    lists:any(
      fun(#b_blk{is=Is}) ->
              lists:any(
                fun(#b_set{op=call, args=[Callee | _]}) ->
                        is_self(Callee, Mod, F, A);
                   (_) -> false
                end, Is)
      end, maps:values(Bs)).

b2i(true) -> 1;
b2i(false) -> 0.

%% guard_subsumable: does the caller prove, for some callee-tested
%% parameter, a type that is a subtype of (subsumes) the callee's test?
%% Returns {Bool, NumberOfSubsumedTests}. KNOWN-TRUE only.
guard_subsumable(_ArgTypes, Guards) when map_size(Guards) =:= 0 ->
    {false, 0};
guard_subsumable(ArgTypes, Guards) ->
    N = maps:fold(
          fun(Idx, TestTypes, Acc) ->
                  case nth0(Idx, ArgTypes) of
                      {ok, AType} when AType =/= any ->
                          Acc + length([T || T <- TestTypes, subtype(AType, T)]);
                      _ -> Acc
                  end
          end, 0, Guards),
    {N > 0, N}.

nth0(I, L) when I >= 0, I < length(L) -> {ok, lists:nth(I + 1, L)};
nth0(_, _) -> error.

%% const_args: returns {AnyLiteral, EffectiveLiteral, LiteralFun}.
%%  AnyLiteral      — any literal (non-fun) arg (liberal upper bound)
%%  EffectiveLiteral— a literal (non-fun) arg on a DISPATCHED param (folds a branch)
%%  LiteralFun      — a literal fun / make_fun arg (precise; collapses call_fun)
const_args(ActualArgs, Defs, Dispatch) ->
    {_, C, CE, LF} =
        lists:foldl(
          fun(#b_literal{val=V}, {I, C, CE, LF}) ->
                  case is_function(V) of
                      true -> {I+1, C, CE, true};
                      false ->
                          CE1 = CE orelse sets:is_element(I, Dispatch),
                          {I+1, true, CE1, LF}
                  end;
             (#b_var{}=Var, {I, C, CE, LF}) ->
                  case maps:find(Var, Defs) of
                      {ok, #b_set{op=make_fun}} -> {I+1, C, CE, true};
                      {ok, #b_set{op={bif, make_fun}}} -> {I+1, C, CE, true};
                      _ -> {I+1, C, CE, LF}
                  end;
             (_, {I, C, CE, LF}) -> {I+1, C, CE, LF}
          end, {0, false, false, false}, ActualArgs),
    {C, CE, LF}.

%% construct_deconstruct:
%%  (a) callee returns a freshly-built tuple/cons AND caller uses the
%%      result only via get_tuple_element/get_hd/get_tl/type-tests; or
%%  (b) an actual arg is locally constructed (put_tuple/put_list), used
%%      only by this call, AND the callee only destructures that param.
construct_deconstruct(_MFA, Rec, Dst, Uses, ActualArgs, Defs, _Corpus) ->
    A = case maps:get(retshape, Rec) of
            {construct, _} ->
                DstUses = maps:get(Dst, Uses, []),
                DstUses =/= [] andalso only_destructuring(DstUses);
            other -> false
        end,
    B = case A of
            true -> true;
            false ->
                Destruct = maps:get(destruct, Rec),
                arg_side_cd(ActualArgs, Defs, Uses, Destruct, 0)
        end,
    A orelse B.

arg_side_cd([], _Defs, _Uses, _Destruct, _I) -> false;
arg_side_cd([Arg | Rest], Defs, Uses, Destruct, I) ->
    Hit = case Arg of
              #b_var{} = V ->
                  Constructed = case maps:find(V, Defs) of
                                    {ok, #b_set{op=put_tuple}} -> true;
                                    {ok, #b_set{op=put_list}} -> true;
                                    _ -> false
                                end,
                  SingleUse = length(maps:get(V, Uses, [])) =< 1,
                  Constructed andalso SingleUse andalso sets:is_element(I, Destruct);
              _ -> false
          end,
    case Hit of
        true -> true;
        false -> arg_side_cd(Rest, Defs, Uses, Destruct, I + 1)
    end.

%% ---- SSA helpers ---------------------------------------------------

def_map(Bs) ->
    lists:foldl(
      fun(#b_blk{is=Is}, Acc) ->
              lists:foldl(fun(#b_set{dst=#b_var{}=D}=S, A) -> A#{D => S};
                             (_, A) -> A
                          end, Acc, Is)
      end, #{}, maps:values(Bs)).

%% def_types: Var -> abstract type implied by its defining op (sound
%% globally, since a def dominates all uses and fixes the value type).
def_types(Bs) ->
    lists:foldl(
      fun(#b_blk{is=Is}, Acc) ->
              lists:foldl(fun(#b_set{dst=#b_var{}=D}=S, A) ->
                                  case def_type(S) of
                                      any -> A;
                                      T -> A#{D => T}
                                  end;
                             (_, A) -> A
                          end, Acc, Is)
      end, #{}, maps:values(Bs)).

def_type(#b_set{op=put_tuple, args=As}) -> {tuple, length(As)};
def_type(#b_set{op=put_list}) -> cons;
def_type(#b_set{op=put_map}) -> map;
def_type(#b_set{op=make_fun}) -> fun_;
def_type(#b_set{op=bs_create_bin}) -> bitstring;
def_type(#b_set{op=bs_init}) -> bitstring;
def_type(#b_set{op=is_nonempty_list}) -> boolean;
def_type(#b_set{op=is_tagged_tuple}) -> boolean;
def_type(#b_set{op={succeeded, _}}) -> boolean;
def_type(#b_set{op={bif, B}}) -> bif_result_type(B);
def_type(#b_set{op=Op, anno=Anno}) when Op =:= call; Op =:= call_fun ->
    case maps:get(result_type, Anno, undefined) of
        undefined -> any;
        T -> from_beam_type(T)
    end;
def_type(#b_set{anno=Anno}) ->
    case maps:get(result_type, Anno, undefined) of
        undefined -> any;
        T -> from_beam_type(T)
    end.

bif_result_type(B) ->
    case lists:member(B, [is_list, is_integer, is_float, is_number, is_atom,
                          is_boolean, is_tuple, is_map, is_binary, is_bitstring,
                          is_function, is_pid, is_port, is_reference,
                          '=:=', '=/=', '==', '/=', '<', '>', '=<', '>=',
                          'and', 'or', 'not', 'xor', is_map_key]) of
        true -> boolean;
        false ->
            case B of
                '+' -> number; '-' -> number; '*' -> number;
                'div' -> integer; 'rem' -> integer;
                'band' -> integer; 'bor' -> integer; 'bxor' -> integer;
                'bsl' -> integer; 'bsr' -> integer; 'bnot' -> integer;
                '/' -> float;
                abs -> number; trunc -> integer; round -> integer;
                float -> float; ceil -> integer; floor -> integer;
                length -> integer; tuple_size -> integer;
                byte_size -> integer; bit_size -> integer;
                size -> integer;
                _ -> any
            end
    end.

%% Map a beam_ssa type term (from result_type/arg_types annos) into the
%% local abstract lattice. Conservative — unknown shapes -> any.
from_beam_type({t_integer, _}) -> integer;
from_beam_type({t_float, _}) -> float;
from_beam_type({t_number, _}) -> number;
from_beam_type(number) -> number;
from_beam_type({t_atom, _}) -> atom;
from_beam_type({t_bitstring, U, _}) when U rem 8 =:= 0 -> binary;
from_beam_type({t_bitstring, _, _}) -> bitstring;
from_beam_type({t_bitstring, _}) -> bitstring;
from_beam_type({t_tuple, Sz, true, _}) when is_integer(Sz), Sz > 0 -> {tuple, Sz};
from_beam_type({t_tuple, _, _, _}) -> tuple;
from_beam_type({t_cons, _, _}) -> cons;
from_beam_type({t_list, _, _}) -> list;
from_beam_type(nil) -> nil;
from_beam_type({t_map, _, _}) -> map;
from_beam_type({t_fun, _, _, _}) -> fun_;
from_beam_type({t_fun, _, _}) -> fun_;
from_beam_type(pid) -> pid;
from_beam_type(port) -> port;
from_beam_type(reference) -> reference;
from_beam_type(_) -> any.

%% type_at(Arg, BlkEnv, DefTypes): most-specific sound type of Arg here.
type_at(#b_literal{val=V}, _Env, _Defs) -> lit_type(V);
type_at(#b_var{}=Var, Env, Defs) ->
    Refined = maps:get(Var, Env, any),
    DefT = maps:get(Var, Defs, any),
    most_specific(Refined, DefT);
type_at(_, _, _) -> any.

lit_type(V) when is_integer(V) -> integer;
lit_type(V) when is_float(V) -> float;
lit_type([]) -> nil;
lit_type([_|_]) -> cons;
lit_type(V) when is_boolean(V) -> boolean;
lit_type(V) when is_atom(V) -> atom;
lit_type(V) when is_tuple(V) -> {tuple, tuple_size(V)};
lit_type(V) when is_map(V) -> map;
lit_type(V) when is_bitstring(V), bit_size(V) rem 8 =:= 0 -> binary;
lit_type(V) when is_bitstring(V) -> bitstring;
lit_type(V) when is_function(V) -> fun_;
lit_type(_) -> any.

most_specific(any, T) -> T;
most_specific(T, any) -> T;
most_specific(A, B) ->
    case subtype(A, B) of
        true -> A;
        false -> case subtype(B, A) of true -> B; false -> A end
    end.

%% ---- abstract type lattice ----------------------------------------

subtype(T, T) -> true;
subtype(_, any) -> true;
subtype(none, _) -> true;
subtype(cons, list) -> true;
subtype(nil, list) -> true;
subtype({tuple, _}, tuple) -> true;
subtype(integer, number) -> true;
subtype(float, number) -> true;
subtype(boolean, atom) -> true;
subtype(binary, bitstring) -> true;
subtype(_, _) -> false.

%% ---- sound forward dataflow for dominating-guard refinement -------
%% Env(BlockId) -> #{Var => refined type}. Refinements added only on
%% the SUCCESS edge of a type-test branch. Blocks with an unprocessed
%% (back-edge) predecessor get the empty env (sound widening).

dataflow(Bs, Defs) ->
    Order = rpo(Bs),
    Preds = pred_map(Bs),
    lists:foldl(
      fun(B, EnvAcc) ->
              PS = maps:get(B, Preds, []),
              Entry = case PS of
                          [] -> #{};
                          _ ->
                              case lists:all(fun(P) -> maps:is_key(P, EnvAcc) end, PS) of
                                  false -> #{};   % back-edge → widen to any
                                  true ->
                                      EEs = [out_env(P, B, EnvAcc, Bs, Defs) || P <- PS],
                                      intersect_join(EEs)
                              end
                      end,
              EnvAcc#{B => Entry}
      end, #{}, Order).

out_env(P, B, EnvAcc, Bs, Defs) ->
    Base = maps:get(P, EnvAcc, #{}),
    #b_blk{last=Last} = maps:get(P, Bs),
    case Last of
        #b_br{bool=#b_var{}=Bool, succ=S, fail=F} ->
            case maps:find(Bool, Defs) of
                {ok, #b_set{op=Op, args=OpArgs}} ->
                    case type_test_of(Op, OpArgs) of
                        {#b_var{}=V, PosType} when B =:= S, B =/= F ->
                            add_fact(V, PosType, Base);
                        _ -> Base
                    end;
                _ -> Base
            end;
        #b_switch{arg=#b_var{}=V, list=L} ->
            case [Lit || {#b_literal{val=Lit}, Lbl} <- L, Lbl =:= B] of
                [Lit] -> add_fact(V, lit_type(Lit), Base);
                _ -> Base
            end;
        _ -> Base
    end.

add_fact(V, T, Env) ->
    Cur = maps:get(V, Env, any),
    Env#{V => most_specific(Cur, T)}.

%% intersect_join: keep only vars present in every env; type = join.
intersect_join([E]) -> E;
intersect_join([E | Es]) ->
    lists:foldl(
      fun(E2, Acc) ->
              maps:fold(
                fun(V, T, A) ->
                        case maps:find(V, E2) of
                            {ok, T2} -> A#{V => join(T, T2)};
                            error -> A
                        end
                end, #{}, Acc)
      end, E, Es);
intersect_join([]) -> #{}.

join(T, T) -> T;
join(A, B) ->
    case subtype(A, B) of
        true -> B;
        false -> case subtype(B, A) of true -> A; false -> any end
    end.

%% dfs_post prepends each block after its successors, so the returned
%% list is already in reverse-postorder (entry first) — do NOT reverse.
rpo(Bs) ->
    {Order, _} = dfs_post(0, Bs, sets:new(), []),
    Order.

dfs_post(B, Bs, Seen, Acc) ->
    case sets:is_element(B, Seen) orelse not maps:is_key(B, Bs) of
        true -> {Acc, Seen};
        false ->
            Seen1 = sets:add_element(B, Seen),
            #b_blk{last=Last} = maps:get(B, Bs),
            {Acc1, Seen2} =
                lists:foldl(fun(S, {A, Sn}) -> dfs_post(S, Bs, Sn, A) end,
                            {Acc, Seen1}, successors(Last)),
            {[B | Acc1], Seen2}
    end.

pred_map(Bs) ->
    lists:foldl(
      fun(B, Acc) ->
              #b_blk{last=Last} = maps:get(B, Bs),
              lists:foldl(fun(S, A) -> A#{S => [B | maps:get(S, A, [])]} end,
                          Acc, successors(Last))
      end, #{}, maps:keys(Bs)).
