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
%% Tail-Modulo-Cons (TMC), idea #68 Technique A.
%%
%% Recognizes body-recursive list builders in tail-modulo-constructor
%% position -- clauses whose result is `[H | self(...)]' -- for rewriting
%% into an O(1)-stack destination-passing-style tail loop (the map / filter /
%% append / list-comprehension family).
%%
%% This module performs the full destination-passing-style (DPS) rewrite. It
%% runs by default; the `no_tmc' compile option disables it, and with the pass
%% off output is byte-identical. A function `F/A' is rewritten when it has at
%% least one return site of the shape `ret (put_list H, R)' where `R' is the
%% result of a self call to `F/A' (the TMC edge), AND every self call in the
%% function is in a "good" position -- either returned directly (a plain
%% tail-self loop edge, i.e. a filter skip) or consumed only as the tail of a
%% put_list that is itself returned (a TMC edge). Beyond the single-cons map
%% shape this covers filter skips (`f([_|T]) -> f(T)'), multiple cons clauses
%% (several cons edges feeding one hole) and multi-cons per step
%% (`[A, B | f(T)]'). Anything else (self call in head position, threaded
%% through further computation, mutual recursion) is left unchanged.

-module(beam_ssa_tmc).
-moduledoc false.

%% Structure: two front-end recognizers each describe the recursion as an Info
%% map -- extract/2 for the body-recursive builder (the cons sites, the self
%% recursion, the base seal-sites and any filter continue edges) and
%% extract_accrev/2 for the tail-recursive accumulator-prepend whose base is
%% lists:reverse(Acc). Each has its own lowering (build_dps/4 and
%% build_dps_accrev/4) that emits the Root/Dest-threading helper with
%% set_cons_tail; the two lowerings share the cell/instruction helpers
%% (build_cells, keep_call_instrs, ...). extract_accrev needs no
%% uniqueness/alias proof because it never mutates Acc -- it builds a fresh Root
%% forward and seals with reverse(Acc, Root); the only precondition is that Acc
%% is observed solely as the prepend tail and the reverse argument. Note for the
%% tradeoff writeup: the TMC form carries the force-fullsweep tax on GC-spanning
%% builds, whereas Technique B's rev_inplace copy-falls-back on a tenured spine
%% -- so B stays the long-list fallback until the tracked-edge/builder-box
%% refinement removes the tax.

-export([module/2, recognize/2, eligible/2]).

-include("beam_ssa.hrl").

-type fa() :: {atom(), arity()}.

%%----------------------------------------------------------------------
%% module(Module, Opts) -> {ok, Module}
%%
%% The tail-modulo-cons pass. For every function whose self-recursion is in
%% cons-tail position (map, filter, append, list comprehensions -- including
%% multiple cons clauses and multi-cons steps `[A, B | f(T)]'), rewrite it into
%% destination-passing style: the original function builds the first cell(s) and
%% tail-calls a generated helper `-tmc-Name/Arity-'/Arity+2 that threads the
%% running list (Root) and the cell whose tail is the current hole (Dest),
%% filling the hole with each new cell via set_cons_tail and sealing at the base
%% clause. O(1) stack via the tail call; identical element order.
%%----------------------------------------------------------------------
-spec module(#b_module{}, [compile:option()]) -> {ok, #b_module{}}.
module(#b_module{body=Fs0}=Module, Opts) ->
    Report = proplists:get_bool(tmc_report, Opts),
    Fs = lists:flatmap(fun(F) -> transform_fun(F, Report) end, Fs0),
    {ok, Module#b_module{body=Fs}}.

transform_fun(#b_function{anno=#{func_info := {Mod,Name,Arity}}}=F, Report) ->
    %% Every beam_ssa function carries a func_info annotation.
    FA = {Name,Arity},
    case try_transform(FA, F, Mod) of
        {ok, Kind, Result} ->
            report(Report, Mod, Name, Arity, Kind),
            Result;
        no ->
            [F]
    end.

%% Recognize and lower, then guard the result: a builder shape may pass the
%% recognizer but produce a helper that is not well-formed SSA -- e.g. the list
%% element is computed *after* the self call (so its definition is stranded when
%% the call block becomes the tail-call). Such a transform is discarded and the
%% function is compiled unchanged. This keeps the pass safe to run by default:
%% the worst case is a missed optimization, never a broken module.
try_transform(FA, F, Mod) ->
    case extract(FA, F) of
        {ok, Info} ->
            accept(build_dps(F, Mod, FA, Info), "body-rec");
        no ->
            %% front-end 2: accumulator+reverse -> forward TMC.
            case extract_accrev(FA, F) of
                {ok, Info2} ->
                    accept(build_dps_accrev(F, Mod, FA, Info2), "acc+reverse");
                no ->
                    no
            end
    end.

%% Commit the lowering only if every rewritten function is well-formed SSA.
accept(Result, Kind) ->
    case lists:all(fun valid_fun/1, Result) of
        true -> {ok, Kind, Result};
        false -> no
    end.

%% A lowering is valid only if every variable used in a block reachable from the
%% entry is defined in a reachable block (or is a function argument). This
%% rejects shapes where the list element depends on a value computed *after* the
%% self call: build_dps turns the call block into the tail-call and thereby
%% orphans the blocks that computed that value, leaving the (moved) element
%% construction referencing a now-unreachable definition.
valid_fun(#b_function{args=Args, bs=Bs}) ->
    RPO = beam_ssa:rpo(Bs),
    Defined = maps:from_keys(Args ++ beam_ssa:def(RPO, Bs), []),
    lists:all(
      fun(L) ->
              #b_blk{is=Is, last=Last} = maps:get(L, Bs),
              lists:all(fun(I) -> args_defined(I, Defined) end, Is)
                  andalso args_defined(Last, Defined)
      end, RPO).

%% A phi operand is supplied along its predecessor edge, so it need only be
%% defined in that predecessor -- checking global reachable definedness is
%% sufficient here.
args_defined(#b_set{op=phi, args=Args}, Defined) ->
    lists:all(fun({V, _Pred}) -> val_defined(V, Defined) end, Args);
args_defined(#b_set{args=Args}, Defined) ->
    lists:all(fun(A) -> val_defined(A, Defined) end, Args);
args_defined(#b_ret{arg=A}, Defined) -> val_defined(A, Defined);
args_defined(#b_br{bool=B}, Defined) -> val_defined(B, Defined);
args_defined(#b_switch{arg=A}, Defined) -> val_defined(A, Defined).

val_defined(#b_var{}=V, Defined) -> is_map_key(V, Defined);
val_defined(_, _) -> true.

report(false, _, _, _, _) -> ok;
report(true, Mod, Name, Arity, Kind) ->
    io:format("tmc: rewrote ~p:~p/~p (~s)~n", [Mod,Name,Arity,Kind]).

%%----------------------------------------------------------------------
%% extract(FA, F) -> {ok, Info} | no
%%   Narrow rewritable shape: exactly one self call, in cons-tail position.
%%----------------------------------------------------------------------
extract(FA, #b_function{bs=Blocks}) ->
    case recognize(FA, Blocks) of
        {true, _} ->
            Defs = beam_ssa:definitions(maps:keys(Blocks), Blocks),
            ConsSites = [{L, Chain, Vars, Rec}
                         || {L,#b_blk{last=#b_ret{arg=V}}} <- maps:to_list(Blocks),
                            {ok, Chain, Vars, Rec} <- [cons_site(V, FA, Defs)]],
            SelfCalls = [Dst || {Dst,S} <- maps:to_list(Defs), is_self_call(S, FA)],
            %% recognize/2 found at least one TMC cons return, so ConsSites is
            %% non-empty. Every cons clause is a cons edge (its own distinct self
            %% call). Any OTHER self call must be a `continue' edge -- a self call
            %% returned directly (a filter skip). recognize/2 already proved every
            %% self call is good; verify the split (distinct cons edges, and the
            %% rest returned directly).
            ConsRecs = [R || {_,_,_,R} <- ConsSites],
            Continue = SelfCalls -- ConsRecs,
            RetVars = ret_var_set(Blocks),
            %% A cons edge's self call must NOT also be returned directly: that
            %% is the shape produced by the inlined `lists:filter' shape
            %%   `Rec = f(T), case B of true -> [X|Rec]; false -> Rec end'
            %% where one recursion result is BOTH consed and skip-returned. The
            %% DPS lowering rewrites the call block into the cons path and would
            %% drop the skip, so reject (leave the function unchanged).
            case length(lists:usort(ConsRecs)) =:= length(ConsRecs)
                andalso lists:all(fun(R) -> not sets:is_element(R, RetVars) end,
                                  ConsRecs)
                andalso lists:all(fun(C) -> sets:is_element(C, RetVars) end,
                                  Continue) of
                true ->
                    ConsBlocks = [L || {L,_,_,_} <- ConsSites],
                    Sites = [{L, Chain, Vars, Rec, cons_args(Rec, Defs),
                              def_block(Rec, Blocks)}
                             || {L, Chain, Vars, Rec} <- ConsSites],
                    BaseSites = base_sites(ConsBlocks, Blocks, Defs, FA),
                    {ok, #{cons_sites => Sites,
                           base_sites => BaseSites, continue => Continue}};
                false ->
                    no
            end;
        false ->
            no
    end.

%% V is a chain of one or more put_lists ending in a self call:
%% `[E1, E2, ..., Em | Rec]'. Returns {ok, ElemChain, ChainVars, Rec} where
%% ChainVars are the put_list result vars of the chain (dropped from the cons
%% block and rebuilt as cells by build_dps). A single cons is the m = 1 case;
%% m > 1 is a multi-cons clause like `[H, g(H) | f(T)]'.
cons_site(V, FA, Defs) ->
    cons_chain(V, FA, Defs, [], []).

cons_chain(V, FA, Defs, ElemAcc, VarAcc) ->
    case resolve(V, Defs) of
        {set, #b_set{op=put_list, args=[E, Tl], dst=D}} ->
            case resolve(Tl, Defs) of
                {set, #b_set{op=call}=S} ->
                    case is_self_call(S, FA) of
                        true ->
                            {ok, lists:reverse([E|ElemAcc]),
                             lists:reverse([D|VarAcc]), Tl};
                        false ->
                            no
                    end;
                {set, #b_set{op=put_list}} ->
                    cons_chain(Tl, FA, Defs, [E|ElemAcc], [D|VarAcc]);
                _ ->
                    no
            end;
        _ ->
            no
    end.

%% The recursion arguments of a self call (its args minus the callee).
cons_args(Rec, Defs) ->
    #b_set{args=[_Callee|Args]} = maps:get(Rec, Defs),
    Args.

%% Seal-sites: every ret block other than the cons block, excluding blocks that
%% raise. In the destination-passing helper each seals the current hole (Dest)
%% with its return value and returns Root. That value becomes the tail of the
%% built list, which is exactly the body-recursive meaning of
%% `[H1 | ... [Hk | Value]]' -- whether Value is `[]', a function argument, or
%% any other expression (e.g. a call result, as in epp:coalesce_strings/1).
%%
%% The shared function_clause exception block returns an erlang:error(badarg)
%% call result, so it matches #b_ret too; it must NOT be sealed. It throws before
%% any seal could run, and codegen asserts it stays a bare erlang:error call
%% (assert_exception_block/1). Leaving erlang:error ret blocks unchanged both
%% keeps that invariant and preserves their exception semantics.
%%
%% The recognizer guarantees the only self call is the cons-edge one (SelfCalls
%% =:= [Rec] in extract/2), so no seal-site returns a self-call result -- sealing
%% never swallows a recursion that should have continued the loop.
%% A ret block that returns a self-call result directly is a `continue' edge
%% (a filter skip: `f([_|T]) -> f(T)') -- it must CONTINUE the loop, threaded to
%% the helper, not be sealed. build_dps handles those separately.
base_sites(ConsBlocks, Blocks, Defs, FA) ->
    [{L, V} || {L, #b_blk{last=#b_ret{arg=V}}} <- maps:to_list(Blocks),
               not lists:member(L, ConsBlocks),
               not is_error_ret(V, Defs),
               not is_self_ret(V, FA, Defs)].

is_error_ret(V, Defs) ->
    case resolve(V, Defs) of
        {set, #b_set{op=call,
                     args=[#b_remote{mod=#b_literal{val=erlang},
                                     name=#b_literal{val=error}} | _]}} ->
            true;
        _ ->
            false
    end.

is_self_ret(V, FA, Defs) ->
    case resolve(V, Defs) of
        {set, S} -> is_self_call(S, FA);
        _ -> false
    end.

%%----------------------------------------------------------------------
%% build_dps(F, Mod, FA, Info) -> [F_rewritten, F_dps]
%%----------------------------------------------------------------------
build_dps(#b_function{anno=Anno, args=Args, bs=Bs, cnt=Cnt}=F, Mod, {Name,Arity}, Info) ->
    #{cons_sites := Sites, base_sites := BaseSites, continue := Continue} = Info,
    DpsName = dps_name(Name, Arity),
    DpsArity = Arity + 2,
    DpsCallee = #b_local{name=#b_literal{val=DpsName}, arity=DpsArity},
    Nil = #b_literal{val=[]},
    ConsBlocks = [L || {L,_,_,_,_,_} <- Sites],

    %% ---- helper f_dps: original body + [Root,Dest] args ----
    %% Each cons clause builds its cell chain, splices the chain head onto the
    %% current hole (Dest) and tail-calls the helper with the chain's LAST cell
    %% as the new Dest (its CDR is now the hole).
    RootV = #b_var{name=Cnt},
    DestV = #b_var{name=Cnt+1},
    {DpsBs0, DpsCnt} =
        lists:foldl(fun(Site, {Acc, V}) ->
                            cons_block(dps, Site, DpsCallee, RootV, DestV, Nil, Acc, V)
                    end, {Bs, Cnt+2}, Sites),
    DpsBs1 = maps:without(ConsBlocks, DpsBs0),
    %% Seal each base block: fill the last hole with the base value, return Root.
    DpsBs2 = lists:foldl(
               fun({Lb, BaseVal}, Acc) ->
                       #b_blk{is=Is0}=B = maps:get(Lb, Acc),
                       B1 = B#b_blk{is = Is0 ++ [mk_set(none, set_cons_tail, [DestV, BaseVal])],
                                    last = #b_ret{arg=RootV}},
                       Acc#{Lb => B1}
               end, DpsBs1, BaseSites),
    %% Continue edges (filter skips: `f([_|T]) -> f(T)'): tail-call the helper
    %% threading the SAME Root/Dest, so the element is skipped and the loop
    %% continues (NOT sealed).
    DpsBsC = thread_continue(Continue, DpsCallee, RootV, DestV, DpsBs2),
    FDps = #b_function{anno = Anno#{func_info => {Mod, DpsName, DpsArity}},
                       args = Args ++ [RootV, DestV],
                       bs = DpsBsC,
                       cnt = DpsCnt},

    %% ---- original f: each cons clause builds its chain and bootstraps into
    %% the helper (chain head = Root, chain last cell = Dest). Continue edges are
    %% left as plain tail-recursive self calls that reach a cons and bootstrap.
    {FBs0, FrwCnt} =
        lists:foldl(fun(Site, {Acc, V}) ->
                            cons_block(frw, Site, DpsCallee, RootV, DestV, Nil, Acc, V)
                    end, {Bs, Cnt}, Sites),
    FBs = maps:without(ConsBlocks, FBs0),
    FRw = F#b_function{bs = FBs, cnt = FrwCnt},

    [FRw, FDps].

%% Rewrite a cons-site call block. The shared part builds this clause's cell
%% chain (dropping the sunk chain vars, keeping the element/loop instructions);
%% only the trailing instructions differ by mode. Mode `dps' (the helper)
%% splices the chain head onto the current hole (Dest) and tail-calls threading
%% Root and the chain's last cell as the new Dest. Mode `frw' (the bootstrap in
%% the original function) tail-calls with the chain head as Root and the last
%% cell as Dest, with no splice.
cons_block(Mode, {L, Chain, ChainVars, Rec, RecArgs, Lcall}, DpsCallee,
           RootV, DestV, Nil, Bs, Var) ->
    CallBlk = maps:get(Lcall, Bs),
    KeptIs = keep_call_instrs(CallBlk#b_blk.is, Rec),
    ElemIs = cons_elem_instrs(L, Bs, ChainVars),
    {Cells, HeadV, LastV, Var2} = build_cells(Chain, Nil, Var),
    Tail = case Mode of
               dps -> [mk_set(none, set_cons_tail, [DestV, HeadV]),
                       mk_set(Rec, call, [DpsCallee | RecArgs ++ [RootV, LastV]])];
               frw -> [mk_set(Rec, call, [DpsCallee | RecArgs ++ [HeadV, LastV]])]
           end,
    NewBlk = CallBlk#b_blk{is = KeptIs ++ ElemIs ++ Cells ++ Tail,
                           last = #b_ret{arg=Rec}},
    {Bs#{Lcall => NewBlk}, Var2}.

%% Build the cell chain for `[E1, ..., Em]' from the tail up: cellm = [Em | Nil],
%% cell_{m-1} = [E_{m-1} | cellm], ..., cell1 = [E1 | cell2]. Returns the
%% instructions (definition order), the head cell (cell1), the last cell (cellm,
%% whose CDR becomes the next hole) and the next free var.
build_cells(Chain, Nil, StartVar) ->
    %% The accumulator's Tail slot holds the most recently built cell; after the
    %% last iteration it is cell1, the head of the chain.
    {Is, Head, Last, Next} =
        lists:foldl(
          fun(E, {Acc, Tail, LastV, V}) ->
                  Cell = #b_var{name=V},
                  I = mk_set(Cell, put_list, [E, Tail]),
                  LastV1 = case LastV of undefined -> Cell; _ -> LastV end,
                  {Acc ++ [I], Cell, LastV1, V+1}
          end, {[], Nil, undefined, StartVar}, lists:reverse(Chain)),
    {Is, Head, Last, Next}.

%% In the helper, rewrite each continue-edge self call to call the helper
%% instead (threading Root/Dest), so a skipped element continues the loop.
thread_continue(Continue, DpsCallee, RootV, DestV, Bs) ->
    lists:foldl(
      fun(RecC, Acc) ->
              L = def_block(RecC, Acc),
              #b_blk{is=Is}=B = maps:get(L, Acc),
              Is1 = [thread_self_call(I, RecC, DpsCallee, RootV, DestV) || I <- Is],
              Acc#{L => B#b_blk{is=Is1}}
      end, Bs, Continue).

thread_self_call(#b_set{dst=RecC, op=call, args=[_Callee|As]}=S, RecC,
                 DpsCallee, RootV, DestV) ->
    S#b_set{args=[DpsCallee | As ++ [RootV, DestV]]};
thread_self_call(I, _RecC, _DpsCallee, _RootV, _DestV) ->
    I.

%%======================================================================
%% Front-end 2: accumulator+reverse -> forward TMC.
%%
%%   f([H|T], Acc) -> f(T, [g(H)|Acc]);          %% tail self, prepend Acc
%%   f([],   Acc) -> lists:reverse(Acc[, Tail]).  %% terminal reverse of Acc
%%
%% Lowered to a destination-passing helper: build FORWARD with set_cons_tail
%% instead of prepend, and seal the base with lists:reverse(Acc, Root). Because
%% the base becomes reverse(Acc0, Root) = reverse(Acc0) ++ Root (Root is the
%% forward list), the rewrite is correct for ANY initial Acc0 -- no "Acc0 = []"
%% proof, and no uniqueness/alias proof is needed because Acc is never mutated
%% (a fresh Root is built).
%%
%% Beyond the single-prepend map shape this handles, with the same seal-vs-
%% continue rule as the body-recursive filter widening:
%%   * filter / multi-self-call -- some self calls thread the accumulator
%%     UNCHANGED (a skip edge, `f(T, Acc)'); a skip tail-calls the helper with
%%     the same Root/Dest and NO splice, a prepend splices set_cons_tail and
%%     threads Root/last-cell forward.
%%   * multi-clause prepend -- several clauses each prepend, all feeding the one
%%     shared Dest hole.
%%   * multi-prepend per step -- `f(T, [b, a | Acc])' builds a forward cell chain.
%%
%% Preconditions (else the whole function is left unchanged): a single
%% accumulator parameter terminally returned via lists:reverse; every self call
%% is a tail call returned directly and threads the accumulator at one position
%% as EITHER a clean prepend chain to Acc OR Acc unchanged; the accumulator and
%% prepend cells never escape (used only in the chain, the recognized self
%% calls, the reverse base, or a match_fail path, and never returned); and every
%% return is a self call, the reverse base, or an erlang:error raise.
%%======================================================================
extract_accrev(FA, #b_function{args=Args, bs=Blocks}) ->
    Defs = beam_ssa:definitions(maps:keys(Blocks), Blocks),
    SelfCalls = [{Dst,S} || {Dst, #b_set{op=call}=S} <- maps:to_list(Defs),
                            is_self_call(S, FA)],
    RetVars = ret_var_set(Blocks),
    %% Every self call must be a tail call whose result is returned directly.
    case SelfCalls =/= [] andalso
        lists:all(fun({Dst,_}) -> sets:is_element(Dst, RetVars) end, SelfCalls) of
        true ->
            case find_acc(Blocks, Defs, Args) of
                {ok, AccVar} ->
                    case find_base_reverse(AccVar, Blocks, Defs) of
                        {ok, Rev} ->
                            accrev_info(FA, Args, Blocks, Defs, SelfCalls, AccVar, Rev);
                        no -> no
                    end;
                no -> no
            end;
        false -> no
    end.

%% The accumulator is the single parameter terminally returned via
%% lists:reverse(Acc[,Tail]).
find_acc(Blocks, Defs, Args) ->
    Accs = lists:usort(
             [A0 || {_L, #b_blk{last=#b_ret{arg=#b_var{}=V}}} <- maps:to_list(Blocks),
                    {set, #b_set{op=call,
                                 args=[#b_remote{mod=#b_literal{val=lists},
                                                 name=#b_literal{val=reverse}},
                                       A0 | _]}} <- [resolve(V, Defs)],
                    is_record(A0, b_var), lists:member(A0, Args)]),
    case Accs of
        [AccVar] -> {ok, AccVar};
        _ -> no
    end.

%% Classify every self call as a prepend edge (accumulator argument is
%% `[E1,...,Em | AccVar]') or a skip/continue edge (accumulator threaded
%% unchanged), then verify the accumulator and prepend cells never escape and
%% every return is accounted for. Any self call that is neither a clean prepend
%% nor a clean skip rejects the whole function.
accrev_info(FA, Args, Blocks, Defs, SelfCalls, AccVar, Rev) ->
    #{rev_var := RevVar} = Rev,
    P = index_of(AccVar, Args),
    Classes = [classify_self(SC, P, AccVar, Defs, Blocks) || SC <- SelfCalls],
    case lists:member(bad, Classes) of
        true -> no;
        false ->
            PrependSites = [PS || {prepend, PS} <- Classes],
            Skips = [Rec || {skip, Rec} <- Classes],
            case PrependSites of
                [] -> no;   %% no cons happens -- not a builder
                _ ->
                    ChainVars = lists:append([CV || {_,_,CV,_,_} <- PrependSites]),
                    SelfRecs = [Rec || {_,_,_,Rec,_} <- PrependSites] ++ Skips,
                    Uses = use_map(Blocks),
                    case internal_uses_ok([AccVar | ChainVars], ChainVars,
                                          SelfRecs, RevVar, Uses, Blocks)
                        andalso all_rets_accounted(Blocks, Defs, FA, RevVar) of
                        true ->
                            {ok, #{prepends => PrependSites, skips => Skips,
                                   base => Rev, acc => AccVar}};
                        false -> no
                    end
            end
    end.

%% AccVar must be referenced by exactly one argument, at the accumulator
%% position P; that argument is either AccVar itself (skip) or a put_list chain
%% ending in AccVar (prepend). Anything else is bad.
classify_self({Rec, #b_set{args=[_Callee|SelfArgs]}}, P, AccVar, Defs, Blocks) ->
    RefPositions = [I || {I, A} <- enumerate(SelfArgs),
                         references_acc(A, AccVar, Defs)],
    case RefPositions of
        [P] ->
            AArg = lists:nth(P, SelfArgs),
            case AArg =:= AccVar of
                true ->
                    {skip, Rec};
                false ->
                    {ok, WalkElems, ChainVars} = prepend_chain(AArg, AccVar, Defs),
                    Elems = lists:reverse(WalkElems),
                    RecArgs = set_nth(P, SelfArgs, AccVar),
                    Lcall = def_block(Rec, Blocks),
                    {prepend, {Lcall, Elems, ChainVars, Rec, RecArgs}}
            end;
        _ ->
            bad
    end.

references_acc(A, AccVar, Defs) ->
    A =:= AccVar orelse prepend_chain(A, AccVar, Defs) =/= no.

%% Walk a `[E1,...,Em | AccVar]' put_list chain. Returns the elements outermost
%% first (the caller reverses them into forward build order) and the chain's
%% put_list dst vars.
prepend_chain(A, AccVar, Defs) ->
    prepend_chain(A, AccVar, Defs, [], []).

prepend_chain(A, AccVar, Defs, EAcc, VAcc) ->
    case resolve(A, Defs) of
        {set, #b_set{op=put_list, args=[E, Tl], dst=D}} ->
            case Tl =:= AccVar of
                true -> {ok, lists:reverse([E|EAcc]), lists:reverse([D|VAcc])};
                false -> prepend_chain(Tl, AccVar, Defs, [E|EAcc], [D|VAcc])
            end;
        _ ->
            no
    end.

enumerate(L) -> lists:zip(lists:seq(1, length(L)), L).

set_nth(P, L, X) -> [case I of P -> X; _ -> E end || {I, E} <- enumerate(L)].

%% Position (1-based) of AccVar in the argument list; AccVar is guaranteed
%% present (it is a parameter, verified by find_acc).
index_of(X, L) -> length(lists:takewhile(fun(Y) -> Y =/= X end, L)) + 1.

%% Exactly one `ret RevVar' where RevVar = lists:reverse(AccVar[, Tail]).
find_base_reverse(AccVar, Blocks, Defs) ->
    Revs = [{L, V, Arity, tl_arg(RevArgs)}
            || {L, #b_blk{last=#b_ret{arg=#b_var{}=V}}} <- maps:to_list(Blocks),
               {set, #b_set{op=call,
                            args=[#b_remote{mod=#b_literal{val=lists},
                                            name=#b_literal{val=reverse},
                                            arity=Arity}, A0 | RevArgs]}}
                   <- [resolve(V, Defs)],
               A0 =:= AccVar, (Arity =:= 1 orelse Arity =:= 2)],
    case Revs of
        [{RetL, RevVar, _Arity, TailArg}] ->
            {ok, #{ret_block => RetL, rev_var => RevVar,
                   tail => TailArg,
                   call_block => def_block(RevVar, Blocks)}};
        _ -> no
    end.

tl_arg([]) -> #b_literal{val=[]};
tl_arg([T]) -> T.

%% AccVar and every prepend cell must be observed only inside the loop: as the
%% next put_list in a recognized chain, as an argument of a recognized self call
%% (prepend or skip), as the reverse base argument, or in a match_fail error
%% path -- and never returned. Then eliminating the reversed accumulator in
%% favour of a forward build changes nothing observable. (match_fail carries the
%% function_clause args for the error message; it is an error-only path.)
internal_uses_ok(Internal, ChainVars, SelfRecs, RevVar, Uses, Blocks) ->
    lists:all(
      fun(V) ->
              %% use_map only ever records #b_set users, so these clauses are
              %% total over the users list.
              lists:all(
                fun(#b_set{op=match_fail}) -> true;
                   (#b_set{op=put_list, dst=D}) -> lists:member(D, ChainVars);
                   (#b_set{dst=D}) -> lists:member(D, SelfRecs) orelse D =:= RevVar
                end, maps:get(V, Uses, []))
                  andalso not returned_anywhere(V, Blocks)
      end, Internal).

%% Every return is a self call (rewritten), the reverse base (rewritten to
%% seal + reverse) or an erlang:error raise (left intact). A return of anything
%% else -- a raw value that would leave the Dest hole unsealed -- rejects the
%% function (the FE1 coalesce_strings hazard, avoided by construction here).
all_rets_accounted(Blocks, Defs, FA, RevVar) ->
    lists:all(
      fun(#b_blk{last=#b_ret{arg=V}}) ->
              is_self_ret(V, FA, Defs) orelse V =:= RevVar
                  orelse is_error_ret(V, Defs);
         (#b_blk{}) ->
              true
      end, maps:values(Blocks)).

ret_block_of(Rec, Blocks) ->
    [L] = [L || {L, #b_blk{last=#b_ret{arg=V}}} <- maps:to_list(Blocks), V =:= Rec],
    L.

returned_anywhere(Var, Blocks) ->
    lists:any(fun(#b_blk{last=#b_ret{arg=V}}) -> V =:= Var;
                 (_) -> false
              end, maps:values(Blocks)).

def_block(Var, Blocks) ->
    [L] = [L || {L, #b_blk{is=Is}} <- maps:to_list(Blocks),
                lists:any(fun(#b_set{dst=D}) -> D =:= Var end, Is)],
    L.

build_dps_accrev(#b_function{anno=Anno, args=Args, bs=Bs, cnt=Cnt}=F, Mod,
                 {Name,Arity}, Info) ->
    #{prepends := Prepends, skips := Skips, acc := AccVar,
      base := #{ret_block := RevRetL, rev_var := RevVar, tail := TailArg,
                call_block := RevCallL}} = Info,
    DpsName = dps_name(Name, Arity),
    DpsArity = Arity + 2,
    DpsCallee = #b_local{name=#b_literal{val=DpsName}, arity=DpsArity},
    Nil = #b_literal{val=[]},
    RevRemote2 = #b_remote{mod=#b_literal{val=lists},
                           name=#b_literal{val=reverse}, arity=2},
    RootV = #b_var{name=Cnt},
    DestV = #b_var{name=Cnt+1},
    PrependRets = [ret_block_of(Rec, Bs) || {_,_,_,Rec,_} <- Prepends],

    %% ---- helper f_dps: prepend edges splice+thread, skip edges thread only ----
    %% Each prepend rebuilds its cell chain forward, splices the chain head onto
    %% the current hole (Dest) and threads Root + the chain's last cell. Each
    %% skip tail-calls the helper with the SAME Root/Dest (no splice). The base
    %% seals the last hole then reverse(Acc0, Root).
    {DpsBs0, DpsCnt} =
        lists:foldl(fun(P, {Acc, V}) ->
                            accrev_prepend_block(dps, P, DpsCallee, RootV, DestV,
                                                 Nil, Acc, V)
                    end, {Bs, Cnt+2}, Prepends),
    DpsBs1 = thread_continue(Skips, DpsCallee, RootV, DestV, DpsBs0),
    RevCallBlk0 = maps:get(RevCallL, DpsBs1),
    KeptRev = keep_call_instrs(RevCallBlk0#b_blk.is, RevVar),
    DpsRevBlk = RevCallBlk0#b_blk{
        is = KeptRev ++ [mk_set(none, set_cons_tail, [DestV, TailArg]),
                         mk_set(RevVar, call, [RevRemote2, AccVar, RootV])],
        last = #b_ret{arg=RevVar}},
    DpsBs = maps:without([RevRetL | PrependRets], DpsBs1#{RevCallL => DpsRevBlk}),
    FDps = #b_function{anno = Anno#{func_info => {Mod, DpsName, DpsArity}},
                       args = Args ++ [RootV, DestV],
                       bs = DpsBs,
                       cnt = DpsCnt},

    %% ---- original f: prepend edges bootstrap into the helper (chain head =
    %% Root, last cell = Dest); skip edges stay plain self calls that recurse
    %% until a prepend bootstraps; the reverse base clause is left unchanged. ----
    {FBs0, FrwCnt} =
        lists:foldl(fun(P, {Acc, V}) ->
                            accrev_prepend_block(frw, P, DpsCallee, RootV, DestV,
                                                 Nil, Acc, V)
                    end, {Bs, Cnt}, Prepends),
    FBs = maps:without(PrependRets, FBs0),
    FRw = F#b_function{bs = FBs, cnt = FrwCnt},

    [FRw, FDps].

%% Rewrite a prepend-edge call block: drop the prepend put_list chain and the
%% self call, rebuild the chain forward as cells, then either splice+thread
%% (dps) or bootstrap (frw). RecArgs already threads AccVar unchanged in the
%% accumulator position, so the helper carries Acc = Acc0 throughout and the
%% base seals with reverse(Acc0, Root).
accrev_prepend_block(Mode, {Lcall, Elems, ChainVars, Rec, RecArgs}, DpsCallee,
                     RootV, DestV, Nil, Bs, Var) ->
    CallBlk = maps:get(Lcall, Bs),
    Kept = [I || I <- CallBlk#b_blk.is,
                 not lists:any(fun(CV) -> is_dst(I, CV) end, ChainVars),
                 not is_dst(I, Rec),
                 not is_succeeded_of(I, Rec)],
    {Cells, HeadV, LastV, Var2} = build_cells(Elems, Nil, Var),
    Tail = case Mode of
               dps -> [mk_set(none, set_cons_tail, [DestV, HeadV]),
                       mk_set(Rec, call, [DpsCallee | RecArgs ++ [RootV, LastV]])];
               frw -> [mk_set(Rec, call, [DpsCallee | RecArgs ++ [HeadV, LastV]])]
           end,
    NewBlk = CallBlk#b_blk{is = Kept ++ Cells ++ Tail, last = #b_ret{arg=Rec}},
    {Bs#{Lcall => NewBlk}, Var2}.

%% A block's instruction list contains only #b_set{} records, so a
%% #b_set-only clause is total here (a catch-all would be unreachable).
is_dst(#b_set{dst=D}, Var) -> D =:= Var.
is_succeeded_of(#b_set{op={succeeded,_}, args=[A]}, Var) -> A =:= Var;
is_succeeded_of(_, _) -> false.

mk_set(Dst, Op, Args) ->
    #b_set{dst=Dst, op=Op, args=Args}.

%% Drop the self call (defines Rec) and its succeeded test; keep the rest.
keep_call_instrs(Is, Rec) ->
    [I || I <- Is, not is_dst(I, Rec), not is_succeeded_of(I, Rec)].

%% Element-supporting instructions that ssa_opt may have sunk into the cons
%% block Lc (which build_dps removes). Keep all of Lc's instructions except the
%% put_list cells of the cons chain (ChainVars) -- those are rebuilt as fresh
%% cells. The chain vars are used only within the chain (recognizer invariant),
%% so the kept instructions never reference them and are safe to fold in before
%% the rebuilt cells. Lc is always distinct from the call block (the self call's
%% succeeded test splits them), which build_dps relies on when it removes Lc.
cons_elem_instrs(Lc, Bs, ChainVars) ->
    #b_blk{is=Is} = maps:get(Lc, Bs),
    [I || #b_set{dst=D}=I <- Is, not lists:member(D, ChainVars)].

dps_name(Name, Arity) ->
    list_to_atom(lists:concat(["-tmc-", Name, "/", Arity, "-"])).

%%----------------------------------------------------------------------
%% eligible(FA, Blocks) -> boolean()
%%   True iff F/A is a clean v1 TMC target.
%%----------------------------------------------------------------------
-spec eligible(fa(), #{beam_ssa:label() => beam_ssa:b_blk()}) -> boolean().
eligible(FA, Blocks) ->
    case recognize(FA, Blocks) of
        {true, _Sites} -> true;
        false -> false
    end.

%%----------------------------------------------------------------------
%% recognize(FA, Blocks) -> {true, [Label]} | false
%%   [Label] = labels of the blocks whose `ret' is a TMC cons site.
%%----------------------------------------------------------------------
-spec recognize(fa(), #{beam_ssa:label() => beam_ssa:b_blk()}) ->
          {true, [beam_ssa:label()]} | false.
recognize(FA, Blocks) ->
    Defs = beam_ssa:definitions(maps:keys(Blocks), Blocks),
    %% Collect the TMC cons return sites (block labels).
    Sites = [L || {L, #b_blk{last=#b_ret{arg=V}}} <- maps:to_list(Blocks),
                  is_tmc_cons_ret(V, FA, Defs)],
    case Sites of
        [] -> false;
        [_|_] ->
            case self_calls_all_good(FA, Blocks) of
                true -> {true, lists:sort(Sites)};
                false -> false
            end
    end.

%% A returned value V is a TMC cons site iff V = put_list(H, R) and R is a
%% self call to FA.
is_tmc_cons_ret(V, FA, Defs) ->
    case cons_site(V, FA, Defs) of
        {ok, _Chain, _Vars, _Rec} -> true;
        no -> false
    end.

%%----------------------------------------------------------------------
%% Cleanliness: every self call is in a good position.
%%----------------------------------------------------------------------
self_calls_all_good(FA, Blocks) ->
    Uses = use_map(Blocks),
    RetVars = ret_var_set(Blocks),
    SelfVars = [Dst || #b_blk{is=Is} <- maps:values(Blocks),
                       #b_set{dst=Dst}=S <- Is,
                       Dst =/= none, is_self_call(S, FA)],
    %% A function with no self call at all is not a loop (shouldn't reach here
    %% since recognize found a TMC site, which implies a self call).
    SelfVars =/= [] andalso
        lists:all(fun(SV) -> good_use(SV, Uses, RetVars, 16) end, SelfVars).

good_use(SV, Uses, RetVars, Fuel) when Fuel > 0 ->
    case sets:is_element(SV, RetVars) of
        true ->
            true;   %% returned directly (plain tail-self edge)
        false ->
            RealUses = [U || U <- maps:get(SV, Uses, []), not is_succeeded(U)],
            case RealUses of
                [] ->
                    false;
                Us ->
                    lists:all(
                      fun(#b_set{op=put_list, args=[_H, T], dst=D}) ->
                              T =:= SV andalso
                                  good_use(D, Uses, RetVars, Fuel - 1);
                         (_) ->
                              false
                      end, Us)
            end
    end;
good_use(_, _, _, _) ->
    false.

is_succeeded(#b_set{op={succeeded,_}}) -> true;
is_succeeded(_) -> false.

is_self_call(#b_set{op=call, args=[#b_local{name=#b_literal{val=F},arity=A}|_]},
             {F, A}) ->
    true;
is_self_call(_, _) ->
    false.

%%----------------------------------------------------------------------
%% SSA helpers.
%%----------------------------------------------------------------------
use_map(Blocks) ->
    maps:fold(
      fun(_L, #b_blk{is=Is}, Acc) ->
              lists:foldl(
                fun(#b_set{args=Args}=S, A) ->
                        lists:foldl(
                          fun(#b_var{}=Arg, AA) ->
                                  maps:update_with(Arg, fun(X) -> [S|X] end,
                                                   [S], AA);
                             (_, AA) -> AA
                          end, A, Args)
                end, Acc, Is)
      end, #{}, Blocks).

ret_var_set(Blocks) ->
    sets:from_list([V || #b_blk{last=#b_ret{arg=#b_var{}=V}} <- maps:values(Blocks)]).

%% resolve/2 is only ever applied to values (put_list/call arguments and ret
%% args), which are always a variable or a literal.
resolve(#b_var{}=V, Defs) ->
    case maps:find(V, Defs) of
        {ok, S} -> {set, S};
        error -> none
    end;
resolve(#b_literal{}, _) -> none.
