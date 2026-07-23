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
%% This module is the RECOGNIZER half (increment A1-1, dry-run). It reports
%% the eligible functions and does not modify the SSA; the destination-passing
%% rewrite lands in a follow-up increment. Gated behind the `tmc' compile
%% option (default OFF); with the option off this module is never invoked and
%% output is byte-identical.
%%
%% v1 scope: cons-only, self-recursion-as-loop. A function `F/A' is eligible
%% when it has at least one return site of the shape `ret (put_list H, R)'
%% where `R' is the result of a self call to `F/A' (the TMC edge), AND every
%% self call in the function is in a "good" position -- either returned
%% directly (a plain tail-self loop edge) or consumed only as the tail of a
%% put_list that is itself returned (a TMC edge). Anything else (self call in
%% head position, threaded through further computation, mutual recursion,
%% multi-cons / nested-constructor tails) is left for later increments.

-module(beam_ssa_tmc).
-moduledoc false.

%% Structure: a front-end recognizer (extract/2) describes the recursion as an
%% Info map -- the cell-building site (element + recursion arguments), the
%% self-recursion, and the base seal-sites -- and a shape-agnostic
%% destination-passing lowering (build_dps/4) consumes that Info to emit the
%% Root/Dest-threading helper with set_cons_tail. build_dps is deliberately
%% not welded to the body-recursive front-end: a second front-end (a
%% tail-recursive accumulator-prepend whose base is lists:reverse(Acc), with a
%% uniquely-owned Acc -- the same precondition Technique B / beam_ssa_alias
%% already prove) can produce an equivalent Info and reuse the lowering to
%% build forward in one pass rather than prepend-then-reverse. That second
%% front-end is a later increment. Note for the tradeoff writeup: the TMC form
%% carries the force-fullsweep tax on GC-spanning builds, whereas Technique B's
%% rev_inplace copy-falls-back on a tenured spine -- so B stays the long-list
%% fallback until the tracked-edge/builder-box refinement removes the tax.

-export([module/2, recognize/2, eligible/2]).

-include("beam_ssa.hrl").

-type fa() :: {atom(), arity()}.

%%----------------------------------------------------------------------
%% module(Module, Opts) -> {ok, Module}
%%
%% The tail-modulo-cons pass. For every function whose sole self-recursion
%% is a single cons-in-tail-position self call (the narrow v1 shape: map /
%% filter-less / append), rewrite it into destination-passing style: the
%% original function builds the first cell and tail-calls a generated helper
%% `-tmc-Name/Arity-'/Arity+2 that threads the running list (Root) and the
%% cell whose tail is the current hole (Dest), filling the hole with each new
%% cell via set_cons_tail and sealing at the base clause. O(1) stack via the
%% tail call; identical element order.
%%----------------------------------------------------------------------
-spec module(#b_module{}, [compile:option()]) -> {ok, #b_module{}}.
module(#b_module{body=Fs0}=Module, Opts) ->
    Report = proplists:get_bool(tmc_report, Opts),
    Fs = lists:flatmap(fun(F) -> transform_fun(F, Report) end, Fs0),
    {ok, Module#b_module{body=Fs}}.

transform_fun(#b_function{anno=Anno}=F, Report) ->
    case Anno of
        #{func_info := {Mod,Name,Arity}} ->
            case extract({Name,Arity}, F) of
                {ok, Info} ->
                    _ = Report andalso
                        io:format("tmc: rewrote ~p:~p/~p~n", [Mod,Name,Arity]),
                    build_dps(F, Mod, {Name,Arity}, Info);
                no ->
                    [F]
            end;
        #{} ->
            [F]
    end.

%%----------------------------------------------------------------------
%% extract(FA, F) -> {ok, Info} | no
%%   Narrow rewritable shape: exactly one self call, in cons-tail position.
%%----------------------------------------------------------------------
extract(FA, #b_function{args=Args, bs=Blocks}) ->
    case recognize(FA, Blocks) of
        {true, _} ->
            Defs = def_map(Blocks),
            ConsSites = [{L,Elem,Rec}
                         || {L,#b_blk{last=#b_ret{arg=V}}} <- maps:to_list(Blocks),
                            {ok,Elem,Rec} <- [cons_site(V, FA, Defs)]],
            SelfCalls = [Dst || {Dst,S} <- maps:to_list(Defs), is_self_call(S, FA)],
            case {ConsSites, SelfCalls} of
                {[{Lc,Elem,Rec}], [Rec]} ->
                    %% exactly one cons site fed by the one and only self call
                    Lcall = call_block_of(Rec, Blocks),
                    #b_set{args=[_Callee|RecArgs]} = maps:get(Rec, Defs),
                    case {Lcall, base_sites(Lc, Args, Blocks)} of
                        {none, _} -> no;
                        {_, []} -> no;
                        {_, BaseSites} ->
                            {ok, #{cons_block => Lc, elem => Elem, rec => Rec,
                                   rec_args => RecArgs, call_block => Lcall,
                                   base_sites => BaseSites}}
                    end;
                _ ->
                    no
            end;
        false ->
            no
    end.

%% V = put_list(Elem, Rec) where Rec is a self call.
cons_site(V, FA, Defs) ->
    case resolve(V, Defs) of
        {set, #b_set{op=put_list, args=[Elem, Tl]}} ->
            case resolve(Tl, Defs) of
                {set, #b_set{op=call}=S} ->
                    case is_self_call(S, FA) of
                        true -> {ok, Elem, Tl};
                        false -> no
                    end;
                _ -> no
            end;
        _ -> no
    end.

%% Which block defines Rec (the self call)?
call_block_of(Rec, Blocks) ->
    case [L || {L,#b_blk{is=Is}} <- maps:to_list(Blocks),
               lists:any(fun(#b_set{dst=D}) -> D =:= Rec end, Is)] of
        [L] -> L;
        _ -> none
    end.

%% Base seal-sites: ret blocks (other than the cons block) whose value is []
%% or a function argument. Excludes exception/error blocks (which return a
%% call result), so their exception semantics are preserved.
base_sites(ConsBlock, Args, Blocks) ->
    ArgSet = sets:from_list(Args),
    [{L, V} || {L, #b_blk{last=#b_ret{arg=V}}} <- maps:to_list(Blocks),
               L =/= ConsBlock,
               is_base_val(V, ArgSet)].

is_base_val(#b_literal{val=[]}, _) -> true;
is_base_val(#b_var{}=V, ArgSet) -> sets:is_element(V, ArgSet);
is_base_val(_, _) -> false.

%%----------------------------------------------------------------------
%% build_dps(F, Mod, FA, Info) -> [F_rewritten, F_dps]
%%----------------------------------------------------------------------
build_dps(#b_function{anno=Anno, args=Args, bs=Bs, cnt=Cnt}=F, Mod, {Name,Arity}, Info) ->
    #{cons_block := Lc, elem := Elem, rec := Rec, rec_args := RecArgs,
      call_block := Lcall, base_sites := BaseSites} = Info,
    DpsName = dps_name(Name, Arity),
    DpsArity = Arity + 2,
    DpsCallee = #b_local{name=#b_literal{val=DpsName}, arity=DpsArity},
    Nil = #b_literal{val=[]},

    %% ---- helper f_dps: original body + [Root,Dest] args ----
    RootV = #b_var{name=Cnt},
    DestV = #b_var{name=Cnt+1},
    NewV  = #b_var{name=Cnt+2},
    CallBlk0 = maps:get(Lcall, Bs),
    %% Keep any element/argument computations in the call block (e.g. get_hd /
    %% get_tl); drop only the self call and its succeeded test.
    KeptIs = keep_call_instrs(CallBlk0#b_blk.is, Rec),
    %% Rewrite the recursion block: build this cell, splice it onto the hole,
    %% then tail-call the helper threading (Root, New).
    DpsCallBlk = CallBlk0#b_blk{
        is = KeptIs ++
             [mk_set(NewV, put_list, [Elem, Nil]),
              mk_set(none, set_cons_tail, [DestV, NewV]),
              mk_set(Rec, call, [DpsCallee | RecArgs ++ [RootV, NewV]])],
        last = #b_ret{arg=Rec}},
    DpsBs1 = maps:remove(Lc, Bs#{Lcall => DpsCallBlk}),
    %% Seal each base block: fill the last hole with the base value, return Root.
    DpsBs = lists:foldl(
              fun({Lb, BaseVal}, Acc) ->
                      #b_blk{is=Is0}=B = maps:get(Lb, Acc),
                      B1 = B#b_blk{is = Is0 ++ [mk_set(none, set_cons_tail, [DestV, BaseVal])],
                                   last = #b_ret{arg=RootV}},
                      Acc#{Lb => B1}
              end, DpsBs1, BaseSites),
    FDps = #b_function{anno = Anno#{func_info => {Mod, DpsName, DpsArity}},
                       args = Args ++ [RootV, DestV],
                       bs = DpsBs,
                       cnt = Cnt+3},

    %% ---- original f: build the first cell, bootstrap into the helper ----
    Root0 = #b_var{name=Cnt},                   %% separate namespace, reuse Cnt
    FCallBlk = CallBlk0#b_blk{
        is = KeptIs ++
             [mk_set(Root0, put_list, [Elem, Nil]),
              mk_set(Rec, call, [DpsCallee | RecArgs ++ [Root0, Root0]])],
        last = #b_ret{arg=Rec}},
    FBs = maps:remove(Lc, Bs#{Lcall => FCallBlk}),
    FRw = F#b_function{bs = FBs, cnt = Cnt+1},

    [FRw, FDps].

mk_set(Dst, Op, Args) ->
    #b_set{dst=Dst, op=Op, args=Args}.

%% Drop the self call (defines Rec) and its succeeded test; keep the rest.
keep_call_instrs(Is, Rec) ->
    lists:filter(
      fun(#b_set{dst=D}) when D =:= Rec -> false;
         (#b_set{op={succeeded,_}, args=[A]}) when A =:= Rec -> false;
         (_) -> true
      end, Is).

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
    Defs = def_map(Blocks),
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
    case resolve(V, Defs) of
        {set, #b_set{op=put_list, args=[_Hd, Tl]}} ->
            case resolve(Tl, Defs) of
                {set, #b_set{op=call}=S} -> is_self_call(S, FA);
                _ -> false
            end;
        _ -> false
    end.

%%----------------------------------------------------------------------
%% Cleanliness: every self call is in a good position.
%%----------------------------------------------------------------------
self_calls_all_good(FA, Blocks) ->
    Defs = def_map(Blocks),
    Uses = use_map(Blocks),
    RetVars = ret_var_set(Blocks),
    SelfVars = [Dst || #b_blk{is=Is} <- maps:values(Blocks),
                       #b_set{dst=Dst}=S <- Is,
                       Dst =/= none, is_self_call(S, FA)],
    %% A function with no self call at all is not a loop (shouldn't reach here
    %% since recognize found a TMC site, which implies a self call).
    SelfVars =/= [] andalso
        lists:all(fun(SV) -> good_use(SV, Uses, Defs, RetVars, 16) end, SelfVars).

good_use(SV, Uses, Defs, RetVars, Fuel) when Fuel > 0 ->
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
                                  good_use(D, Uses, Defs, RetVars, Fuel - 1);
                         (_) ->
                              false
                      end, Us)
            end
    end;
good_use(_, _, _, _, _) ->
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
def_map(Blocks) ->
    maps:fold(
      fun(_L, #b_blk{is=Is}, Acc) ->
              lists:foldl(
                fun(#b_set{dst=Dst}=S, A) when Dst =/= none -> A#{Dst => S};
                   (_, A) -> A
                end, Acc, Is)
      end, #{}, Blocks).

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

resolve(#b_var{}=V, Defs) ->
    case maps:find(V, Defs) of
        {ok, S} -> {set, S};
        error -> {var, V}
    end;
resolve(#b_literal{}=L, _) -> {lit, L};
resolve(Other, _) -> {other, Other}.
