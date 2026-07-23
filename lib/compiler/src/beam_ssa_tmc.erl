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

-export([recognize/2, eligible/2]).

-include("beam_ssa.hrl").

-type fa() :: {atom(), arity()}.

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
