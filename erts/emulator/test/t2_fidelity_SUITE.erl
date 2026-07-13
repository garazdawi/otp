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

%%%-------------------------------------------------------------------
%%% T2-Full optimization fidelity oracle.
%%%
%%% Two peers run the SAME deterministic kernels: a control peer pinned
%%% to plain T1 (+JT2enable false) and a forced-tier peer (+JT2enable
%%% true with the install-quality gate off and callee retention on, so
%%% every eligible function T2-compiles at load and cross-module P1
%%% caller-inlining fires). The kernels are the exact shapes proven to
%%% trigger the tier's new optimizations — P1 tail/body fold-site
%%% expansion, maps:fold flatmap folds, P1b pass-through wrappers,
%%% map-matching heads, and guard-BIFs — driven over an input battery
%%% that deliberately hits the tricky paths: bignum-mid-list re-tag at
%%% deopt, improper lists, and every exception clause.
%%%
%%% Fidelity is asserted on erlang:phash2 of the full value-or-exception
%%% per (kernel,input); any divergence names its label. An install floor
%%% and a fire-counter floor keep the comparison from going vacuous —
%%% the forced peer must not only match T1 but must actually have
%%% installed the kernels and fired the optimizations. aarch64-only:
%%% elsewhere the forced peer is also T1 and the comparison is
%%% meaningless.
%%%-------------------------------------------------------------------
-module(t2_fidelity_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1]).

-export([fidelity/1]).

%% Run on the peers (by name, over erpc).
-export([run_kernels/0]).

%% Fire-counter floors — asserted on the DELTA the kernels contribute
%% (t2_opt_stats snapshotted before vs after the forced peer loads +
%% runs the kernels), so boot-time activity cannot mask a regression and
%% the assertion is specific to this suite's functions. Observed deltas
%% on a clean build: P1SitesInlined +7 (foldl_sum, foldl_count,
%% foldl_body, foldl_body_count, foldl_sum_nt, foldl_count_nt, ptcaller),
%% P2AccUnboxed +6, P2IvUnboxed +2, P3GuardsRemoved+P3IvOvfRemoved +11.
%% Floors sit just under those to tolerate count jitter while still
%% failing loudly if a whole optimization goes dark.
-define(T2_FLOOR_P1, 6).    %% element(1): P1 fold-sites inlined
-define(T2_FLOOR_P2ACC, 5). %% element(3): P2 accumulators unboxed
-define(T2_FLOOR_P2IV, 1).  %% element(4): P2 induction vars unboxed (maps)
-define(T2_FLOOR_P3, 8).    %% element(5)+element(6): P3 removed something

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes, 6}}].

all() ->
    [fidelity].

init_per_suite(Config) ->
    case erlang:system_info(emu_flavor) of
        jit ->
            case is_aarch64() of
                true -> Config;
                false -> {skip, "T2 backend emits on aarch64 only"}
            end;
        _ ->
            {skip, "T2 exists only in the JIT emulator"}
    end.

end_per_suite(_Config) ->
    ok.

%%%-------------------------------------------------------------------
%%% The oracle
%%%-------------------------------------------------------------------

fidelity(_Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    %% Control: pinned plain T1. Ambient ERL_AFLAGS (e.g. the CI fuzzer
    %% leg's +JT2enable true) must not tier the control.
    {ok, CtrlPeer, CtrlNode} =
        peer:start(#{name => peer:random_name(?MODULE),
                     args => ["-pa", PA, "+JT2enable", "false"]}),
    %% Forced-T2: install-gate off + callee retention on, so even losing
    %% blobs install and cross-module P1 caller-inline can fire.
    {ok, T2Peer, T2Node} =
        peer:start(#{name => peer:random_name(?MODULE),
                     env => [{"T2_INSTALL_GATE", "0"},
                             {"T2_RETAIN", "1"}],
                     args => ["-pa", PA, "+JT2enable", "true"]}),
    try
        Ctrl = erpc:call(CtrlNode, ?MODULE, run_kernels, [], 120000),

        %% Snapshot the forced peer's opt counters BEFORE it has loaded
        %% this suite module (nothing referenced ?MODULE on that node
        %% yet, so its kernels are not compiled), then run the kernels
        %% (which loads + T2-compiles the module, firing the opts), then
        %% snapshot again. The delta is this suite's own contribution.
        _ = erpc:call(T2Node, erts_debug, set_internal_state,
                      [available_internal_state, true]),
        S0 = opt_stats(T2Node),
        T2 = erpc:call(T2Node, ?MODULE, run_kernels, [], 120000),
        S1 = opt_stats(T2Node),
        ct:log("control:~n~p~nforced-t2:~n~p", [Ctrl, T2]),

        %% (2) FIDELITY gate: per-label, zero divergence.
        true = length(Ctrl) =:= length(T2),
        Pairs = lists:zip(Ctrl, T2),
        Misaligned = [{L1, L2} || {{L1, _}, {L2, _}} <- Pairs, L1 =/= L2],
        Diverged = [{Label, CH, TH}
                    || {{Label, CH}, {Label2, TH}} <- Pairs,
                       Label =:= Label2,
                       CH =/= TH],
        ct:log("labels=~p misaligned=~p diverged=~p",
               [length(Ctrl), Misaligned, Diverged]),
        [] = Misaligned,
        [] = Diverged,

        %% (3) Non-vacuous install: every kernel installed on the forced
        %% peer. Collect + log any that are not, then assert none.
        NotInstalled =
            [{F, A}
             || {F, A} <- kernel_mfas(),
                not is_tuple(
                      erpc:call(T2Node, erts_debug, get_internal_state,
                                [{t2_installed, ?MODULE, F, A}]))],
        ct:log("kernels=~p not_installed=~p",
               [length(kernel_mfas()), NotInstalled]),
        [] = NotInstalled,

        %% (4) Fire-counters: prove the optimizations actually fired on
        %% THIS suite's kernels (delta S1-S0), not just that results
        %% matched. A whole optimization going dark fails here even
        %% though the fidelity gate would still be green.
        check_fire_counters(S0, S1)
    after
        catch peer:stop(CtrlPeer),
        catch peer:stop(T2Peer)
    end,
    ok.

%% Read the six-field t2_opt_stats tuple from a node. A missing BIF (or
%% a non-jit/off-aarch64 node that slipped past the suite skip) yields a
%% clear failure rather than a confusing badmatch downstream.
opt_stats(Node) ->
    case catch erpc:call(Node, erts_debug, get_internal_state,
                         [t2_opt_stats]) of
        {P1, _, _, _, _, _} = S when is_integer(P1) -> S;
        Other -> ct:fail({t2_opt_stats_unavailable, Other})
    end.

check_fire_counters({B1, B2, B3, B4, B5, B6}, {A1, A2, A3, A4, A5, A6}) ->
    Delta = {D1, _D2, D3, D4, D5, D6} =
        {A1 - B1, A2 - B2, A3 - B3, A4 - B4, A5 - B5, A6 - B6},
    ct:log("t2_opt_stats before=~p after=~p~n"
           "delta = {P1SitesInlined=~p, P1LoopsRecovered=~p, "
           "P2AccUnboxed=~p, P2IvUnboxed=~p, P3GuardsRemoved=~p, "
           "P3IvOvfRemoved=~p}",
           [{B1, B2, B3, B4, B5, B6}, {A1, A2, A3, A4, A5, A6} | tuple_to_list(Delta)]),
    true = D1 >= ?T2_FLOOR_P1,          %% P1 fold-sites inlined
    true = D3 >= ?T2_FLOOR_P2ACC,       %% P2 accumulators unboxed
    true = D4 >= ?T2_FLOOR_P2IV,        %% P2 induction vars unboxed (maps)
    true = (D5 + D6) >= ?T2_FLOOR_P3,   %% P3 removed guards / overflow checks
    ok.

%% The kernel functions whose T2 install we assert (must be non-empty
%% and every entry must install on the forced peer).
kernel_mfas() ->
    [{foldl_sum, 1}, {foldl_count, 1},
     {foldl_body, 1}, {foldl_body_count, 1},
     {foldl_sum_nt, 1}, {foldl_count_nt, 1},
     {mapsfold_sum, 1}, {mapsfold_kv, 1},
     {apply_fold, 2}, {ptcaller, 1},
     {getv, 1}, {has, 1}, {big, 1},
     {fst, 1}, {mg, 2}, {msz, 1}, {hdk, 1}, {mk, 2}, {bsz, 1}].

%%%-------------------------------------------------------------------
%%% Peer side: build the deterministic (kernel,input) matrix and hash
%%% each value-or-exception. Same code runs on both peers.
%%%-------------------------------------------------------------------

run_kernels() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    [{Label, erlang:phash2(RE, 1 bsl 27)} || {Label, RE} <- kernel_results()].

kernel_results() ->
    list_fold_results()
        ++ maps_fold_results()
        ++ map_head_results()
        ++ guard_bif_results().

%% List-fold kernels over the full list battery (6 inputs each).
list_inputs() ->
    [{empty,    []},
     {small,    lists:seq(1, 50)},
     {large,    lists:seq(1, 5000)},
     %% Bignum mid-list forces the P2 re-tag-at-deopt path; must stay
     %% byte-identical between T1 and T2.
     {bignum,   lists:seq(1, 500) ++ [1 bsl 60] ++ lists:seq(502, 1000)},
     {improper, [1, 2, 3 | 4]},
     {not_list, not_a_list}].

list_kernels() ->
    [{foldl_sum,        fun foldl_sum/1},
     {foldl_count,      fun foldl_count/1},
     {foldl_body,       fun foldl_body/1},
     {foldl_body_count, fun foldl_body_count/1},
     {foldl_sum_nt,     fun foldl_sum_nt/1},
     {foldl_count_nt,   fun foldl_count_nt/1},
     {ptcaller,         fun ptcaller/1}].

list_fold_results() ->
    [{{K, ITag}, call(F, I)}
     || {K, F} <- list_kernels(),
        {ITag, I} <- list_inputs()].

%% maps:fold kernels over the maps battery (4 inputs each).
maps_fold_inputs() ->
    [{empty,   #{}},
     {flat,    maps:from_list([{N, N * N} || N <- lists:seq(1, 20)])},
     {hash,    maps:from_list([{N, N} || N <- lists:seq(1, 100)])},
     {not_map, not_a_map}].

maps_fold_kernels() ->
    [{mapsfold_sum, fun mapsfold_sum/1},
     {mapsfold_kv,  fun mapsfold_kv/1}].

maps_fold_results() ->
    [{{K, ITag}, call(F, I)}
     || {K, F} <- maps_fold_kernels(),
        {ITag, I} <- maps_fold_inputs()].

%% Map-matching-head kernels (matching / non-matching / non-map).
map_head_results() ->
    Big60 = maps:from_list([{k_atom(N), N} || N <- lists:seq(1, 60)]),
    Big60NoK50 = maps:remove(k50, Big60),
    [{{getv, match},    call(fun getv/1, #{a => 1, b => 2})},
     {{getv, no_match}, call(fun getv/1, #{a => 1})},
     {{getv, not_map},  call(fun getv/1, not_a_map)},
     {{has, match},     call(fun has/1, #{k => 7})},
     {{has, no_key},    call(fun has/1, #{x => 1})},
     {{has, not_map},   call(fun has/1, not_a_map)},
     {{big, match},     call(fun big/1, Big60)},
     {{big, no_key},    call(fun big/1, Big60NoK50)},
     {{big, not_map},   call(fun big/1, not_a_map)}].

%% Guard-BIF kernels. 2-arg kernels are pinned to a fixed second arg.
guard_bif_results() ->
    [{{fst, tuple},     call(fun fst/1, {a, b, c})},
     {{fst, not_tuple}, call(fun fst/1, not_a_tuple)},
     {{mg, match},      call(fun(M) -> mg(M, k) end, #{k => 7})},
     {{mg, no_key},     call(fun(M) -> mg(M, k) end, #{x => 1})},
     {{mg, not_map},    call(fun(M) -> mg(M, k) end, not_a_map)},
     {{msz, map},       call(fun msz/1, #{a => 1, b => 2})},
     {{msz, not_map},   call(fun msz/1, not_a_map)},
     {{hdk, list},      call(fun hdk/1, [1, 2, 3])},
     {{hdk, not_list},  call(fun hdk/1, not_a_list)},
     {{mk, match},      call(fun(M) -> mk(M, k) end, #{k => 1})},
     {{mk, no_key},     call(fun(M) -> mk(M, k) end, #{x => 1})},
     {{mk, not_map},    call(fun(M) -> mk(M, k) end, not_a_map)},
     {{bsz, bin},       call(fun bsz/1, <<"hello">>)},
     {{bsz, not_bin},   call(fun bsz/1, not_a_binary)}].

k_atom(N) ->
    list_to_atom("k" ++ integer_to_list(N)).

%% Normalize an exception to a comparable value: atom reasons pass
%% through, tuple reasons collapse to their tag (so {badmap,X} vs
%% {badkey,Y} etc. compare by tag, not by the offending term).
kind(A) when is_atom(A) -> A;
kind(T) when is_tuple(T) -> element(1, T);
kind(X) -> X.

call(F, Arg) ->
    try F(Arg) catch C:R -> {C, kind(R)} end.

%%%-------------------------------------------------------------------
%%% The kernels — EXACT shapes proven to trigger the T2 optimizations.
%%% Operand order matters: `foldl(...) + 1` triggers the body-site
%%% expander; `1 + foldl(...)` does NOT.
%%%-------------------------------------------------------------------

%% P1 tail fold-sites (cross-module lists:foldl, literal fun).
foldl_sum(L)   -> lists:foldl(fun(X, A) -> A + X end, 0, L).
foldl_count(L) -> lists:foldl(fun(X, A) when X > 0 -> A + 1; (_, A) -> A end, 0, L).

%% P1 body (non-tail) fold-sites (fold result consumed by continuation).
foldl_body(L)       -> lists:foldl(fun(X, A) -> A + X end, 0, L) + 1.
foldl_body_count(L) -> lists:foldl(fun(X, A) when X > 0 -> A + 1; (_, A) -> A end, 0, L) + 1.

%% P1 non-tail via opaque blocker (phash2(ok,1) is always 0 but blocks
%% the tail-rewrite).
foldl_sum_nt(L)   -> R = lists:foldl(fun(X, A) -> A + X end, 0, L), R + erlang:phash2(ok, 1).
foldl_count_nt(L) -> R = lists:foldl(fun(X, A) when X > 0 -> A + 1; (_, A) -> A end, 0, L), R + erlang:phash2(ok, 1).

%% maps:fold (flatmap when <=32 keys).
mapsfold_sum(M) -> maps:fold(fun(_K, V, A) -> A + V end, 0, M).
mapsfold_kv(M)  -> maps:fold(fun(K, V, A) -> A + K + V end, 0, M).

%% P1b local same-module pass-through wrapper (arity-change apply_fold/2
%% -> foldl/3).
apply_fold(F, L) -> lists:foldl(F, 0, L).
ptcaller(L)      -> apply_fold(fun(X, A) -> A + X end, L).

%% Eligibility: map-matching heads (is_map + get_map_elements + element).
getv(#{a := X, b := Y}) -> X + Y.
has(#{k := V}) -> {ok, V}; has(M) when is_map(M) -> no_key; has(_) -> not_a_map.
big(#{k50 := V}) -> V.   %% >32-key map -> hashmap lookup

%% Eligibility: guard-BIFs.
fst(T) -> element(1, T).
mg(M, K) -> map_get(K, M).
msz(M) -> map_size(M).
hdk(L) -> hd(L).
mk(M, K) when is_map_key(K, M) -> yes; mk(_, _) -> no.
bsz(B) -> byte_size(B).

%%%-------------------------------------------------------------------
%%% Helpers
%%%-------------------------------------------------------------------

is_aarch64() ->
    Arch = erlang:system_info(system_architecture),
    string:find(Arch, "aarch64") =/= nomatch orelse
        string:find(Arch, "arm64") =/= nomatch.
