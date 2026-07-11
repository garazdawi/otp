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
%%% T2-Full result parity (PLAN/T2FULL/18 wave W4; retrospective 16 §7
%%% P0 "bank the parity harnesses as a committed CT suite").
%%%
%%% Two peers run identical deterministic kernels: a control peer
%%% (plain T1) and a forced-tier peer (+JT2enable true with the
%%% install-quality gate off, so even losing blobs install and
%%% execute). The kernels cover the tier's own turf — integer/float
%%% tail loops, byte scanning, byte-aligned bs_match lexing — plus the
%%% exception path. Parity is asserted on erlang:phash2 of the full
%%% results; an install floor on the forced peer keeps the comparison
%%% from going vacuous. aarch64-only: elsewhere both peers would be
%%% T1 and the comparison meaningless.
%%%-------------------------------------------------------------------
-module(t2_parity_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([suite/0, all/0,
         init_per_suite/1, end_per_suite/1]).

-export([kernel_parity/1]).

%% Run on the peers (by name, over erpc).
-export([run_kernels/0]).

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes, 3}}].

all() ->
    [kernel_parity].

init_per_suite(Config) ->
    case erlang:system_info(emu_flavor) of
        jit ->
            Arch = erlang:system_info(system_architecture),
            case string:find(Arch, "aarch64") =/= nomatch orelse
                 string:find(Arch, "arm64") =/= nomatch of
                true -> Config;
                false -> {skip, "T2 backend emits on aarch64 only"}
            end;
        _ ->
            {skip, "T2 exists only in the JIT emulator"}
    end.

end_per_suite(_Config) ->
    ok.

kernel_parity(_Config) ->
    PA = filename:dirname(code:which(?MODULE)),
    {ok, CtrlPeer, CtrlNode} =
        peer:start(#{name => peer:random_name(?MODULE),
                     args => ["-pa", PA]}),
    {ok, T2Peer, T2Node} =
        peer:start(#{name => peer:random_name(?MODULE),
                     env => [{"T2_INSTALL_GATE", "0"}],
                     args => ["-pa", PA, "+JT2enable", "true"]}),
    try
        Ctrl = erpc:call(CtrlNode, ?MODULE, run_kernels, [], 120000),
        T2 = erpc:call(T2Node, ?MODULE, run_kernels, [], 120000),
        %% Per-kernel comparison so a divergence names its kernel.
        Pairs = lists:zip(Ctrl, T2),
        Diverged =
            [{Name, CH, TH}
             || {{Name, CH}, {Name2, TH}} <- Pairs,
                Name =:= Name2,
                CH =/= TH],
        ct:log("control: ~p~nforced-t2: ~p", [Ctrl, T2]),
        [] = Diverged,
        true = length(Ctrl) =:= length(T2),
        %% Non-vacuousness: the forced peer must actually have
        %% installed a healthy population of blobs.
        _ = erpc:call(T2Node, erts_debug, set_internal_state,
                      [available_internal_state, true]),
        Stats = erpc:call(T2Node, erts_debug, get_internal_state,
                          [t2_stats]),
        Installed = element(3, Stats),
        ct:log("forced-t2 t2_stats: ~p (installed=~p)", [Stats, Installed]),
        true = Installed >= 100
    after
        catch peer:stop(CtrlPeer),
        catch peer:stop(T2Peer)
    end,
    ok.

%%%-------------------------------------------------------------------
%%% The kernels (peer side). All inputs deterministic; each returns
%%% {Name, phash2(Result)}.
%%%-------------------------------------------------------------------

run_kernels() ->
    {module, ?MODULE} = code:ensure_loaded(?MODULE),
    Bin = mk_corpus_bin(),
    List = lists:seq(1, 50000),
    Floats = [X * 0.5 || X <- lists:seq(1, 10000)],
    Kernels =
        [{tsum, fun() -> tsum(List, 0) end},
         {fsum, fun() -> fsum(Floats, 0.0) end},
         {byte_scan, fun() -> byte_scan(Bin, 0, 0, 0) end},
         {word_lex, fun() -> word_lex(Bin, 0, [] , 0) end},
         {stdlib_mix,
          fun() ->
                  M = maps:from_list([{N, N * N} || N <- lists:seq(1, 5000)]),
                  {lists:sort(maps:values(M)),
                   lists:reverse(List),
                   string:to_upper("t2 parity"),
                   base64:encode(binary:part(Bin, 0, 3000))}
          end},
         {exceptions,
          fun() ->
                  A = (catch maps:from_keys([a | b], v)),
                  B = (catch tsum(not_a_list, 0)),
                  C = try binary:part(Bin, byte_size(Bin), 1)
                      catch error:R -> R
                      end,
                  {A, B, C}
          end}],
    [{Name, erlang:phash2(F(), 1 bsl 27)} || {Name, F} <- Kernels].

%% Integer tail loop — the tier's bread and butter.
tsum([], Acc) -> Acc;
tsum([H | T], Acc) -> tsum(T, Acc + H).

%% Float tail loop.
fsum([], Acc) -> Acc;
fsum([H | T], Acc) -> fsum(T, Acc + H).

%% Byte scan-and-count (scanbench-shaped): digits, spaces, other.
byte_scan(<<C, Rest/binary>>, D, S, O) when C >= $0, C =< $9 ->
    byte_scan(Rest, D + 1, S, O);
byte_scan(<<C, Rest/binary>>, D, S, O) when C =:= $\s; C =:= $\n ->
    byte_scan(Rest, D, S + 1, O);
byte_scan(<<_, Rest/binary>>, D, S, O) ->
    byte_scan(Rest, D, S, O + 1);
byte_scan(<<>>, D, S, O) ->
    {D, S, O}.

%% Word lexer (lex_wl-shaped): word-length list + word count over the
%% corpus, byte-aligned bs_match all the way.
word_lex(<<C, Rest/binary>>, Len, Acc, N)
  when C =/= $\s, C =/= $\n ->
    word_lex(Rest, Len + 1, Acc, N);
word_lex(<<_, Rest/binary>>, 0, Acc, N) ->
    word_lex(Rest, 0, Acc, N);
word_lex(<<_, Rest/binary>>, Len, Acc, N) ->
    word_lex(Rest, 0, [Len | Acc], N + 1);
word_lex(<<>>, 0, Acc, N) ->
    {N, Acc};
word_lex(<<>>, Len, Acc, N) ->
    {N + 1, [Len | Acc]}.

%% Deterministic ~200kB text-shaped corpus, no randomness.
mk_corpus_bin() ->
    Chunk = <<"the quick brown fox 12345 jumped over 9876 lazy dogs\n">>,
    iolist_to_binary(lists:duplicate(4000, Chunk)).
