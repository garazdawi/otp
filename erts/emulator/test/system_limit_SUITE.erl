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

%% Tests that exhausting a fixed-size runtime table (the atom table and the
%% code tables: export, module, fun) raises a *catchable* error instead of
%% aborting the whole node. Each test fills the relevant table on a fresh
%% peer node and verifies the node survives and stays responsive.
-module(system_limit_SUITE).

-export([suite/0, all/0]).
-export([atom_table/1,
         export_table_decode/1,
         export_table_make_fun/1,
         code_load/1]).

%% Entry points executed on the peer node (must be exported for erpc).
-export([atom_table_body/0,
         export_decode_body/0,
         export_make_fun_body/0,
         code_load_body/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes, 3}}].

all() ->
    [atom_table, export_table_decode, export_table_make_fun, code_load].

%%%
%%% (1) A full atom table -> catchable system_limit / badarg, node survives.
%%%
atom_table(Config) when is_list(Config) ->
    %% +t chosen well above what a peer node interns at start so it boots and
    %% loads this module, yet low enough to fill quickly.
    {ok, Peer, Node} = ?CT_PEER(#{args => ["+t", "32768"]}),
    run_on_peer(Peer, Node, atom_table_body, []).

atom_table_body() ->
    %% Pre-warm every path so nothing lazy-loads once the table is full.
    _ = (catch binary_to_atom(<<"prewarm_b2a">>, utf8)),
    _ = (catch binary_to_term(term_to_binary(prewarm))),
    _ = (catch list_to_atom("prewarm_l2a")),
    _ = fill_atoms(0),
    %% Table is now at the limit; further atom creation must be catchable.
    {error, system_limit} = probe(fun() -> list_to_atom(novel("la")) end),
    {error, system_limit} =
        probe(fun() -> binary_to_atom(list_to_binary(novel("ba")), utf8) end),
    %% Decoding a novel atom from the external format -> graceful badarg.
    {error, badarg} = probe(fun() -> binary_to_term(novel_atom_ext()) end),
    ok.

fill_atoms(N) ->
    try list_to_atom("fillatom_" ++ integer_to_list(N)) of
        _ -> fill_atoms(N + 1)
    catch
        error:system_limit -> N
    end.

novel_atom_ext() ->
    N = list_to_binary(novel("dec")),
    <<131, 119, (byte_size(N)):8, N/binary>>.        % SMALL_ATOM_UTF8_EXT

%%%
%%% (2) A full export table via external-fun decode -> catchable badarg.
%%%
export_table_decode(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    run_on_peer(Peer, Node, export_decode_body, []).

export_decode_body() ->
    Last = fill_exports(0),
    %% A further novel m:f/A decode must fail with badarg, repeatably.
    {error, badarg} = probe(fun() -> binary_to_term(mkstub(Last + 1)) end),
    {error, badarg} = probe(fun() -> binary_to_term(mkstub(Last + 2)) end),
    ok.

%%%
%%% (3) A full export table via make_fun/3 and apply/3 -> catchable
%%%     system_limit (not a node abort, not undef).
%%%
export_table_make_fun(Config) when is_list(Config) ->
    {ok, Peer, Node} = ?CT_PEER(),
    run_on_peer(Peer, Node, export_make_fun_body, []).

export_make_fun_body() ->
    N = fill_exports(0),
    %% Derive the module atoms at runtime so the compiler cannot constant-fold
    %% make_fun/apply of a literal M:F/A into a compile-time literal fun (which
    %% would bypass the runtime export-stub path under test).
    ModMk = list_to_atom("novelmk_" ++ integer_to_list(N)),
    ModAp = list_to_atom("novelap_" ++ integer_to_list(N)),
    {error, system_limit} = probe(fun() -> erlang:make_fun(ModMk, f, 0) end),
    {error, system_limit} = probe(fun() -> apply(ModAp, f, []) end),
    ok.

%%%
%%% (4) A full export table -> load_module/code:load_binary refuses with a
%%%     catchable {error, system_limit} instead of aborting mid-load.
%%%
code_load(Config) when is_list(Config) ->
    Bin = compile_tiny(),
    {ok, Peer, Node} = ?CT_PEER(),
    run_on_peer(Peer, Node, code_load_body, [Bin]).

code_load_body(Bin) ->
    _ = fill_exports(0),
    {error, system_limit} = code:load_binary(tinymod, "tinymod.beam", Bin),
    ok.

compile_tiny() ->
    Forms = [{attribute, 1, module, tinymod},
             {attribute, 2, export, [{f, 0}]},
             {function, 3, f, 0, [{clause, 3, [], [], [{atom, 3, ok}]}]}],
    {ok, tinymod, Bin} = compile:forms(Forms, [binary]),
    Bin.

%% Run Body on the peer (which must not abort), then confirm the peer is still
%% alive and accepts new work via an atom-free computation. ?CT_PEER stays at
%% each call site so its ?FUNCTION_NAME-derived peer name reflects the case.
run_on_peer(Peer, Node, Body, Args) ->
    try
        ok = erpc:call(Node, ?MODULE, Body, Args),
        4 = erpc:call(Node, erlang, '+', [2, 2])
    after
        peer:stop(Peer)
    end.

%%%
%%% Shared helpers (run on the peer).
%%%

%% Fill the export table with distinct m:f/A stubs (varying arity interns only
%% the atoms 'm' and 'f', so the export table -- not the atom table -- fills).
%% Returns the arity at which decoding first failed.
fill_exports(A) ->
    case (catch binary_to_term(mkstub(A))) of
        Fun when is_function(Fun) -> fill_exports(A + 1);
        {'EXIT', {badarg, _}} -> A;
        Other -> exit({unexpected_fill_result, A, Other})
    end.

%% EXPORT_EXT external-fun binary for m:f/A.
mkstub(A) ->
    <<131, 113, 119, 1, $m, 119, 1, $f, 98, A:32/signed>>.

%% Run F, returning {Class, Reason} if it raises, or 'created' on success.
probe(F) ->
    try F() of
        _ -> created
    catch
        C:E -> {C, E}
    end.

novel(Prefix) ->
    Prefix ++ "_" ++ integer_to_list(erlang:unique_integer([positive])).
