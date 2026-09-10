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
-module(erl_features_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0]).

-export([init_overlong_feature_arg/1,
         features_in_safe_decode/1]).

all() ->
    [init_overlong_feature_arg,
     features_in_safe_decode].

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,1}}].

init_overlong_feature_arg(Config) when is_list(Config) ->
    Overlong = lists:duplicate(300, $a),
    {ok, Peer, Node} =
        ?CT_PEER(#{args => ["-enable-feature", Overlong],
                   connection => 0}),
    Enabled = rpc:call(Node, erl_features, enabled, []),
    true = is_list(Enabled),
    {badrpc, {'EXIT', {badarg, _}}} =
        rpc:call(Node, erlang, list_to_existing_atom, [Overlong]),
    peer:stop(Peer),
    ok.

features_in_safe_decode(Config) when is_list(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    UniqueAtomName =
        lists:flatten(io_lib:format("erl_features_SUITE_unique_~p_~p",
                                    [erlang:system_time(),
                                     erlang:unique_integer([positive])])),
    BeamFile = build_beam_with_meta(
                 PrivDir, erl_features_safe_decode_stub,
                 [{enabled_features, [list_to_atom(UniqueAtomName)]}]),
    {ok, Peer, Node} = ?CT_PEER(#{connection => 0}),
    try
        {badrpc, {'EXIT', {badarg, _}}} =
            rpc:call(Node, erlang, list_to_existing_atom, [UniqueAtomName]),
        {badrpc, {'EXIT', {badarg, _}}} =
            rpc:call(Node, erl_features, used, [BeamFile]),
        {badrpc, {'EXIT', {badarg, _}}} =
            rpc:call(Node, erlang, list_to_existing_atom, [UniqueAtomName])
    after
        peer:stop(Peer)
    end,
    ok.

build_beam_with_meta(PrivDir, ModuleName, MetaTerm) ->
    Forms = [{attribute, 1, module, ModuleName},
             {attribute, 2, export, []},
             {eof, 3}],
    {ok, ModuleName, BeamBin} = compile:forms(Forms, [binary, return_errors]),
    {ok, ModuleName, ChunkList} = beam_lib:all_chunks(BeamBin),
    MetaBin = erlang:term_to_binary(MetaTerm),
    Chunks = lists:keystore("Meta", 1, ChunkList, {"Meta", MetaBin}),
    {ok, NewBeam} = beam_lib:build_module(Chunks),
    File = filename:join(PrivDir, atom_to_list(ModuleName) ++ ".beam"),
    ok = file:write_file(File, NewBeam),
    File.
