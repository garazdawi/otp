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
-module(edlin_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0, init_per_testcase/2]).
-export([eof_multiline_cursor/1, insert_search_with_aft/1]).

suite() ->
    [{timetrap, {minutes, 1}}].

all() ->
    [eof_multiline_cursor, insert_search_with_aft].

init_per_testcase(_Case, Config) ->
    edlin:init(),
    Config.

%% The eof handler must base the final horizontal move on the last line
%% of LA (where the cursor lands), not the first.
eof_multiline_cursor(_Config) ->
    Cont = {line, "P> ", {[], {[], []}, ["ab", "ghijk"]}, {normal, none}},
    {done, _, [], [{move_combo, 0, 2, 5}]} = edlin:edit_line(eof, Cont),
    ok.

%% A printable key in search mode with non-empty Aft used to raise
%% case_clause because do_op returned a 3-tuple the caller didn't match.
insert_search_with_aft(_Config) ->
    Cont = {line, "P> ", {[], {[], "tail"}, []}, {search, none}},
    {search, _, {line, _, {[], {"x", []}, []}, _}, Rs} =
        edlin:edit_line("x", Cont),
    true = lists:member({insert_chars, unicode, "x"}, Rs),
    true = lists:member(delete_after_cursor, Rs),
    ok.
