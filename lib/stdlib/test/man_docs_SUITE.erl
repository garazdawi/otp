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
-module(man_docs_SUITE).
-moduledoc false.

-export([all/0, suite/0]).
-export([equiv_grouping_merges_entries/1]).

-include_lib("kernel/include/eep48.hrl").

suite() ->
    [{timetrap,{minutes,1}}].

all() ->
    [equiv_grouping_merges_entries].

equiv_grouping_merges_entries(_Config) ->
    Equiv = <<"foo(X, default)">>,
    Doc = #{~"en" => ~"Shared documentation body."},
    Foo0 = {{function, foo, 0}, {1,1}, [~"foo()"], Doc, #{equiv => Equiv}},
    Foo1 = {{function, foo, 1}, {2,1}, [~"foo(X)"], Doc, #{equiv => Equiv}},
    Docs = #docs_v1{anno = {1,1},
                    beam_language = erlang,
                    format = ~"text/markdown",
                    module_doc = #{~"en" => ~"# fake_mod\n\nA fake module.\n\n## Description\n\nFor testing.\n"},
                    metadata = #{},
                    docs = [Foo0, Foo1]},
    Path = filename:join(code:lib_dir(stdlib), "src/fake_mod.erl"),
    Out = iolist_to_binary(man_docs:module_to_manpage(fake_mod, Path, Docs, "3")),
    [_, FunSection] = binary:split(Out, ~".SH FUNCTIONS\n"),
    [_] = binary:matches(FunSection, ~".B\n"),
    [Head, _] = binary:split(FunSection, ~".RS\n"),
    {_,_} = binary:match(Head, ~"foo()"),
    {_,_} = binary:match(Head, ~"foo(X)"),
    ok.
