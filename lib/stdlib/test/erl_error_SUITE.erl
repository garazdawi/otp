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
-module(erl_error_SUITE).

-export([all/0, suite/0]).
-export([custom_format_fun_no_brackets/1]).

suite() -> [{ct_hooks,[ts_install_cth]}].

all() -> [custom_format_fun_no_brackets].

%% Regression: a custom format_fun returning non-bracket-wrapped output
%% (e.g. a "..." truncation marker) must not crash the formatter.
custom_format_fun_no_brackets(Config) when is_list(Config) ->
    FF = fun(_Term, _I) -> "..." end,
    Bin1 = iolist_to_binary(
             erl_error:format_exception(
               error, badarg, [{some_mod, some_fun, [1,2,3], []}],
               #{format_fun => FF})),
    {_,_} = binary:match(Bin1, <<"...">>),
    Bin2 = iolist_to_binary(
             erl_error:format_call(1, {some_mod, some_fun}, [1,2,3], FF)),
    {_,_} = binary:match(Bin2, <<"...">>),
    ok.
