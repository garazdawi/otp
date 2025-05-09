%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2004-2025. All Rights Reserved.
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
-module(test_server_skip_SUITE).

-export([all/1, init_per_suite/1, end_per_suite/1]).
-export([dummy/1]).

-include_lib("common_test/include/ct.hrl").

all(suite) ->
    [dummy].

init_per_suite(Config) when is_list(Config) ->
    {skip,"Skipping init_per_suite - check that \'dummy\' and"
     " \'end_per_suite\' are also skipped"}.

dummy(suite) -> [];
dummy(doc) -> ["This testcase should never be executed"];
dummy(Config) when is_list(Config) ->
    test_server:fail("This testcase should be executed since"
	    " init_per_suite/1 is skipped").

end_per_suite(doc) -> ["This testcase should never be executed"];
end_per_suite(Config) when is_list(Config) ->
    test_server:fail("end_per_suite/1 should not be executed when"
	    " init_per_suite/1 is skipped").
