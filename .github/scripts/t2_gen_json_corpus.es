#!/usr/bin/env escript
%%! -noshell

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

%% Generate a synthetic nativejson-shaped corpus (twitter=string-heavy,
%% citm_catalog=structure-heavy, canada=number-heavy) for the T2 G-bin
%% json:decode/1 scan benches (PLAN/verification/t2_gbin.erl). The real
%% nativejson-benchmark files are not vendored; these mirror their shapes so
%% the off/on decode *ratio* (the rule-5 signal) is representative. Fixed seed
%% -> deterministic across runs.
%%
%%   escript t2_gen_json_corpus.es <output-dir>

main([Dir]) ->
    _ = file:make_dir(Dir),
    rand:seed(exsss, {7, 8, 9}),
    ok = w(Dir, "twitter.json", json:encode(twitter(3000))),
    ok = w(Dir, "citm_catalog.json", json:encode(citm(2500))),
    ok = w(Dir, "canada.json", json:encode(canada(1500))),
    [io:format("~s: ~.2f MB~n",
               [F, filelib:file_size(filename:join(Dir, F)) / 1.0e6])
     || F <- ["twitter.json", "citm_catalog.json", "canada.json"]],
    ok;
main(_) ->
    io:format("usage: t2_gen_json_corpus.es <output-dir>~n"),
    halt(1).

w(Dir, F, IoData) ->
    file:write_file(filename:join(Dir, F), iolist_to_binary(IoData)).

twitter(N) ->
    #{<<"statuses">> =>
          [#{<<"text">> => txt(80 + rand:uniform(120)),
             <<"id">> => rand:uniform(1 bsl 50),
             <<"retweet_count">> => rand:uniform(10000),
             <<"user">> => #{<<"name">> => txt(10 + rand:uniform(20)),
                             <<"screen_name">> => txt(8 + rand:uniform(12)),
                             <<"description">> => txt(40 + rand:uniform(100)),
                             <<"followers_count">> => rand:uniform(1000000)},
             <<"lang">> => <<"en">>,
             <<"truncated">> => false}
           || _ <- lists:seq(1, N)]}.

canada(N) ->
    #{<<"type">> => <<"FeatureCollection">>,
      <<"features">> =>
          [#{<<"type">> => <<"Feature">>,
             <<"geometry">> =>
                 #{<<"type">> => <<"Polygon">>,
                   <<"coordinates">> =>
                       [[[(rand:uniform(360000) - 180000) / 1000,
                          (rand:uniform(180000) - 90000) / 1000]
                         || _ <- lists:seq(1, 40)]]}}
           || _ <- lists:seq(1, N)]}.

citm(N) ->
    #{<<"events">> =>
          maps:from_list(
            [{integer_to_binary(rand:uniform(1 bsl 40)),
              #{<<"id">> => rand:uniform(1 bsl 40),
                <<"name">> => txt(20 + rand:uniform(30)),
                <<"subTopicIds">> => [rand:uniform(1000) || _ <- lists:seq(1, 8)],
                <<"performances">> =>
                    [#{<<"eventId">> => rand:uniform(1 bsl 40),
                       <<"start">> => rand:uniform(1 bsl 50),
                       <<"prices">> =>
                           [#{<<"amount">> => rand:uniform(50000),
                              <<"audienceSubCategoryId">> => rand:uniform(100)}
                            || _ <- lists:seq(1, 5)]}
                     || _ <- lists:seq(1, 3)]}}
             || _ <- lists:seq(1, N)])}.

txt(L) ->
    unicode:characters_to_binary(
      [case rand:uniform(20) of
           1 -> 32; 2 -> $!; 3 -> $,; 4 -> $.;
           _ -> $a + rand:uniform(25)
       end || _ <- lists:seq(1, L)]).
