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
-module(ct_cover_to_html).
-moduledoc false.

%%% Convert the per-testcase coverage artifacts written by cth_coverage
%%% into an interactive HTML *attribution* report: for every instrumented
%%% source line, which test cases executed it.
%%%
%%% Unlike the merged LCOV output produced by ct_cover_to_lcov, this
%%% report keeps the per-testcase dimension. Each input file
%%%
%%%    <Suite>.<Case>.coverdata = term_to_binary([{Module, [{Line, Count}]}])
%%%
%%% (as written by cth_coverage, with only executed lines present) is
%%% attributed to the test case named by its file basename. The report
%%% therefore answers "which test cases cover this line", aggregated
%%% across every test job that touched the module -- including test
%%% cases from other applications, which is what makes it a cross-app
%%% coverage view.
%%%
%%% The line manifest written by cth_coverage
%%% (<dir>/coverage.manifest = term_to_binary([{Module, [Line]}]) with
%%% ALL instrumented lines) is used, when supplied via {manifest, File},
%%% to mark instrumented-but-unexecuted lines (the coverage gaps) in red.
%%%
%%% One HTML page is written per module plus an index. Source is located
%%% best-effort from the loaded module's compile info; when it cannot be
%%% found the page still shows the attribution, without the source text.

-export([convert/2, convert/3, main/1]).

%% A single hot line can be reached by very many test cases; cap how many
%% names are embedded per line to keep the report a bounded size. The true
%% count is always shown; the surplus is reported as "+N more".
-define(DEFAULT_MAX_TESTS_PER_LINE, 200).
-define(DEFAULT_TITLE, "Erlang/OTP coverage attribution").

-doc """
Read all `InFiles` (one per test case) and write an interactive HTML
attribution report to `OutDir`.

Unreadable or malformed input files are skipped with a warning on
standard error.
""".
-spec convert(InFiles, OutDir) -> ok | {error, term()} when
      InFiles :: [file:filename()],
      OutDir :: file:filename().
convert(InFiles, OutDir) ->
    convert(InFiles, OutDir, []).

-doc """
As `convert/2`, with options.

* `{manifest, ManifestFile}` names the line manifest written by
  cth_coverage; its instrumented lines that no test case executed are
  rendered as un-hit. An unreadable or malformed manifest is an error.
* `{max_tests_per_line, N}` caps how many test-case names are embedded
  per line (200 by default); the true count is always shown.
* `{title, String}` sets the report title.
* `{source_map, #{module() => filename()}}` overrides source-file
  resolution for the given modules (otherwise the source is located from
  the module's compile info / code path).
""".
-spec convert(InFiles, OutDir, Opts) -> ok | {error, term()} when
      InFiles :: [file:filename()],
      OutDir :: file:filename(),
      Opts :: [Opt],
      Opt :: {manifest, file:filename()}
           | {max_tests_per_line, pos_integer()}
           | {title, string()}
           | {source_map, #{module() => file:filename()}}.
convert(InFiles, OutDir, Opts) when is_list(InFiles), is_list(Opts) ->
    case load_manifest(proplists:get_value(manifest, Opts)) of
        {ok, Manifest} ->
            Cfg = #{max => proplists:get_value(max_tests_per_line, Opts,
                                               ?DEFAULT_MAX_TESTS_PER_LINE),
                    title => proplists:get_value(title, Opts, ?DEFAULT_TITLE),
                    smap => proplists:get_value(source_map, Opts, #{})},
            Attr = lists:foldl(fun attribute_file/2, #{}, InFiles),
            write_report(OutDir, Attr, Manifest, Cfg);
        {error, _} = Error ->
            Error
    end.

-doc """
Escript entry point.

Usage: `ct_cover_to_html [--manifest <file>] [--max-per-line <n>]
[--title <t>] <out-dir> (<coverdata-dir> | <in1.coverdata> ...)`
""".
-spec main(Args :: [string()]) -> ok | no_return().
main(Args) ->
    main(Args, []).

main(["--manifest", F | Rest], Opts) ->
    main(Rest, [{manifest, F} | Opts]);
main(["--max-per-line", N | Rest], Opts) ->
    main(Rest, [{max_tests_per_line, list_to_integer(N)} | Opts]);
main(["--title", T | Rest], Opts) ->
    main(Rest, [{title, T} | Opts]);
main([OutDir, MaybeDir], Opts) ->
    InFiles =
        case filelib:is_dir(MaybeDir) of
            true -> filelib:wildcard(filename:join(MaybeDir, "*.coverdata"));
            false -> [MaybeDir]
        end,
    run(InFiles, OutDir, Opts);
main([OutDir | InFiles], Opts) when InFiles =/= [] ->
    run(InFiles, OutDir, Opts);
main(_, _Opts) ->
    io:format(standard_error,
              "usage: ct_cover_to_html [--manifest <file>] [--max-per-line <n>] "
              "[--title <t>] <out-dir> (<coverdata-dir> | <in.coverdata> ...)~n",
              []),
    erlang:halt(1).

run([], _OutDir, _Opts) ->
    io:format(standard_error,
              "ct_cover_to_html: error: no input .coverdata files~n", []),
    erlang:halt(1);
run(InFiles, OutDir, Opts) ->
    case convert(InFiles, OutDir, Opts) of
        ok ->
            ok;
        {error, Reason} ->
            io:format(standard_error,
                      "ct_cover_to_html: error: ~tp~n", [Reason]),
            erlang:halt(1)
    end.

%%%-----------------------------------------------------------------
%%% Reading and attributing

%% Attr :: #{Module => #{Line => [TestId]}}
attribute_file(File, Acc) ->
    Tid = filename:basename(File, ".coverdata"),
    case file:read_file(File) of
        {ok, Bin} ->
            try binary_to_term(Bin) of
                Data when is_list(Data) ->
                    try lists:foldl(fun(E, A) -> attribute_module(Tid, E, A) end,
                                    Acc, Data)
                    catch _:_ -> warn(File, malformed_coverdata), Acc
                    end;
                _ ->
                    warn(File, malformed_coverdata), Acc
            catch error:badarg ->
                    warn(File, not_external_term_format), Acc
            end;
        {error, Reason} ->
            warn(File, Reason), Acc
    end.

attribute_module(Tid, {Module, Lines}, Acc)
  when is_atom(Module), is_list(Lines) ->
    ModMap = lists:foldl(fun(L, A) -> attribute_line(Tid, L, A) end,
                         maps:get(Module, Acc, #{}), Lines),
    Acc#{Module => ModMap}.

attribute_line(Tid, {Line, Count}, ModMap)
  when is_integer(Line), Line > 0, is_integer(Count), Count > 0 ->
    maps:update_with(Line, fun(Ts) -> [Tid | Ts] end, [Tid], ModMap);
attribute_line(_Tid, {Line, _Count}, ModMap) when is_integer(Line) ->
    ModMap.

warn(File, Reason) ->
    io:format(standard_error,
              "ct_cover_to_html: warning: skipping ~ts: ~tp~n",
              [File, Reason]).

%%%-----------------------------------------------------------------
%%% Line manifest (all instrumented lines, executed or not)

%% Reused shape from ct_cover_to_lcov: #{Module => [Line]} sorted/unique.
load_manifest(undefined) ->
    {ok, #{}};
load_manifest(File) ->
    case file:read_file(File) of
        {ok, Bin} ->
            try binary_to_term(Bin) of
                Data when is_list(Data) ->
                    try {ok, lists:foldl(fun manifest_module/2, #{}, Data)}
                    catch _:_ -> {error, {manifest, File, malformed_manifest}}
                    end;
                _ ->
                    {error, {manifest, File, malformed_manifest}}
            catch error:badarg ->
                    {error, {manifest, File, not_external_term_format}}
            end;
        {error, Reason} ->
            {error, {manifest, File, Reason}}
    end.

manifest_module({Module, Lines}, Acc) when is_atom(Module), is_list(Lines) ->
    Good = lists:usort([L || L <- Lines, is_integer(L), L > 0]),
    maps:update_with(Module, fun(Ls) -> lists:umerge(Good, Ls) end, Good, Acc).

%%%-----------------------------------------------------------------
%%% Report

write_report(OutDir, Attr, Manifest, Cfg) ->
    ok = filelib:ensure_dir(filename:join(OutDir, "index.html")),
    Mods = lists:usort(maps:keys(Attr) ++ maps:keys(Manifest)),
    _ = [render_module(M, OutDir, maps:get(M, Attr, #{}),
                       maps:get(M, Manifest, []), Cfg) || M <- Mods],
    render_index(OutDir, Mods, Attr, Manifest, all_tests(Attr), Cfg),
    ok.

all_tests(Attr) ->
    lists:usort(
      maps:fold(fun(_M, LM, Acc) ->
                        maps:fold(fun(_L, Ts, A) -> Ts ++ A end, Acc, LM)
                end, [], Attr)).

%% Hit = every line some test case executed (a hit line is, by definition,
%% instrumented). Instr = the instrumented lines, i.e. the manifest unioned
%% with the hit lines, so the ratio is correct even with no manifest.
stats(AttrM, ManL) ->
    Union = lists:foldl(fun(L, A) -> A#{L => true} end,
                        maps:from_keys(ManL, true), maps:keys(AttrM)),
    {maps:size(AttrM), maps:size(Union)}.

pct(_, 0) -> 100.0;
pct(H, I) -> 100.0 * H / I.

%%%---------------- index ----------------
render_index(OutDir, Mods, Attr, Manifest, Tests, #{title := Title}) ->
    Rows = [index_row(M, maps:get(M, Attr, #{}), maps:get(M, Manifest, []))
            || M <- Mods],
    {TH, TI} = lists:foldl(
                 fun(M, {H0, I0}) ->
                         {H, I} = stats(maps:get(M, Attr, #{}),
                                        maps:get(M, Manifest, [])),
                         {H0 + H, I0 + I}
                 end, {0, 0}, Mods),
    Html =
        ["<!doctype html><meta charset=utf-8><title>", esc(Title), "</title>",
         css(),
         "<h1>", esc(Title), "</h1>",
         io_lib:format("<p class=sub>~w modules &middot; ~w test cases &middot; "
                       "overall <b>~.1f%</b> (~w/~w lines)</p>",
                       [length(Mods), length(Tests), pct(TH, TI), TH, TI]),
         "<table class=idx><thead><tr><th>Module</th><th>Lines</th>"
         "<th>Coverage</th><th></th></tr></thead><tbody>", Rows,
         "</tbody></table>"],
    write_utf8(filename:join(OutDir, "index.html"), Html).

index_row(M, AttrM, ManL) ->
    {H, I} = stats(AttrM, ManL),
    P = pct(H, I),
    io_lib:format(
      "<tr><td><a href=\"~ts.html\">~ts</a></td>"
      "<td class=num>~w/~w</td><td class=num>~.1f%</td>"
      "<td class=barcell><div class=bar><div class=fill "
      "style=\"width:~.1f%;background:~ts\"></div></div></td></tr>",
      [M, M, H, I, P, P, barcolor(P)]).

barcolor(P) when P >= 75 -> "#4caf50";
barcolor(P) when P >= 40 -> "#ffb300";
barcolor(_) -> "#e53935".

%%%---------------- per-module page ----------------
render_module(M, OutDir, AttrM, ManL, #{max := Max, smap := SMap}) ->
    ManSet = maps:from_keys(ManL, true),
    Lines = source_lines(M, SMap),
    {H, I} = stats(AttrM, ManL),
    Body = [render_line(N, Text, AttrM, ManSet)
            || {N, Text} <- lists:enumerate(Lines)],
    Cov = cov_json(AttrM, Max),
    Html =
        ["<!doctype html><meta charset=utf-8><title>", atom_to_list(M),
         " coverage</title>", css(),
         io_lib:format("<div class=top><a href=\"index.html\">&larr; all modules</a>"
                       " &nbsp; <b>~ts</b> &nbsp; <span class=sub>~.1f% (~w/~w lines)"
                       "</span></div>", [M, pct(H, I), H, I]),
         "<div class=legend><span class=k><i class='sw hit'></i>covered</span>"
         "<span class=k><i class='sw miss'></i>instrumented, not hit</span>"
         "<span class=k>click a covered line &rarr; its test cases; "
         "type below &rarr; highlight a test case's lines</span></div>",
         "<div class=wrap><div class=code><table>", Body, "</table></div>",
         "<div class=panel><div class=pctl>highlight test case: "
         "<input id=filter type=text placeholder='substring' "
         "oninput=\"filterTest(this.value)\"></div>"
         "<div id=panel class=pbody>Click a "
         "<span class=hit style='padding:0 4px'>covered</span> line.</div>"
         "</div></div>",
         "<script>var COV=", Cov, ";", js(), "</script>"],
    write_utf8(filename:join(OutDir, atom_to_list(M) ++ ".html"), Html).

render_line(N, Text, AttrM, ManSet) ->
    Esc = esc(Text),
    case maps:get(N, AttrM, undefined) of
        undefined ->
            Cls = case maps:is_key(N, ManSet) of true -> "miss"; false -> "none" end,
            io_lib:format("<tr id=L~w class=~ts><td class=ln>~w</td>"
                          "<td class=cnt></td><td class=src>~ts</td></tr>",
                          [N, Cls, N, Esc]);
        Tids ->
            K = length(lists:usort(Tids)),
            io_lib:format("<tr id=L~w class=hit onclick=\"showTests(~w)\">"
                          "<td class=ln>~w</td><td class=cnt title='~w test cases'>~w</td>"
                          "<td class=src>~ts</td></tr>", [N, N, N, K, K, Esc])
    end.

%% COV[Line] = {n: true unique count, t: [up to Max sorted test ids]}
cov_json(AttrM, Max) ->
    Entries =
        [begin
             U = lists:usort(Ts),
             T = lists:sublist(U, Max),
             io_lib:format("\"~w\":{\"n\":~w,\"t\":[~ts]}",
                           [L, length(U),
                            lists:join(",", [json_str(X) || X <- T])])
         end || {L, Ts} <- lists:sort(maps:to_list(AttrM))],
    ["{", lists:join(",", Entries), "}"].

%%%---------------- source ----------------
source_lines(M, SMap) ->
    case source_file(M, SMap) of
        undefined ->
            ["%% (source not available for " ++ atom_to_list(M) ++ ")"];
        Path ->
            case file:read_file(Path) of
                {ok, Bin} ->
                    string:split(unicode:characters_to_list(Bin), "\n", all);
                _ ->
                    ["%% (source not available for " ++ atom_to_list(M) ++ ")"]
            end
    end.

%% Best-effort source resolution, mirroring ct_cover_to_lcov, with an
%% explicit source_map override taking precedence.
source_file(Module, SMap) ->
    case maps:get(Module, SMap, undefined) of
        undefined -> source_from_info(Module);
        Path -> Path
    end.

source_from_info(Module) ->
    Source = try Module:module_info(compile) of
                 Info when is_list(Info) -> proplists:get_value(source, Info);
                 _ -> undefined
             catch _:_ -> undefined
             end,
    case Source of
        [_ | _] ->
            Abs = filename:absname(Source),
            case filelib:is_regular(Abs) of
                true -> Abs;
                false -> source_from_beam(Module)
            end;
        _ ->
            source_from_beam(Module)
    end.

source_from_beam(Module) ->
    try code:which(Module) of
        Beam when is_list(Beam), Beam =/= [] ->
            AppDir = filename:dirname(filename:dirname(Beam)),
            Src = filename:join([AppDir, "src", atom_to_list(Module) ++ ".erl"]),
            case filelib:is_regular(Src) of
                true -> filename:absname(Src);
                false -> undefined
            end;
        _ ->
            undefined
    catch _:_ ->
            undefined
    end.

%%%---------------- helpers ----------------
write_utf8(File, Iodata) ->
    file:write_file(File, unicode:characters_to_binary(Iodata)).

esc(Text) ->
    lists:map(fun($<) -> "&lt;";
                 ($>) -> "&gt;";
                 ($&) -> "&amp;";
                 (C) -> C
              end, lists:flatten(Text)).

json_str(S) ->
    ["\"", lists:map(fun($") -> "\\\"";
                        ($\\) -> "\\\\";
                        (C) -> C
                     end, lists:flatten(S)), "\""].

%%%---------------- assets ----------------
css() ->
    "<style>"
    "body{font:13px/1.5 -apple-system,Segoe UI,Roboto,sans-serif;margin:0;color:#222;background:#fafafa}"
    "h1{font-size:20px;margin:16px 20px 4px}.sub{color:#777}"
    ".top{padding:10px 20px;background:#fff;border-bottom:1px solid #ddd;position:sticky;top:0;z-index:2}"
    ".top a{color:#1565c0;text-decoration:none}"
    ".legend{padding:6px 20px;color:#666;background:#f2f2f2;border-bottom:1px solid #e0e0e0}"
    ".legend .k{margin-right:18px}.sw{display:inline-block;width:11px;height:11px;vertical-align:-1px;margin-right:5px;border-radius:2px}"
    ".sw.hit{background:#c8e6c9}.sw.miss{background:#ffcdd2}"
    ".wrap{display:flex;align-items:flex-start}"
    ".code{flex:1;overflow-x:auto;background:#fff}"
    ".code table{border-collapse:collapse;width:100%;font:12px/1.5 SFMono-Regular,Menlo,Consolas,monospace}"
    ".code td{padding:0 8px;white-space:pre}"
    ".ln{text-align:right;color:#999;background:#f6f6f6;user-select:none;border-right:1px solid #eee}"
    ".cnt{text-align:right;color:#2e7d32;background:#f6f6f6;user-select:none;min-width:20px}"
    "tr.hit .src{background:#e8f5e9}tr.hit{cursor:pointer}tr.hit:hover .src{background:#d3ecd4}"
    "tr.miss .src{background:#ffebee}"
    "tr.sel .src{background:#fff59d!important}tr.hi .src{background:#bbdefb!important}"
    ".panel{width:320px;position:sticky;top:52px;align-self:flex-start;background:#fff;border-left:1px solid #ddd;height:calc(100vh - 52px);overflow:auto}"
    ".pctl{padding:10px;border-bottom:1px solid #eee;color:#555}"
    ".pctl input{width:96%;padding:3px}"
    ".pbody{padding:12px}.pbody ul{margin:8px 0 0;padding-left:18px}.pbody li{margin:2px 0;word-break:break-all}"
    ".idx{border-collapse:collapse;margin:12px 20px}.idx th,.idx td{padding:4px 12px;border-bottom:1px solid #eee;text-align:left}"
    ".idx .num{text-align:right;font-variant-numeric:tabular-nums}.idx a{color:#1565c0;text-decoration:none}"
    ".bar{width:160px;height:10px;background:#eee;border-radius:5px;overflow:hidden}.fill{height:100%}"
    "</style>".

js() ->
    "function clr(c){var e=document.getElementsByClassName(c);"
    "while(e.length)e[0].classList.remove(c);}"
    "function showTests(l){clr('sel');var r=document.getElementById('L'+l);"
    "if(r)r.classList.add('sel');var o=COV[l]||{n:0,t:[]};var t=o.t.slice().sort();"
    "var more=o.n-t.length;"
    "var h='<b>Line '+l+'</b> covered by '+o.n+' test case(s):<ul>'+"
    "t.map(function(x){return '<li>'+x+'</li>';}).join('')+'</ul>'+"
    "(more>0?('<p class=sub>+'+more+' more (raise --max-per-line to list all)</p>'):'');"
    "document.getElementById('panel').innerHTML=h;}"
    "function filterTest(s){clr('hi');if(!s)return;"
    "for(var l in COV){if(COV[l].t.some(function(x){return x.indexOf(s)>=0;})){"
    "var e=document.getElementById('L'+l);if(e)e.classList.add('hi');}}}".
