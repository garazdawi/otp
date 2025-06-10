%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2025. All Rights Reserved.
%% Copyright 2020-2024 Facebook, Inc. and its affiliates.
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

%% Notes:
%% * Sorting of warnings/errors in the compiler is a bit weird
%%   - First they are partitioned into their types (i.e. error/warning)
%%   - Then the `Type` in erl_scan, epp and erl_parse are printed sorted
%%     by their filename + anno.
%%   - Then any other `Type` is printed, sorted by their filename +
%%     the order returned from the pass.
%%     - The order returned from each pass varies depending on pass, but
%%       erl_lint for example sorts errors by reversed insertion order and
%%       warnings by location (aka line number).
%% 
%%   How do we want sorting? Ideally we would like it sorted on order of
%%   relevance, I assume that is why erl_scan, epp, erl_parse are printed
%%   first as any error there is probably what you want to fix first.
%% 
%%   Many other compilers seem to mix errors and warnings and they are emitted
%%   in pass order + insertion order (which is usually encounter order).
-module(erl_diagnostic).
-moduledoc """
This module provides infrastructure for collecting and reporting diagnostics
such as errors, warnings, informational messages, and hints during compilation
or code analysis.

It supports:
- Declarative definition of diagnostics with custom formatting functions.
- Tracking diagnostic states across different types (error, warning, info, hint).
- Emission and management of diagnostics including enabling/disabling, 
  overrides, and legacy formats.
- Rich formatting output including plain text, ANSI-colored terminal output,
  and JSON (with optional links to documentation).
- Utilities for attaching related source locations, underlining source ranges,
  and emitting helpful suggestions or notes.

Diagnostics can be reported through various APIs, including a modern map-based
system (`emit/4`, `emit/5`, etc.) and a legacy format (`emit_legacy/3`).

This system is designed to be extensible, efficient, and suitable for both 
machine-readable outputs (e.g., for editors or LSPs) and human-friendly 
terminal displays.
""".
-moduledoc(#{since => "OTP 29.0"}).

-export([init/1, emit/3, emit/4, emit/5, emit/6, emit_legacy/3, emit_legacy/5,
        get_warnings/1, get_errors/1, has_errors/1, has_warnings/1, report/4]).
-export([add_diagnostics/2, get_diagnostic/2, push_diagnostic/4, pop_diagnostic/2, get_value/2,
         dump/1]).

-export([format_error/1]).

-export_type([error_info/0, error_listing/0, error_description/0]).

-type error_description() :: term().
-type error_info() :: {erl_anno:location(), module(), error_description()}.
-type error_listing() :: [{file:filename(), [error_info()]}].

-type code() :: unicode:unicode_binary().

-type key() :: term().
-type types() :: error | warning | info | hint.

-type diagnostic() ::
    #{ key := key(),
       type := types(),
       code => code(),
       off => unicode:chardata(),
       default => term(),
       type => boolean | value | set,
       format => function()
     }.

-type diagnostic_int() :: {
    #{ diagnostic => diagnostic(),
       value => [dynamic()] }
}.

-type entry() :: {diagnostic_int(), erl_anno:anno(), Arg :: term()}.

-type options() :: #{ }.

-export_type([state/0]).
-opaque state() ::
    #{ diagnostics := #{ term() => diagnostic_int() },
       warnings_as_errors := boolean(),
       warning := [entry()],
       error := [entry()],
       info := [entry()],
       hint := [entry()]
    }.
% -nominal state() :: {?MODULE, state()}. 

-doc """
Initialize a new diagnostic context
""".
-spec init(Opts :: options()) -> state().
init(Opts) ->
    #{ warnings_as_errors => maps:get(warnings_as_errors, Opts, false),
       diagnostics => #{},
       warning => [],
       error => [],
       info => [],
       hint => [] }.

-doc """
Add new diagnostics into the context.
""".
-spec add_diagnostics(state(), [diagnostic()]) -> state().
add_diagnostics(S = #{ diagnostics := Ds }, Diagnostics) ->
    NewDs = lists:foldl(
              fun F(#{ type := warning } = D, Acc) when map_get(warnings_as_errors, S) ->
                      F(D#{ type := error }, Acc);
                  F(#{ key := Key } = D, Acc) ->
                      maps:is_key(Key, Acc) andalso error({duplicate_key, D}),
                      Acc#{ Key => #{ diagnostic => D,
                                      value => [{maps:get(default, D, true), default}] } }
              end, Ds, Diagnostics),
    S#{ diagnostics := NewDs }.

-doc """
Push a new value for a diagnostic.
""".
-spec push_diagnostic(state(), Key :: key(), Value :: term(), Anno :: erl_anno:anno()) ->
    state().
push_diagnostic(#{ diagnostics := Ds } = S, Key, Value, Anno) ->
    D = maps:get(Key, Ds),
    S#{ diagnostics := Ds#{ Key := D#{ value := [{Value, Anno} | maps:get(value, D)]}}}.

-doc """
Pop a previous value for a diagnostic.  
""".
-spec pop_diagnostic(state(), key()) ->
    {ok, Value :: term(), state()}.
pop_diagnostic(S, _Key) ->
    {ok, undefined, S}.

-doc """
Get a diagnostic.
""".
-spec get_diagnostic(state(), key()) -> {ok, diagnostic()} | undefined.
get_diagnostic(#{ diagnostics := Ds }, Key) ->
    case maps:find(Key, Ds) of
        error -> undefined;
        {ok, _} = Ok -> Ok
    end.

-doc """
Get the value of a diagnostic.
""".
-spec get_value(state(), Key :: key()) -> term().
get_value(S, Key) ->
    {ok, D} = get_diagnostic(S, Key),
    element(1,hd(maps:get(value, D))).

-doc #{ equiv => emit(S, Key, Anno, [], #{}) }.
-spec emit(state(), key(), erl_anno:anno()) ->
    state().
emit(S, Key, Anno) ->
    emit(S, Key, Anno, []).

-doc #{ equiv => emit(S, Key, Anno, Args, #{}) }.
-spec emit(state(), key(), erl_anno:anno(), [term()]) ->
    state().
emit(S, Key, Anno, Args) ->
    emit(S, Key, Anno, Args, #{}).

-doc """
Emit a diagnostic entry into the collection.
""".
-spec emit(state(), key(), erl_anno:anno(), [term()], #{ related => [{erl_anno:anno(), unicode:chardata()}]}) -> state().
emit(S, Key, Anno, Args, Opts) ->
    try get_diagnostic(S, Key) of
        {ok, #{ diagnostic := #{ type := Type, format := Fmt } } = D}
            when is_function(Fmt, length(Args)) ->
            case get_value(S, Key) of
                true ->
                    S#{ Type := [{D, Anno, Args, Opts} | maps:get(Type, S)]};
                false -> S
            end;
        undefined ->
            erlang:error({badarg,[S, Key, Anno, Args]})
    catch error:{bad_key, _} ->
        erlang:error({badarg,[S, Key, Anno, Args]})
    end.

-doc """
Emit a diagnostic entry into the collection and create the diagnostic `D`
if it does not exist.
""".
-spec emit(state(), key(), erl_anno:anno(), [term()], #{}, diagnostic()) ->
    state().
emit(S, Key, Anno, Args, Opts, D) ->
    DiagState =
    case get_diagnostic(S, Key) of
        undefined -> erl_diagnostic:add_diagnostics(S, [D]);
        _ -> S
    end,
    erl_diagnostic:emit(
        DiagState,
        Key, Anno, Args, Opts).

%% Emit a legacy diagnostic entry into the collection
%% This is a private API only used by the compiler
-doc """
Emits a diagnostic using the legacy 

This function takes
""".
-spec emit_legacy(state(), types(), error_listing()) -> state().
emit_legacy(S, Type, [{File, [{Location, Mod, Reason}|T]}|R]) ->
    AnnoNoFile = case Location of
                     none -> erl_anno:new(0);
                     Location -> erl_anno:new(Location)
                 end,

    Anno = erl_anno:set_file(File, AnnoNoFile),

    emit_legacy(emit_legacy(S, Type, Anno, Mod, Reason), Type, [{File, T} | R]);
emit_legacy(S, Type, [{_File, []}|R]) ->
    emit_legacy(S, Type, R);
emit_legacy(S, _Type, []) ->
    S.

-spec emit_legacy(state(), types(), erl_anno:anno(), module(), term()) -> state().
emit_legacy(S, Type, Anno, Mod, Reason) ->
    Code = case erlang:function_exported(Mod, error_code, 1) of
               true -> case Mod:error_code(Reason) of undefined -> #{}; C -> #{ code => C } end;
               false -> #{}
           end,

    erl_anno:file(Anno) =/= undefined orelse error({badarg, "anno has no file set"}),

    D = #{ diagnostic => Code#{ key => {Mod, Reason}, type => Type,
                                format => fun(A) -> Mod:format_error(A) end} },
    S#{ Type := [{D, Anno, [Reason], #{ legacy => Mod }} | maps:get(Type, S)]}.

-doc """
Get all emitted warnings in legacy format.
""".
-spec get_warnings(state()) -> [term()].
get_warnings(#{ warning := Ws }) ->
    pack_diagnostics(Ws).

-doc """
Get all emitted warnings in legacy format.
""".
-spec get_errors(state()) -> [term()].
get_errors(#{ error := Es }) ->
    pack_diagnostics(Es).

pack_diagnostics(Ds) ->
    [{File, lists:map(fun({_D, Anno, [Tag], #{ legacy := Mod }}) ->
                              Location = case erl_anno:location(Anno) of
                                             0 -> none;
                                             L -> L
                                         end,
                              {Location, Mod, Tag};
                         ({D, Anno, Arg, Opts}) ->
                              {erl_anno:location(Anno), erl_diagnostic, {D, Arg, get_help(D, Opts)}}
                      end, FileDs)} || {File, FileDs} <- group_diagnostics(Ds)].

group_diagnostics(Ds) ->
    Files = lists:usort([erl_anno:file(Anno) || {_, Anno,_, _} <- Ds]),
    [{File, lists:sort(fun({_DA, AnnoA, [TagA], #{ legacy := ModA }},
                           {_DB, AnnoB, [TagB], #{ legacy := ModB }}) ->
                               case erl_anno:location(AnnoA) == erl_anno:location(AnnoB) of
                                   true -> {ModA, TagA} =< {ModB, TagB};
                                   false -> erl_anno:location(AnnoA) < erl_anno:location(AnnoB)
                               end;
                          ({_, AnnoA, _, _}, {_, AnnoB, _, _}) ->
                               erl_anno:location(AnnoA) =< erl_anno:location(AnnoB)
                       end,
                       [{D, Anno, Arg, Opts}
                        || {D, Anno, Arg, Opts} <- lists:reverse(Ds),
                           erl_anno:file(Anno) =:= File])}
     || File <- Files].

-doc """
Return if any errors have been emitted
""".
-spec has_errors(state()) -> boolean().
has_errors(#{ error := Es }) -> Es =/= [].

-doc """
Return if any warnings have been emitted
""".
-spec has_warnings(state()) -> boolean().
has_warnings(#{ warning := Ws }) -> Ws =/= [].

-doc """
Dump the diagnostic state to stdout
""".
-spec dump(state()) -> ok.
dump(#{ diagnostics := D }) ->
    io:format("Diagnostics:\n"),
    [io:format(" ~p: ~p ~p~n",[Key, Type, Values])
    || Key := #{ diagnostic := #{ type := Type }, value := Values} <:- maps:iterator(D, ordered)],
    ok.

-doc """
Format all entries into the given format.
""".
-spec report(io:device(), state(), json | ansi | txt, #{ types => [types()],
            filename => base | relative | full }) -> ok.
report(Dev, S, Format, Opts) ->
    [report_type(Dev, group_diagnostics(maps:get(T, S, [])), Format, Opts)
     || T <- maps:get(types, Opts, [error, warning, info, hint])],
    ok.

report_type(Dev, Files, ansi, Opts) ->
    [format_messages(Dev, File, Ds, Opts) || {File, Ds} <- Files];
report_type(Dev, Files, json, _Opts) ->
    io:format(Dev, "~ts", [json:format(format_json(Files))]).

%% TODO: gcc-13 added support for outputting in sarif format. Should we use that instead of LSP?
format_json([{File, [{#{ diagnostic := D } = DI, Anno, Arg, _Opts} | T]} | Files]) ->
    Code = case application:get_diagnostic(maps:get(code, D, "")) of
               {ok, [#{ url := Url }]} ->
                   #{ ~"doc_uri" => unicode:characters_to_binary(Url), ~"code" => maps:get(code, D)};
               _ -> #{}
           end,
    [Code#{ ~"uri" => unicode:characters_to_binary("file://" ++ filename:absname(File)),
            ~"range" => #{ ~"start" => #{ ~"line" => erl_anno:line(Anno),
                                          ~"character" => erl_anno:column(Anno) } },
            ~"severity" => atom_to_binary(maps:get(type, D)),
            ~"message" => unicode:characters_to_binary(format_error({DI, Arg, []}))
          } |
     format_json([{File, T} | Files])];
format_json([{_File, []} | Files]) ->
    format_json(Files);
format_json([]) ->
    [].

-doc false.
format_error({#{ diagnostic := #{ format := Fmt } }, FmtArgs, _Help}) ->
    string:trim(case apply(Fmt, FmtArgs) of
        {Format, Args} when is_list(Args) ->
            io_lib:format(Format, Args);
        Bin ->
            unicode:characters_to_list(Bin)
    end).

get_help(D, Opts) ->
    lists:flatten([get_code_note(D, Opts),
                   get_warning_help(D),
                   get_suggestion_help(D, Opts)]).

get_code_note(#{ diagnostic := #{ code := Code }}, Opts) when is_binary(Code) ->
    case lists:member(Code, maps:get(ignore_code, Opts, [])) of
        false ->
            {"help", io_lib:format("call `erlc -explain ~ts` to see a detailed explanation", [Code])};
        true ->
            []
    end;
get_code_note(_, _) ->
    [].

get_warning_help(#{ diagnostic := #{ type := warning, off := Off, on := On } } = D) ->
    {"note", io_lib:format("`~ts` was enabled ~ts; \n\tuse `~ts` to turn off this warning", [On, get_enabler(D), Off])};
get_warning_help(#{ diagnostic := #{ type := warning, off := Off } } = D) ->
    {"note", io_lib:format("was enabled ~ts; use `~ts` to turn off this warning", [get_enabler(D), Off])};
get_warning_help(_) ->
    [].

get_enabler(#{ value := [{_, Tool}|_]}) when is_atom(Tool) ->
    io_lib:format("by ~p", [Tool]);
get_enabler(#{ value := [{_, Anno}|_]}) ->
    io_lib:format("at ~s:~p:~p",[erl_anno:file(Anno),
    erl_anno:line(Anno), erl_anno:column(Anno)]).

get_suggestion_help(_D, #{ possible := {Name, PossibleNames} } = _O) ->
    case PossibleNames of
        [] -> [];
        _ ->
            %% kk and kl has a similarity of 0.66. Short names are common in
            %% Erlang programs, therefore we choose a relatively low threshold
            %% here.
            SufficientlySimilar = 0.66,
            NameString = to_string(Name),
            Similarities = [{string:jaro_similarity(NameString, to_string(F)), to_string(F)} ||
                               F <- PossibleNames],
            {MaxSim, GuessName} = lists:last(lists:sort(Similarities)),
            case MaxSim > SufficientlySimilar of
                true -> {"help", io_lib:format("did you mean ~ts?",[GuessName])};
                false -> []
            end
    end;
get_suggestion_help(_D, _O) ->
    [].

to_string(N) ->
    io_lib:format("~p",[N]).

% -spec format_messages(File::string(), [erl_lint:error_info()], Opts::[term()]) -> [message()].
format_messages(Dev, F, [{D, none, Args, DOpts} | Es], Opts) ->
    Brief = maps:get(brief, Opts, false),
    Type = maps:get(type, maps:get(diagnostic, D)),
    io:format(Dev, "~ts: ~s~ts\n~ts~s",
              [format_filename(F, Opts), io_lib:format("~p: ", [Type]),
              format_error({D, Args, []}),
               [io_lib:format("\n% ~ts: ~ts",[Cat, Msg]) ||
                   {Cat, Msg} <- get_help(D, maps:merge(DOpts, Opts)),
                   not Brief],
                ["\n\n" || not Brief]
              ]),
    format_messages(Dev, F, Es, ignore_code(D, Opts));
format_messages(Dev, F, [{D, Anno, Args, DOpts} | Es], Opts) ->
    Brief = maps:get(brief, Opts, false),
    StartLoc = erl_anno:location(Anno),
    EndLoc = StartLoc,
    Src = [string:trim(quote_source(F, StartLoc, EndLoc)) || not Brief],
    io:format(Dev, "~ts:~ts: ~s~ts~ts\n~ts~ts~ts~s",
              [
               format_filename(F, Opts),
               fmt_pos(StartLoc),
               fmt_type(D, Opts),
               format_error({D, Args, []}),
               [get_code(D) || not Brief],
               Src,
               [format_related(maps:get(related, DOpts, []), F, Opts) || not Brief],
               [io_ansi:format(["\n% ",help_color(Cat),"~ts: ~ts", io_ansi:reset()],[Cat, Msg]) ||
                   {Cat, Msg} <- get_help(D, maps:merge(DOpts, Opts)), not Brief],
                ["\n\n" || not Brief]
              ]),
    format_messages(Dev, F, Es, ignore_code(D, Opts));
format_messages(_, _, [], _Opts) ->
    [].

format_related([], _, _) ->
    [];
format_related([{Anno, Slogan}|R], Filename, Opts) ->
    case erl_anno:file(Anno) =/= undefined andalso string:equal(erl_anno:file(Anno), Filename) of
        true -> 
            StartLoc = erl_anno:location(Anno),
            EndLoc = erl_anno:location(Anno),
            ["\n\n",indent(2, [format_filename(Filename, Opts),":",
                             fmt_pos(StartLoc),": ", Slogan,"\n",
                             string:trim(quote_source(Filename, StartLoc, EndLoc))])
            | format_related(R, Filename, Opts)];
        false ->
            format_related(R, Filename, Opts)
    end.

indent(Num, String) ->
    Lines = string:split(String, "\n", all),
    lists:join($\n, [[lists:duplicate(Num, $ ), Line] || Line <- Lines]).

format_filename(Filename, #{ filename := base}) ->
    filename:basename(Filename);
format_filename(Filename, #{ filename := full}) ->
    filename:absname(Filename);
format_filename(Filename, _) ->
    {ok, Cwd} = file:get_cwd(),
    case string:prefix(filename:absname(Filename), Cwd ++ "/") of
        nomatch -> Filename;
        RelativeFilename -> RelativeFilename
    end.

ignore_code(#{ diagnostic := #{ code := Code }}, Opts) ->
    IgnoreCore = maps:get(ignore_code, Opts, []),
    Opts#{ ignore_code => [Code | IgnoreCore]};
ignore_code(_, Opts) ->
    Opts.

help_color("note") -> io_ansi:cyan();
help_color(_) -> io_ansi:light_black().

get_code(#{ diagnostic := #{ code := Code } }) ->
    case application:get_diagnostic(Code) of
        {ok, [#{url := Url}]} ->
            io_ansi:format([" [",io_ansi:hyperlink_start(Url),"~ts",io_ansi:hyperlink_reset(),"]"], [Code]);
        _ ->
            io_lib:format(" [~ts]", [Code])
    end;
get_code(_) -> "".

fmt_type(#{ diagnostic := #{ type := T }} = _D, _Opts) ->
    Color = case T of
                error -> red;
                warning -> magenta;
                info -> light_blue;
                hint -> light_black
            end,
    io_ansi:format([Color, "~p: ", io_ansi:reset()], [T]).

fmt_pos({Line, Col}) ->
    io_lib:format("~w:~w", [Line, Col]);
fmt_pos(Line) ->
    io_lib:format("~w", [Line]).

quote_source(File, Line, Loc2) when is_integer(Line) ->
    quote_source(File, {Line, 1}, Loc2);
quote_source(File, Loc1, Line) when is_integer(Line) ->
    quote_source(File, Loc1, {Line, -1});
quote_source(File, {StartLine, StartCol}, {EndLine, EndCol}) ->
    case file:read_file(File) of
        {ok, Bin} ->
            Enc = case epp:read_encoding_from_binary(Bin) of
                      none -> epp:default_encoding();
                      Enc0 -> Enc0
                  end,
            Ctx =
                if
                    StartLine =:= EndLine -> 0;
                    true -> 1
                end,
            case seek_line(Bin, 1, StartLine - Ctx) of
                {ok, Bin1} ->
                    quote_source_1(Bin1, Enc, StartLine, StartCol, EndLine, EndCol, Ctx);
                error ->
                    ""
            end;
        {error, _} ->
            ""
    end.

quote_source_1(Bin, Enc, StartLine, StartCol, EndLine, EndCol, Ctx) ->
    case take_lines(Bin, Enc, StartLine - Ctx, EndLine + Ctx) of
        [] ->
            "";
        Lines ->
            Lines1 =
                case length(Lines) =< (4 + Ctx) of
                    true ->
                        Lines;
                    false ->
                        %% before = context + start line + following line
                        %% after = end line + context
                        %% (total lines: 3 + 1 + context)
                        Before = lists:sublist(Lines, 2 + Ctx),
                        After = lists:reverse(
                            lists:sublist(lists:reverse(Lines), 1 + Ctx)
                        ),
                        Before ++ [{0, "..."}] ++ After
                end,
            Lines2 = decorate(Lines1, StartLine, StartCol, EndLine, EndCol),
            [[fmt_line(L, Text) || {L, Text} <:- Lines2], $\n]
    end.

line_prefix() ->
    "% ".

fmt_line(L, Text) ->
    {LineText, LineTextLength} = line_to_txt(L),
    [line_prefix(),
     io_lib:format("~*.ts| ", [LineTextLength, LineText]),
     Text, "\n"].

line_to_txt(L) ->
    LineText = integer_to_list(abs(L)),
    Length = max(4, length(LineText)),
    if
        L < 0 ->
            {"", Length};
        true ->
            {LineText, Length}
    end.

decorate([{Line, Text} = L | Ls], StartLine, StartCol, EndLine, EndCol) when
  Line =:= StartLine, EndLine =:= StartLine ->
    %% start and end on same line
    S = underline(Text, StartCol, EndCol),
    decorate(S, L, Ls, StartLine, StartCol, EndLine, EndCol);
decorate([{Line, Text} = L | Ls], StartLine, StartCol, EndLine, EndCol) when Line =:= StartLine ->
    %% start with end on separate line
    S = underline(Text, StartCol, string:length(Text) + 1),
    decorate(S, L, Ls, StartLine, StartCol, EndLine, EndCol);
decorate([{_Line, _Text} = L | Ls], StartLine, StartCol, EndLine, EndCol) ->
    [L | decorate(Ls, StartLine, StartCol, EndLine, EndCol)];
decorate([], _StartLine, _StartCol, _EndLine, _EndCol) ->
    [].

%% don't produce empty decoration lines
decorate("", L, Ls, StartLine, StartCol, EndLine, EndCol) ->
    [L | decorate(Ls, StartLine, StartCol, EndLine, EndCol)];
decorate(Text, {Line, _} = L, Ls, StartLine, StartCol, EndLine, EndCol) ->
    [L, {-Line, Text} | decorate(Ls, StartLine, StartCol, EndLine, EndCol)].

%% End typically points to the first position after the actual region.
%% If End = Start, we adjust it to Start+1 to mark at least one character
%% TODO: colorization option
underline(_Text, Start, End) when End < Start ->
    % no underlining at all if end column is unknown
    "";
underline(Text, Start, Start) ->
    underline(Text, Start, Start + 1);
underline(Text, Start, End) ->
    underline(Text, Start, End, 1).

underline([$\t | Text], Start, End, N) when N < Start ->
    [$\t | underline(Text, Start, End, N + 1)];
underline([_ | Text], Start, End, N) when N < Start ->
    [$\s | underline(Text, Start, End, N + 1)];
underline(_Text, _Start, End, N) ->
    underline_1(N, End).

underline_1(N, End) when N < End ->
    [$^ | underline_1(N + 1, End)];
underline_1(_N, _End) ->
    "".

seek_line(Bin, L, L) -> {ok, Bin};
seek_line(<<$\n, Rest/binary>>, N, L) -> seek_line(Rest, N + 1, L);
seek_line(<<$\r, $\n, Rest/binary>>, N, L) -> seek_line(Rest, N + 1, L);
seek_line(<<_, Rest/binary>>, N, L) -> seek_line(Rest, N, L);
seek_line(<<>>, _, _) -> error.

take_lines(<<>>, _Enc, _Here, _To) ->
    [];
take_lines(Bin, Enc, Here, To) when Here =< To ->
    {Text, Rest} = take_line(Bin, <<>>),
    [{Here, text_to_string(Text, Enc)}
     | take_lines(Rest, Enc, Here + 1, To)];
take_lines(_Bin, _Enc, _Here, _To) ->
    [].

text_to_string(Text, Enc) ->
    case unicode:characters_to_list(Text, Enc) of
        String when is_list(String) -> String;
        {error, String, _Rest} -> String;
        {incomplete, String, _Rest} -> String
    end.

take_line(<<$\n, Rest/binary>>, Ack) ->
    {Ack, Rest};
take_line(<<$\r, $\n, Rest/binary>>, Ack) ->
    {Ack, Rest};
take_line(<<B, Rest/binary>>, Ack) ->
    take_line(Rest, <<Ack/binary, B>>);
take_line(<<>>, Ack) ->
    {Ack, <<>>}.


% dbg(T) -> io:format("~p~n",[T]), T.