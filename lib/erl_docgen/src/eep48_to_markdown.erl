%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2018. All Rights Reserved.
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
-module(eep48_to_markdown).
-feature(maybe_expr, enable).

%% This module takes care of rendering and normalization of
%% application/erlang+html style documentation.

%% It is a copy of shell_docs.erl from the erlang/otp repo that
%% has been modified to emit markdown instead of shell.

-include_lib("kernel/include/eep48.hrl").

-export([render_docs/1, render_docs/2]).
-export([render/3, render/4, render/5]).
-export([render_type/3, render_type/4, render_type/5]).
-export([render_callback/3, render_callback/4, render_callback/5]).

-export_type([chunk_elements/0, chunk_element_attr/0]).

-record(config, {docs :: docs_v1() | undefined}).

-define(ALL_ELEMENTS, [
    a,
    p,
    'div',
    br,
    h1,
    h2,
    h3,
    h4,
    h5,
    h6,
    hr,
    i,
    b,
    em,
    strong,
    pre,
    code,
    ul,
    ol,
    li,
    dl,
    dt,
    dd,
    table,
    tr,
    td
]).
%% inline elements are:
-define(INLINE, [i, b, em, strong, code, a]).
-define(IS_INLINE(ELEM),
    (((ELEM) =:= a) orelse ((ELEM) =:= code) orelse
        ((ELEM) =:= i) orelse ((ELEM) =:= em) orelse
        ((ELEM) =:= b) orelse ((ELEM) =:= strong))
).
%% non-inline elements are:
-define(BLOCK, [p, 'div', pre, br, ul, ol, li, dl, dt, dd, h1, h2, h3, h4, h5, h6, hr, table, tr, td]).
-define(IS_BLOCK(ELEM), not ?IS_INLINE(ELEM)).
-define(IS_PRE(ELEM), ((ELEM) =:= pre)).

%% If you update the below types, make sure to update the documentation in
%% erl_docgen/doc/src/doc_storage.xml as well!!!
-type docs_v1() :: #docs_v1{
    docs :: [chunk_entry()], format :: binary(), module_doc :: none | hidden | #{ binary() => chunk_elements() }
}.
-type chunk_entry() :: {
    {Type :: function | type | callback, Name :: atom(), Arity :: non_neg_integer()},
    Anno :: erl_anno:anno(),
    Sigs :: [binary()],
    Docs :: none | hidden | #{binary() => chunk_elements()},
    Meta :: #{signature := term()}
}.
-type config() :: #{}.
-type chunk_elements() :: [chunk_element()].
-type chunk_element() ::
    {chunk_element_type(), chunk_element_attrs(), chunk_elements()}
    | binary().
-type chunk_element_attrs() :: [chunk_element_attr()].
-type chunk_element_attr() :: {atom(), unicode:chardata()}.
-type chunk_element_type() :: chunk_element_inline_type() | chunk_element_block_type().
-type chunk_element_inline_type() :: a | code | em | strong | i | b.
-type chunk_element_block_type() ::
    p
    | 'div'
    | br
    | pre
    | ul
    | ol
    | li
    | dl
    | dt
    | dd
    | h1
    | h2
    | h3
    | h4
    | h5
    | h6.

-export([convert/1, convert_application/1, modules/1]).

-spec normalize(Docs) -> NormalizedDocs when
    Docs :: chunk_elements(),
    NormalizedDocs :: chunk_elements().
normalize(Docs) ->
    shell_docs:normalize(Docs).

convert_application(App) ->
    put(application, atom_to_list(App)),
    Modules = modules(App),
    case App of
        wx ->
            %% We cannot run wx in parallel as there are docs in wx.hrl and many different
            %% modules have types defined in it.
            [try convert(M)
             catch E:R:ST ->
                     io:format("~p:~p:~p~n",[E,R,ST]),
                     erlang:raise(E,R,ST)
             end || M <- Modules];
        _ ->
            Pids =
                [spawn_monitor(
                   fun() ->
                           put(application, atom_to_list(App)),
                           try convert(M)
                           catch E:R:ST ->
                                   io:format("~p:~p:~p~n",[E,R,ST]),
                                   exit({error,E,R,ST})
                           end
                   end) || M <- Modules],
            [receive
                 {'DOWN',Ref,_,_,normal} -> ok;
                 {'DOWN',Ref,_,_,{error,E,R,ST}} ->
                     erlang:raise(E,R,ST)
             end || {_P,Ref} <- Pids]
    end,
    docgen_xml_to_markdown:convert_application(App),
    ok.

modules(App) ->
    [list_to_atom(filename:basename(filename:rootname(File)))
     || File <- filelib:wildcard(filename:join(filename:dirname(code:priv_dir(App)),"doc/chunks/*.chunk"))].

convert(Module) ->
    io:format("Converting: ~p~n",[Module]),

    ModulePath =
        case code:which(Module) of
            preloaded ->
                filename:join(["erts","preloaded","ebin",Module]);
            Path -> Path
        end,

    case code:get_doc(Module, #{ sources => [eep48] }) of
        {ok, #docs_v1{ format = <<"application/erlang+html">>,
                       module_doc = #{ <<"en">> := ModuleDoc }, docs = Docs } = DocsV1 } ->

            put(application, get_app(Module)),

            %% We first recompile the file in order to make sure we have the correct AST
            %% The AST may have changed due to partial .hrl files being converted already.
            [{ok, _} = c:c(Module) || lists:member(get(application),["wx"])],

            {ok, {Module,
                  [{debug_info,
                    {debug_info_v1, erl_abstract_code,
                     {AST, Meta0}}}]}} = beam_lib:chunks(ModulePath,[debug_info]),

            Meta =
                case code:which(Module) of
                    preloaded ->
                        [{cwd,"erts/preloaded/src"}|Meta0];
                    _ ->
                        Meta0
                end,


            NewFiles = convert(#{ meta => Meta, ast => AST, docs => DocsV1 },
                               filter_and_fix_anno(expand_anno(AST), Docs)),

            %% io:format("~p~n", [AST]),
            {_, File, Anno} = lists:foldl(
                                fun({attribute, [{generated,true}|_], file, {File, Line}}, {false, _, _}) ->
                                        {true, File, Line};
                                   (_, FileAnno) when is_tuple(FileAnno) ->
                                        FileAnno;
                                   ({attribute, _, file, {File,_}}, _) ->
                                        File;
                                   ({attribute, Anno, module, _}, File) ->
                                        {false, File, Anno}
                                end, undefined, AST),
            Filename = filename:join(proplists:get_value(cwd, Meta, ""), File),

            %% These modules have a -feature macro at top that needs to be above the -moduledoc line.
            %% So we adjust the docs down one step.
            ModuleDocLine = case lists:member(Module, [asn1ct, erl_features, erl_lint]) of
                                true -> erl_anno:line(Anno) + 1;
                                false -> erl_anno:line(Anno)
                            end,
            {BeforeModule, AfterModule} = lists:split(ModuleDocLine, maps:get(Filename, NewFiles)),

            NewFilesWithModuleDoc =
                NewFiles#{ Filename => BeforeModule ++ convert_moduledoc(ModuleDoc)
                           ++ generate_skipped_callbacks(maps:get(skipped, NewFiles, []), NewFiles)
                           ++ AfterModule
                         },


            %% io:format("~p~n",[hd(NewFileBin)]),
            %%    io:format("~p~n",[NewFiles]),
            %%    ok = nok,
            %% io:format("~p~n",[hd(lists:reverse(NewFileBin))]),
            %%    io:format("~p~n",[AST]),
            %% {AST, Meta}.
            %%    [ io:format("~ts:~n~ts~n", [Key, ""]) || Key := Value <- NewFiles, not is_atom(Key)],
            _ = [ begin
                      io:format("\tUpdated ~ts~n",[Key]),
                      ok = file:write_file(Key, formatter(Key, lists:flatten([[A,$\n] || A <- Value])))
                  end || Key := Value <- NewFilesWithModuleDoc, not is_atom(Key)],
            ok;
        {ok, #docs_v1{ module_doc = hidden }} ->
            ok;
        {ok, #docs_v1{ format = <<"text/markdown">> }} ->
            {ok, Module, Chunks} = beam_lib:all_chunks(ModulePath),
            {ok, NewBeamFile} = beam_lib:build_module(proplists:delete("Docs", Chunks)),
            file:write_file(ModulePath ++ ".beam", NewBeamFile),
            convert(Module);
        Error ->
            io:format("Error: ~p~n",[Error]),
            ok
    end.

convert(Files, Docs) ->
    SortedDocs =
        lists:sort(
          fun(MFA1, MFA2) ->
                  Anno1 = element(2, MFA1),
                  Anno2 = element(2, MFA2),
                  case erl_anno:file(Anno1) =:= erl_anno:file(Anno2) of
                      true ->
                          erl_anno:line(Anno1) >= erl_anno:line(Anno2);
                      false ->
                          erl_anno:file(Anno1) >= erl_anno:file(Anno2)
                  end
          end, Docs),
    {Prev, Acc} =
        case SortedDocs of
            [] -> {[],[]};
            SortedDocs ->
                lists:foldl(
                  fun(MFA,{[H|_] = Prev,Acc}) ->
                          MFAAnno = element(2, MFA),
                          HAnno = element(2, H),
                          case erl_anno:file(MFAAnno) =:= erl_anno:file(HAnno) andalso
                              erl_anno:line(MFAAnno) =:= erl_anno:line(HAnno) of
                              true ->
                                  {[MFA|Prev],Acc};
                              false ->
                                  {[MFA],lists:reverse(Prev) ++ Acc}
                          end
                  end, {[hd(SortedDocs)],[]}, tl(SortedDocs))
        end,
    %% io:format("~p",[SortedDocs]),
    convert([], [], lists:reverse(Prev ++ Acc), Files).
convert([], [], [], Files) ->
    %% When there are no documented functions in module, eg. gen_fsm
    Cwd = proplists:get_value(cwd, maps:get(meta, Files), ""),
    {attribute, _, file, {Filename, _}} = lists:keyfind(file, 3, maps:get(ast, Files)),
    {ok, Bin} = file:read_file(filename:join(Cwd, Filename)),
    Files#{ filename:join(Cwd, Filename) => string:split(strip_beh_info(Bin),"\n",all) };
convert(Lines, Acc, [], Files) ->
    Files#{ maps:get(filename, Files) => Lines ++ Acc};
convert(Lines, Acc, [{{K,F,A}, 0, _, _, _} = E | T], Files) ->
    io:format("Skipping ~p ~p/~p~n",[K,F,A]),
    convert(Lines, Acc, T, Files#{ skipped => [E | maps:get(skipped, Files, [])] });
convert(Lines, Acc, [{Kind, Anno, Slogan, _D, _Meta} = E | T] = Docs, Files) ->
    case erl_anno:file(Anno) =:= maps:get(current, Files, undefined) of
        true ->
            {Before, After} = lists:split(erl_anno:line(Anno)-1, Lines),
            {{Kind, Anno, Slogan, D, Meta}, NewT} = maybe_merge_entries(E, T),
            DocString = generate_doc_attributes(D, Meta, Files),
            convert(Before, DocString ++ After ++ Acc, NewT, Files);
        false ->
            Cwd = proplists:get_value(cwd, maps:get(meta, Files), ""),
            Filename = filename:join(Cwd, erl_anno:file(Anno)),
            {ok, Bin} = file:read_file(Filename),

            NewFiles =
                case maps:get(current, Files, undefined) of
                    undefined -> Files;
                    _ -> Files#{ maps:get(filename, Files) => Lines ++ Acc }
                end,
            convert(string:split(strip_beh_info(Bin),"\n",all), [], Docs,
                    NewFiles#{ current => erl_anno:file(Anno), filename => Filename })
    end.

%% We merge any duplicate entries into the same "-doc" entry
maybe_merge_entries({Kind, Anno, Slogan, #{ <<"en">> := D }, Meta},
                    [{Kind, _, _, #{ <<"en">> := D2 }, M2} | T]) ->
    maybe_merge_entries(
      {Kind, Anno, Slogan, #{ <<"en">> => D2 ++ D },
       maps:merge_with(fun(since, V1, V2) ->
                               unicode:characters_to_binary(
                                 lists:join(", ",
                                            lists:usort(re:split(V1,", ?") ++ re:split(V2,", ?"))));
                          (_, _, V) ->
                               V
                       end, Meta, M2)}, T);
maybe_merge_entries(E, T) ->
    {E, T}.

strip_beh_info(Str) ->
    re:replace(
      re:replace(Str, "\n-export\\(\\[behaviour_info/1\\]\\)\\.\n","",[global,unicode]),
      "\nbehaviour_info\\((.|\n)*\\.\n","",[global,unicode,ungreedy]).

generate_doc_attributes(D, Meta, Files) ->
    DocString =
        case D of
            #{ <<"en">> := ErlangHtml } when not is_map_key(equiv, Meta) ->
                [{doc,render_docs(normalize(ErlangHtml), init_config(maps:get(docs, Files), #{}))}];
            D when D =:= #{}, is_map_key(equiv, Meta) ->
                [];
            D when D =:= #{} ->
                [{doc,""}]
        end,
    DocString ++ meta(Meta).

generate_skipped_callbacks(CBs, Files) ->
    generate_skipped_callbacks(CBs, CBs, Files).
generate_skipped_callbacks([{{callback, F, A}, _, Slogan, D, Meta} | T], AllCBs, Files) ->
    [_, Call] = string:split(Slogan,":"),
    [_, Args] = string:split(Call,"("),
    CallbackProto = lists:flatten(
                      io_lib:format(
                        "-callback ~ts(~ts",
                        [io_lib:write_atom(F),Args])),
    {ok, Toks, _} = erl_scan:string(CallbackProto ++ ".",{1, 1}),
    {Callback, NewD} =
        try
            {ok,{attribute, _, callback, {{F,A}, _}}} = erl_parse:parse_form(Toks),

            {Types, DwithoutTypes} =
                case maps:find(equiv, Meta) of
                    error ->
                        case D of
                            #{ <<"en">> := [{ul,[{class,<<"types">>}],Ts} | Rest] } ->
                                {Ts, #{ <<"en">> => Rest }};
                            #{ <<"en">> := _ } ->
                                {[], D}
                        end;
                    {ok, Equiv} when D =:= #{} ->
                        {Equiv, _, _, EquivD, _} = lists:keyfind(Equiv, 1, AllCBs),
                        case EquivD of
                            #{ <<"en">> := [{ul,[{class,<<"types">>}],Ts} | _] } ->
                                {Ts, D};
                            #{ <<"en">> := _ } ->
                                {[], D}
                        end
                end,
            case Types of
                [] ->
                    {io_lib:format("~ts.",[CallbackProto]),
                     DwithoutTypes};
                Types ->
                    {io_lib:format(
                       "~ts when ~ts.",
                       [CallbackProto,
                        lists:join(", ", munge_types(Types))
                       ]), DwithoutTypes}
            end
        catch E:R:ST ->
                io:format("Failed to parse: ~p~n",[Toks]),
                erlang:raise(E,R,ST)
        end,
    generate_doc_attributes(NewD, Meta, Files) ++
        [pp(Callback), ""]
        ++ generate_skipped_callbacks(T, AllCBs, Files);
generate_skipped_callbacks([], _AllCBs, _Files) ->
    [].

munge_types([{li,Attr,C},{li,_,[<<" "/utf8,_/binary>>|_] = LIC}|T]) ->
    %% If the next li starts with a nbsp we join it to the previous list item as
    %% it is a continuation of it.
    munge_types([{li,Attr,C ++ LIC}|T]);
munge_types([{li,_,C}|T]) ->
    NoNBSP = re:replace(strip_tags(C),"\\h"," ",[unicode,global]),
    [Body | Variables] = lists:reverse(string:split(NoNBSP, " = ", all)),
    [[Var, " :: ", Body] || Var <- lists:reverse(Variables)] ++ munge_types(T);
munge_types([]) ->
    [].

pp(String) ->
    maybe
        {ok, T, _} ?= erl_scan:string(lists:flatten(String), {1,1}),
        {ok, {attribute, _, _, _} = Attr} ?= erl_parse:parse_form(T),
        erl_pp:attribute(Attr)
    else
        {ok, {function, _, _, _, _} = Function} ->
            erl_pp:function(Function);
        Else ->
            io:format("Failed to parse: ~ts~n ~p",[String, Else]),
            error(Else)
    end.

get_app(Module) ->
    case code:which(Module) of
        preloaded ->
            "erts";
        Path ->
            [_Module, _Ebin, App | _] = lists:reverse(filename:split(Path)),
            App
    end.

%% Convert module documentation
convert_moduledoc(ModuleHeader) ->
    String = render_docs(normalize(ModuleHeader), init_config(undefined, #{})),
    FixDiameterDepsBug = re:replace(String, "```text\n(-include_lib\\(\"diameter/include/diameter.hrl\"\\).)\n```", "\n    \\1\n"),
    [{moduledoc,FixDiameterDepsBug}].

formatter(String) ->
    unicode:characters_to_binary(
      case {os:getenv("FORMAT_MD"),os:find_executable("npx")} of
          {"true",Npx} when Npx =/= false ->
              run_formatter("",String);
          _ ->
              String
      end).
formatter(Module, String) ->

    Text =
        case {os:getenv("FORMAT_MD"),os:find_executable("npx")} of
            {"true",Npx} when Npx =/= false ->
                %% Text0 = string:trim(re:replace(String, "\\\\\\\.( )?", ".\n", [unicode, global])),
                %% %% TODO: fix re so that the Text1 string:replace is not needed
                %% Text1 = string:replace(Text0, "\\\\\\.", "."),

                Header = "# FORMAT HEADER\n\n",

                FormatString =
                    lists:foldl(
                      fun({_, Doc}, Acc) ->
                              [[Header, Doc, "\n\n"]|Acc];
                         (_, Acc) ->
                              Acc
                      end, [], String),

                FormattedText = run_formatter(Module, lists:reverse(FormatString)),
                [<<>>|Split] = string:split(FormattedText, Header, all),
                {[], FormattedString} =
                    lists:foldl(fun({Type, _Doc}, {[FormattedDoc|T], Acc}) ->
                                        {T, [{Type, FormattedDoc} | Acc]};
                                   (Else, {FormattedDoc, Acc}) ->
                                        {FormattedDoc, [Else | Acc]}
                                end, {Split,[]}, String),
                lists:reverse(FormattedString);
            _ ->
                String
        end,
    unicode:characters_to_binary(
      lists:map(fun({doc, Doc}) ->
                        doc(Doc);
                   ({moduledoc, Doc}) ->
                        moduledoc(Doc);
                   (Else) ->
                        Else
                end, Text)).

run_formatter(Module, String) ->
    Filename = string:trim(os:cmd("mktemp --suffix=."++filename:basename(Module)++".md")),
    ok = file:write_file(Filename, unicode:characters_to_binary(String)),
    os:cmd("npx prettier --parser markdown --prose-wrap always --write " ++ Filename),
    {ok, FormattedText} = file:read_file(Filename),
    %% file:delete(Filename),
    FormattedText.

doc(String) ->
    ["-doc \"\n", to_erlang_string(String), "\n\"."].

moduledoc(String) ->
    %% NewLines = re:replace(String, "\\\.( )?", ".\n", [global]),
    ["-moduledoc \"", to_erlang_string(String), "\n\"."].

meta(#{ edit_url := _} = Meta) ->
    meta(maps:remove(edit_url, Meta));
meta(#{ signature := _} = Meta) ->
    meta(maps:remove(signature, Meta));
meta(#{ equiv := {function,F,A} } = Meta) ->
    [io_lib:format("-doc(#{equiv => ~p/~p}).",[F,A]) | meta(maps:remove(equiv, Meta))];
meta(Meta) when Meta =:= #{} ->
    "";
meta(Meta) ->
    [io_lib:format("-doc(~p).",[Meta])].


to_erlang_string(Text) ->
     string:trim(re:replace(Text, "(\"|\\\\)", "\\\\\\1", [global, unicode])).

filter_and_fix_anno(AST, [{{What, F, A}, _Anno, S, D, M} | T]) when is_map(D); is_map_key(equiv, M) ->
    NewAnno =
        case What of
            function ->
                case lists:search(fun({attribute, _SpecAnno, spec, {FA, _}}) when is_tuple(FA) ->
                                          {F, A} =:= FA orelse {erlang, F, A} =:= FA;
                                     %% ({attribute, _, spec, {Spec, _}}) when is_atom(Spec) ->
                                     %%      {F, A} =:= {Spec, 0};
                                     (_) ->
                                          false
                                  end, AST) of
                    {value, {attribute, SpecAnno, _, _}} ->
                        SpecAnno;
                    false ->
                        case lists:search(fun({function, _FuncAnno, FF, FA, _}) ->
                                                  {F, A} =:= {FF, FA};
                                             (_) ->
                                                  false
                                          end, AST) of
                            {value, {function, FuncAnno, _, _, _}} ->
                                FuncAnno;
                            false ->
                                io:format("~p~n",[AST]),
                                io:format("Could not find func: ~p/~p~n",[F,A]),
                                error(badarg)
                        end
                end;
           type ->
                case lists:search(fun({attribute, _TypeAnno, TO, {FA, _}}) when
                                            is_tuple(FA), TO =:= type orelse TO =:= opaque ->
                                          {F, A} =:= FA;
                                     ({attribute, _TypeAnno, TO, {Type, _, Args}}) when
                                            is_atom(Type), TO =:= type orelse TO =:= opaque ->
                                          {F, A} =:= {Type, length(Args)};
                                     (_) ->
                                          false
                                  end, AST) of
                    {value, {attribute, TypeAnno, _, _}} ->
                        TypeAnno;
                    false ->
                        io:format("Could not find type: ~p/~p~n",[F,A]),
                        error(badarg)
                end;
            callback ->
                case lists:search(fun({attribute, _CBAnno, callback, {FA, _}}) ->
                                          {F, A} =:= FA;
                                     (_) ->
                                          false
                                  end, AST) of
                    {value, {attribute, CBAnno, _, _}} ->
                        CBAnno;
                    false ->
                        io:format("Could not find callback: ~p/~p~n",[F,A]),
                        erl_anno:new(0)
                end
        end,
    [{{What, F, A}, NewAnno, S, D, M} | filter_and_fix_anno(AST, T)];
filter_and_fix_anno(AST, [_ | T]) ->
    filter_and_fix_anno(AST, T);
filter_and_fix_anno(_, []) ->
    [].

expand_anno(AST) ->
    {NewAST, _} =
        lists:mapfoldl(fun F({attribute, _, file, {NewFile, _}} = E, File) when NewFile =/= File ->
                               F(E, NewFile);
                           F(E, File) ->
                               {setelement(2, E, erl_anno:set_file(File, element(2, E))), File}
                       end, undefined, AST),
    %% io:format("NewAST: ~p~n",[NewAST]),
    NewAST.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API function for dealing with the function documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec render(Module, Function, Docs) -> Res when
    Module :: module(),
    Function :: atom(),
    Docs :: docs_v1(),
    Res :: unicode:chardata() | {error, function_missing}.
render(_Module, Function, #docs_v1{} = D) ->
    render(_Module, Function, D, #{});
render(Module, Function, Arity) when is_integer(Arity) ->
    {ok, D} = code:get_doc(Module),
    render(Module, Function, Arity, D).

-spec render
    (Module, Function, Docs, Config) -> Res when
        Module :: module(),
        Function :: atom(),
        Docs :: docs_v1(),
        Config :: config(),
        Res :: unicode:chardata() | {error, function_missing};
    (Module, Function, Arity, Docs) -> Res when
        Module :: module(),
        Function :: atom(),
        Arity :: arity(),
        Docs :: docs_v1(),
        Res :: unicode:chardata() | {error, function_missing}.
render(Module, Function, #docs_v1{docs = Docs} = D, Config) when
    is_atom(Module), is_atom(Function), is_map(Config)
->
    render_function(
        lists:filter(
            fun
                ({{function, F, _}, _Anno, _Sig, Doc, _Meta}) when Doc =/= none ->
                    F =:= Function;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    );
render(_Module, Function, Arity, #docs_v1{} = D) ->
    render(_Module, Function, Arity, D, #{}).

-spec render(Module, Function, Arity, Docs, Config) -> Res when
    Module :: module(),
    Function :: atom(),
    Arity :: arity(),
    Docs :: docs_v1(),
    Config :: config(),
    Res :: unicode:chardata() | {error, function_missing}.
render(Module, Function, Arity, #docs_v1{docs = Docs} = D, Config) when
    is_atom(Module), is_atom(Function), is_integer(Arity), is_map(Config)
->
    render_function(
        lists:filter(
            fun
                ({{function, F, A}, _Anno, _Sig, Doc, _Meta}) when Doc =/= none ->
                    F =:= Function andalso A =:= Arity;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API function for dealing with the type documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec render_type(Module, Type, Docs) -> Res when
    Module :: module(),
    Type :: atom(),
    Docs :: docs_v1(),
    Res :: unicode:chardata() | {error, type_missing}.
render_type(Module, Type, D = #docs_v1{}) ->
    render_type(Module, Type, D, #{}).

-spec render_type
    (Module, Type, Docs, Config) -> Res when
        Module :: module(),
        Type :: atom(),
        Docs :: docs_v1(),
        Config :: config(),
        Res :: unicode:chardata() | {error, type_missing};
    (Module, Type, Arity, Docs) -> Res when
        Module :: module(),
        Type :: atom(),
        Arity :: arity(),
        Docs :: docs_v1(),
        Res :: unicode:chardata() | {error, type_missing}.
render_type(_Module, Type, #docs_v1{docs = Docs} = D, Config) ->
    render_typecb_docs(
        lists:filter(
            fun
                ({{type, T, _}, _Anno, _Sig, _Doc, _Meta}) ->
                    T =:= Type;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    );
render_type(_Module, Type, Arity, #docs_v1{} = D) ->
    render_type(_Module, Type, Arity, D, #{}).

-spec render_type(Module, Type, Arity, Docs, Config) -> Res when
    Module :: module(),
    Type :: atom(),
    Arity :: arity(),
    Docs :: docs_v1(),
    Config :: config(),
    Res :: unicode:chardata() | {error, type_missing}.
render_type(_Module, Type, Arity, #docs_v1{docs = Docs} = D, Config) ->
    render_typecb_docs(
        lists:filter(
            fun
                ({{type, T, A}, _Anno, _Sig, _Doc, _Meta}) ->
                    T =:= Type andalso A =:= Arity;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% API function for dealing with the callback documentation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec render_callback(Module, Callback, Docs) -> Res when
    Module :: module(),
    Callback :: atom(),
    Docs :: docs_v1(),
    Res :: unicode:chardata() | {error, callback_missing}.
render_callback(_Module, Callback, #docs_v1{} = D) ->
    render_callback(_Module, Callback, D, #{}).

-spec render_callback
    (Module, Callback, Docs, Config) -> Res when
        Module :: module(),
        Callback :: atom(),
        Docs :: docs_v1(),
        Config :: config(),
        Res :: unicode:chardata() | {error, callback_missing};
    (Module, Callback, Arity, Docs) -> Res when
        Module :: module(),
        Callback :: atom(),
        Arity :: arity(),
        Docs :: docs_v1(),
        Res :: unicode:chardata() | {error, callback_missing}.
render_callback(_Module, Callback, Arity, #docs_v1{} = D) ->
    render_callback(_Module, Callback, Arity, D, #{});
render_callback(_Module, Callback, #docs_v1{docs = Docs} = D, Config) ->
    render_typecb_docs(
        lists:filter(
            fun
                ({{callback, T, _}, _Anno, _Sig, _Doc, _Meta}) ->
                    T =:= Callback;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    ).

-spec render_callback(Module, Callback, Arity, Docs, Config) -> Res when
    Module :: module(),
    Callback :: atom(),
    Arity :: arity(),
    Docs :: docs_v1(),
    Config :: config(),
    Res :: unicode:chardata() | {error, callback_missing}.
render_callback(_Module, Callback, Arity, #docs_v1{docs = Docs} = D, Config) ->
    render_typecb_docs(
        lists:filter(
            fun
                ({{callback, T, A}, _Anno, _Sig, _Doc, _Meta}) ->
                    T =:= Callback andalso A =:= Arity;
                (_) ->
                    false
            end,
            Docs
        ),
        D,
        Config
    ).

%% Get the docs in the correct locale if it exists.
-spec get_local_doc(_, 'hidden' | 'none' | map(), #docs_v1{}) -> chunk_elements().
get_local_doc(MissingMod, Docs, D) when is_atom(MissingMod) ->
    get_local_doc(atom_to_binary(MissingMod), Docs, D);
get_local_doc({F, A}, Docs, D) ->
    get_local_doc(
        unicode:characters_to_binary(
            io_lib:format("~tp/~p", [F, A])
        ),
        Docs,
        D
    );
get_local_doc({_Type, F, A}, Docs, D) ->
    get_local_doc({F, A}, Docs, D);
get_local_doc(_Missing, #{<<"en">> := Docs}, D) ->
    %% English if it exists
    normalize_format(Docs, D);
get_local_doc(_Missing, ModuleDoc, D) when map_size(ModuleDoc) > 0 ->
    %% Otherwise take first alternative found
    normalize_format(maps:get(hd(maps:keys(ModuleDoc)), ModuleDoc), D);
get_local_doc(Missing, hidden, _D) ->
    [
        {p, [], [
            <<"The documentation for ">>,
            Missing,
            <<
                " is hidden. This probably means that it is internal "
                "and not to be used by other applications."
            >>
        ]}
    ];
get_local_doc(_Missing, None, _D) when None =:= none; None =:= #{} ->
    [].

-spec normalize_format(chunk_elements(), #docs_v1{}) -> chunk_elements().
normalize_format(Docs, #docs_v1{format = ?NATIVE_FORMAT}) ->
    normalize(Docs);
normalize_format(Docs, #docs_v1{format = <<"text/", _/binary>>}) when is_binary(Docs) ->
    [{pre, [], [Docs]}].

%%% Functions for rendering reference documentation
-spec render_function([chunk_entry()], #docs_v1{}, map()) ->
    unicode:chardata() | {'error', 'function_missing'}.
render_function([], _D, _Config) ->
    {error, function_missing};
render_function(FDocs, #docs_v1{docs = Docs} = D, Config) ->
    Grouping =
        lists:foldl(
            fun
                ({_Group, _Anno, _Sig, _Doc, #{equiv := Group}} = Func, Acc) ->
                    Members = maps:get(Group, Acc, []),
                    Acc#{Group => [Func | Members]};
                ({Group, _Anno, _Sig, _Doc, _Meta} = Func, Acc) ->
                    Members = maps:get(Group, Acc, []),
                    Acc#{Group => [Func | Members]}
            end,
            #{},
            lists:sort(FDocs)
        ),
    lists:map(
        fun({Group, Members}) ->
            lists:map(
                fun(_Member = {_, _, _, hidden, _}) ->
                        {error,hidden};
                    (Member = {_, _, _, Doc, _}) ->
                    Sig = render_signature(Member),
                    LocalDoc =
                        if
                            Doc =:= #{} ->
                                case lists:keyfind(Group, 1, Docs) of
                                    false ->
                                        get_local_doc(Group, none, D);
                                    {_, _, _, GroupDoc, _} ->
                                        get_local_doc(Group, GroupDoc, D)
                                end;
                            true ->
                                get_local_doc(Group, Doc, D)
                        end,
                    render_headers_and_docs(
                        [Sig], LocalDoc, D, Config
                    )
                end,
                Members
            )
        end,
        maps:to_list(Grouping)
    ).

%% Render the signature of either function, type, or anything else really.
-spec render_signature(chunk_entry()) -> chunk_elements().
render_signature({{_Type, _F, _A}, _Anno, _Sigs, _Docs, #{signature := Specs} = Meta}) ->
    lists:flatmap(
        fun(ASTSpec) ->
            PPSpec = erl_pp:attribute(ASTSpec, [{encoding, utf8}]),
            Spec =
                case ASTSpec of
                    {_Attribute, _Line, opaque, _} ->
                        %% We do not want show the internals of the opaque type
                        hd(string:split(PPSpec, "::"));
                    _ ->
                        trim_spec(PPSpec)
                end,
            BinSpec =
                unicode:characters_to_binary(
                    string:trim(Spec, trailing, "\n")
                ),
            [
                {pre, [], BinSpec},
                {hr, [], []}
                | render_meta(Meta)
            ]
        end,
        Specs
    );
render_signature({{_Type, _F, _A}, _Anno, Sigs, _Docs, Meta}) ->
    [{pre, [], Sigs}, {hr, [], []} | render_meta(Meta)].

-spec trim_spec(unicode:chardata()) -> unicode:chardata().
trim_spec(Spec) ->
    unicode:characters_to_binary(
        string:trim(
            lists:join($\n, trim_spec(string:split(Spec, "\n", all), 0)),
            trailing,
            "\n"
        )
    ).
-spec trim_spec([unicode:chardata()], non_neg_integer()) -> unicode:chardata().
trim_spec(["-spec " ++ Spec | T], 0) ->
    [Spec | trim_spec(T, 6)];
trim_spec([H | T], N) ->
    case re:run(H, io_lib:format("(\\s{~p}\\s+)when", [N]), [{capture, all_but_first}]) of
        {match, [{0, Indent}]} ->
            trim_spec([H | T], Indent);
        nomatch ->
            case string:trim(string:slice(H, 0, N), both) of
                "" ->
                    [
                        re:replace(string:slice(H, N, infinity), "  ", " ", [global])
                        | trim_spec(T, N)
                    ];
                _ ->
                    [re:replace(H, "  ", " ", [global]) | trim_spec(T, N)]
            end
    end;
trim_spec([], _N) ->
    [].

-spec render_meta(map()) -> chunk_elements().
render_meta(Meta) ->
    case
        lists:flatmap(
            fun
                ({since, Vsn}) ->
                    [{em, [], <<"Since:">>}, <<" ">>, Vsn];
                ({deprecated, Depr}) ->
                    [{em, [], <<"Deprecated: ">>}, <<" ">>, Depr];
                (_) ->
                    []
            end,
            maps:to_list(Meta)
        )
    of
        [] ->
            [];
        Docs ->
            Docs
    end.

-spec render_headers_and_docs([chunk_elements()], chunk_elements(), #docs_v1{}, map()) ->
    unicode:chardata().
render_headers_and_docs(Headers, DocContents, D, Config) ->
    render_headers_and_docs(Headers, DocContents, init_config(D, Config)).
-spec render_headers_and_docs([chunk_elements()], chunk_elements(), #config{}) ->
    unicode:chardata().
render_headers_and_docs(Headers, DocContents, #config{} = Config) ->
    [
        render_docs(
            lists:flatmap(
                fun(Header) ->
                    [{br, [], []}, Header]
                end,
                Headers
            ),
            Config
        ),
        "\n",
        render_docs(DocContents, 0, Config)
    ].

-spec render_typecb_docs([TypeCB] | TypeCB, #config{}) ->
    unicode:chardata() | {'error', 'type_missing'}
when
    TypeCB :: {
        {type | callback, Name :: atom(), Arity :: non_neg_integer()},
        Encoding :: binary(),
        Sig :: [binary()],
        none | hidden | #{binary() => chunk_elements()}
    }.
render_typecb_docs([], _C) ->
    {error, type_missing};
render_typecb_docs(TypeCBs, #config{} = C) when is_list(TypeCBs) ->
    [render_typecb_docs(TypeCB, C) || TypeCB <- TypeCBs];
render_typecb_docs({_F, _, _Sig, hidden, _Meta} = _TypeCB, #config{docs = _D} = _C) ->
    {error, hidden};
render_typecb_docs({F, _, _Sig, Docs, _Meta} = TypeCB, #config{docs = D} = C) ->
    render_headers_and_docs(render_signature(TypeCB), get_local_doc(F, Docs, D), C).
-spec render_typecb_docs(chunk_elements(), #docs_v1{}, _) ->
    unicode:chardata() | {'error', 'type_missing'}.
render_typecb_docs(Docs, D, Config) ->
    render_typecb_docs(Docs, init_config(D, Config)).

%%% General rendering functions
render_docs(DocContents) ->
    formatter(render_docs(DocContents, init_config(undefined, []))).
-spec render_docs([chunk_element()], #config{}) -> unicode:chardata().
render_docs(DocContents, #config{} = Config) ->
    render_docs(DocContents, 0, Config);
render_docs(DocContents, #docs_v1{} = D) ->
    render_docs(DocContents, init_config(D, [])).
-spec render_docs([chunk_element()], 0, #config{}) -> unicode:chardata().
render_docs(DocContents, Ind, D = #config{}) when is_integer(Ind) ->
    try
        {Doc, _} = trimnl(render_docs(DocContents, [], 0, Ind, D)),
        unicode:characters_to_binary(Doc)
    catch throw:R:ST ->
            io:format("Failed to render: ~tp~n",[R]),
            erlang:raise(throw,R,ST);
          E:R:ST ->
            io:format("Failed to render: ~tp~n",[DocContents]),
            erlang:raise(E,R,ST)
    end.

-spec init_config(#docs_v1{} | undefined, _) -> #config{}.
init_config(D, _Config) ->
    #config{docs = D}.

-spec render_docs(
    Elems :: [chunk_element()],
    Stack :: [chunk_element_type()],
    non_neg_integer(),
    non_neg_integer(),
    #config{}
) ->
    {unicode:chardata(), non_neg_integer()}.
render_docs(Elems, State, Pos, Ind, D) when is_list(Elems) ->
    lists:mapfoldl(
        fun(Elem, P) ->
            render_docs(Elem, State, P, Ind, D)
        end,
        Pos,
        Elems
    );
render_docs(Elem, State, Pos, Ind, D) ->
    %    io:format("Elem: ~p (~p) (~p,~p)~n",[Elem,State,Pos,Ind]),
    render_element(Elem, State, Pos, Ind, D).

%%% The function is the main element rendering function
%%%
%%% Elem: The current element to process
%%% Stack: A stack of element names to see where we are in the dom
%%% Pos: The current print position on the current line
%%% Ind: How much the text should be indented after a newline
%%% Config: The renderer's configuration
%%%
%%% Each element is responsible for putting new lines AFTER itself
%%% The indents are done either by render_words when a newline happens
%%% or when a new element is to be rendered and Pos < Ind.
%%%
%%% Any block elements (i.e. p, ul, li etc) are responsible for trimming
%%% extra new lines. eg. <ul><li><p>content</p></li></ul> should only
%%% have two newlines at the end.
-spec render_element(
    Elem :: chunk_element(),
    Stack :: [chunk_element_type()],
    Pos :: non_neg_integer(),
    Indent :: non_neg_integer(),
    Config :: #config{}
) ->
    {unicode:chardata(), Pos :: non_neg_integer()}.

%% render_element({IgnoreMe,_,Content}, State, Pos, Ind,D)
%%   when IgnoreMe =:= a ->
%%     render_docs(Content, State, Pos, Ind,D);

%% Catch h* before the padding is done as they reset padding
render_element({Tag = h1, _, Content}, State, 0 = Pos, _Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["# ", Docs], NewPos});
render_element({Tag = h2, _, Content}, State, 0 = Pos, _Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["## ", Docs], NewPos});
render_element({Tag = h3, _, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["### ", Docs], NewPos});
render_element({Tag = h4, _, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["#### ", Docs], NewPos});
render_element({Tag = h5, _, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["##### ", Docs], NewPos});
render_element({Tag = h6, _, Content}, State, Pos, _Ind, D) when Pos =< 2 ->
    {Docs, NewPos} = render_docs(Content, [Tag|State], Pos, 0, D),
    trimnlnl({["###### ", Docs], NewPos});
render_element({pre, _Attr, _Content} = E, State, Pos, Ind, D) when Pos > Ind ->
    %% We pad `pre` with two newlines if the previous section did not indent the region.
    {Docs, NewPos} = render_element(E, State, 0, Ind, D),
    {["\n\n", Docs], NewPos};
render_element({br, _Attr, _Content}, [td|_State], Pos, _Ind, _D)  ->
    {" ", Pos + 1};
render_element({br, _Attr, _Content} = E, State, Pos, Ind, D) when Pos > Ind ->
    {Docs, NewPos} = render_element(E, State, 0, Ind, D),
    {["  \n", Docs], NewPos};
render_element({Elem, _Attr, _Content} = E, State, Pos, Ind, D) when Pos > Ind, ?IS_BLOCK(Elem) ->
    {Docs, NewPos} = render_element(E, State, 0, Ind, D),
    {["\n", Docs], NewPos};
render_element({'div', [{class, What}], Content}, State, Pos, Ind, D) ->
    Type = case What of
               <<"warning">> -> What;
               <<"error">> -> What;
               <<"note">> -> <<"info">>;
               <<"change">> -> <<"neutral">>;
               <<"do">> -> <<"neutral">>;
               <<"dont">> -> <<"neutral">>
           end,
    Title = unicode:characters_to_binary([string:titlecase(What), " {: .", Type, "}"]),
    {Header, 0} = render_element({h4, [], [Title]}, State, Pos, Ind, D),
    {Docs, 0} = render_element({'div', [], Content}, ['div' | State], 0, 0, D),
    trimnlnl([pad(Ind - Pos), "> ", string:trim(Header), "\n",
              [[pad(Ind), string:trim(["> ",Line]),"\n"] || Line <- string:split([trim(Docs)],"\n",all)]]);
render_element({Tag, _, Content}, State, Pos, Ind, D) when Tag =:= p; Tag =:= 'div' ->
    trimnlnl(render_docs(Content, [Tag | State], Pos, Ind, D));
render_element({a, [{id,_Id}], []} = A, State, Pos, Ind, D) when Pos > 0 ->
    {Docs, NewPos} = render_element(A, State, 0, Ind, D),
    {["\n",Docs], NewPos};
render_element({a, [{id,Id}], []}, _State, Pos, _Ind, _D) ->
    trimnl({["<a id=\"", Id, "\"/>\n"], Pos});
render_element({dl, [], [{dt,DTAttr,DTContent}, {dd, DDAttr, DDContent} | DLContent]}, State, Pos, Ind, D) when State =:= []; hd(State) =/= a_fix ->
    FilterFun = fun F({a,[Id],_}, {As, Acc}) ->
                        {[Id | As], Acc};
                    F({Tag, Attr, C}, {As, Acc}) when Tag =/= dl ->
                        {NewAs, NewC} = lists:foldl(F, {As, []}, C),
                        {NewAs, [{Tag, Attr, lists:reverse(NewC)} | Acc]};
                    F(Elem, {As, Acc}) when is_binary(Elem); element(1, Elem) =:= dl ->
                        {As, [Elem | Acc]}
                end,
    {DTAs, NewDTContent} = lists:foldl(FilterFun, {[],[]}, DTContent),
    {As,   NewDDContent} = lists:foldl(FilterFun, {DTAs,[]}, DDContent),
    %% io:format("Hoist: ~p~n",[As]),
    render_docs([{dl, [], [{dt, As ++ DTAttr, lists:reverse(NewDTContent)},
                           {dd, DDAttr, lists:reverse(NewDDContent)} | DLContent]}],
                [a_fix | State], Pos, Ind, D);
render_element(Elem, State, Pos, Ind, D) when Pos < Ind ->
    {Docs, NewPos} = render_element(Elem, State, Ind, Ind, D),
    {[pad(Ind - Pos), Docs], NewPos};
render_element({a, Attr, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, State, Pos, Ind, D),
    Href = proplists:get_value(href, Attr),
    undefined = proplists:get_value(id, Attr),
    IsOTPLink = Href =/= undefined andalso string:find(Href, ":") =/= nomatch,
    case proplists:get_value(rel, Attr) of
        undefined when Href =/= undefined ->
            {["[", Docs, "](", Href, ")"], NewPos};
        _ when not IsOTPLink ->
            {Docs, NewPos};
        <<"https://erlang.org/doc/link/seemfa">> ->
            [_App, MFA] = string:split(Href, ":"),
            [Mod, FA] = string:split(MFA, "#"),
            {Prefix, Func, Arity} =
                case string:split(FA, "/") of
                    [<<"Module:", F/binary>>, A] ->
                        {"c:",F, A};
                    [F, A] ->
                        {"", F, A}
                end,
            {
             [
              "[", Docs, "](`",Prefix,Mod,":",Func,"/",Arity,"`)"
             ],
             NewPos
            };
        <<"https://erlang.org/doc/link/seetype">> ->
            case string:lexemes(Href, ":#/") of
                [_App, Mod, Type, Arity] ->
                    {
                     [
                      "[", Docs, "](`t:",Mod,":",Type,"/",Arity,"`)"
                     ],
                     NewPos
                    };
                [_App, Mod, Type] ->
                    {
                     [
                      "[", Docs, "](`t:",Mod,":",Type,"/0`)"
                     ],
                     NewPos
                    }
            end;
        <<"https://erlang.org/doc/link/seeerl">> ->
            case string:lexemes(Href, ":#") of
                [_App, Mod] ->
                    {["[", Docs, "](`m:", Mod, "`)"], NewPos};
                [_App, Mod, Anchor] ->
                    {["[", Docs, "](`m:", Mod, "#", Anchor, "`)"], NewPos}
            end;
        <<"https://erlang.org/doc/link/seeguide">> ->
            CurrentApplication = unicode:characters_to_binary(get(application)),
            RemoveSystemApp = fun(<<"system",_/binary>>) ->
                                      <<"system">>;
                                 (Else) ->
                                      Else
                              end,
            case string:lexemes(Href, ":#") of
                [App, Guide] when App =:= CurrentApplication ->
                    {["[", Docs, "](",Guide,".md)"], NewPos};
                [App, Guide, Anchor] when App =:= CurrentApplication ->
                    {["[", Docs, "](",Guide,".md#",Anchor,")"], NewPos};
                [App, Guide] ->
                    {["[", Docs, "](`p:",RemoveSystemApp(App),":",Guide,"`)"], NewPos};
                [App, Guide, Anchor] ->
                    {["[", Docs, "](`p:",RemoveSystemApp(App),":",Guide,"#",Anchor,"`)"], NewPos}
            end;
        See when See =:= <<"https://erlang.org/doc/link/seecref">>;
                 See =:= <<"https://erlang.org/doc/link/seecom">>;
                 See =:= <<"https://erlang.org/doc/link/seeapp">> ->
            CurrentApplication = unicode:characters_to_binary(get(application)),
            case string:lexemes(Href, ":#") of
                [App, Guide] when App =:= CurrentApplication ->
                    {["[", Docs, "](",Guide,".md)"], NewPos};
                [App, Guide, Anchor] when App =:= CurrentApplication ->
                    {["[", Docs, "](",Guide,".md#",Anchor,")"], NewPos};
                [App, Guide] ->
                    {["[", Docs, "](`p:",App,":",Guide,"`)"], NewPos};
                [App, Guide, Anchor] ->
                    {["[", Docs, "](`p:",App,":",Guide,"#",Anchor,"`)"], NewPos}
            end;
        <<"https://erlang.org/doc/link/seefile">> ->
            CurrentApplication = unicode:characters_to_binary(get(application)),
            case string:lexemes(Href, ":#") of
                [App, Guide] when App =:= CurrentApplication ->
                    {["[", Docs, "](",Guide,")"], NewPos};
                [App, Guide, Anchor] when App =:= CurrentApplication ->
                    {["[", Docs, "](",Guide,".md#",Anchor,")"], NewPos};
                [App, Guide] ->
                    {["[", Docs, "](`p:",App,":",Guide,"`)"], NewPos};
                [App, Guide, Anchor] ->
                    {["[", Docs, "](`p:",App,":",Guide,"#",Anchor,"`)"], NewPos}
            end;
        _ ->
            {Docs, NewPos}
    end;
render_element({code, _, Content}, [pre | _] = State, Pos, Ind, D) ->
    %% When code is within a pre we don't emit any underline
    render_docs(Content, [code | State], Pos, Ind, D);
render_element({code, _, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [code | State], Pos, Ind, D),
    case string:find(Docs, "`") of
        nomatch ->
            {["`", Docs, "`"], NewPos};
        _ ->
            {["`` ", Docs, " ``"], NewPos}
    end;
render_element({em, Attr, Content}, State, Pos, Ind, D) ->
    render_element({i, Attr, Content}, State, Pos, Ind, D);
render_element({i, _, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, [i | State], Pos, Ind, D),
    case lists:member(pre, State) of
        true ->
            {[Docs], NewPos};
        false ->
            {["*", Docs, "*"], NewPos}
    end;
render_element({hr, [], []}, _State, Pos, _Ind, _D) ->
    {"---\n", Pos};
render_element({br, [], []}, _State, Pos, _Ind, _D) ->
    {"", Pos};
render_element({strong, Attr, Content}, State, Pos, Ind, D) ->
    render_element({b, Attr, Content}, State, Pos, Ind, D);
render_element({b, _, Content}, State, Pos, Ind, D) ->
    {Docs, NewPos} = render_docs(Content, State, Pos, Ind, D),
    case lists:member(pre, State) of
        true ->
            {[Docs], NewPos};
        false ->
            {["__", Docs, "__"], NewPos + 4}
    end;
render_element({pre, [], [{code,Attr,Content}]}, State, Pos, Ind, D) ->
    %% This is a pre without any links or emphasis, so we use markdown

    %% For pre we make sure to respect the newlines in pre
    {Docs, _} = trimnl(render_docs(strip_tags(Content), [pre | State], Pos, Ind, D)),
    Type =
        case proplists:get_value(type, Attr) of
            undefined -> "text";
            <<"none">> -> "text";
            <<"erl">> -> "erlang";
            <<"erl-repl">> -> "erlang";
            <<"c">> -> "c"
        end,
    trimnlnl(["```",Type,"\n", pad(Ind), Docs, pad(Ind), "```"]);
render_element({ul, [{class, <<"types">>}], Content}, State, _Pos, Ind, D) ->
    {Docs, _} = render_docs(Content, [types | State], 0, Ind, D),
    trimnlnl(Docs);
render_element({li, Attr, Content}, [types | _] = State, Pos, Ind, C) ->
    Doc =
        case {proplists:get_value(name, Attr), proplists:get_value(class, Attr)} of
            {undefined, Class} when Class =:= undefined; Class =:= <<"type">> ->
                %% Inline html for types
                render_docs(Content ++ [<<"  ">>], [type | State], Pos, Ind, C);
            {_, <<"description">>} ->
                %% Inline html for type descriptions
                render_docs(Content ++ [<<"  ">>], [type | State], Pos, Ind + 2, C);
            {Name, _} ->
                %% Try to render from type metadata
                case render_type_signature(binary_to_atom(Name), C) of
                    undefined when Content =:= [] ->
                        %% Failed and no content, emit place-holder
                        {["```erlang\n-type ", Name, "() :: term().```"], 0};
                    undefined ->
                        %% Failed with metadata, render the content
                        render_docs(Content ++ [<<"  ">>], [type | State], Pos, Ind, C);
                    Type ->
                        %% Emit the erl_pp typespec
                        {["```erlang\n", Type, "```"], 0}
                end
        end,
    trimnl(Doc);
render_element({ul, [], Content}, State, Pos, Ind, D) ->
    trimnlnl(render_docs(Content, [ul | State], Pos, Ind, D));
render_element({ol, [], Content}, State, Pos, Ind, D) ->
    trimnlnl(render_docs(Content, [ol | State], Pos, Ind, D));
render_element({li, [], Content}, [ul | _] = State, Pos, Ind, D) ->
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos + 2, Ind + 2, D),
    trimnl(["* ", Docs]);
render_element({li, [], Content}, [ol | _] = State, Pos, Ind, D) ->
    {Docs, _NewPos} = render_docs(Content, [li | State], Pos + 2, Ind + 2, D),
    trimnl(["1. ", Docs]);
render_element({dl, [], [{dt,DTAttr,DTContent}, {dd,[],DDContent} | Content]}, [a_fix | State], Pos, Ind, D) ->
    Since = proplists:get_value(since, DTAttr),
    {DTDocs, _DTNewPos} =
        render_docs(
          [{b, [], DTContent}],
          [li, dl | State],
          Pos + 2,
          Ind + 2,
          D),
    Ids = [Id || {id,Id} <- DTAttr],
    DTDocsWAnchors = case Ids of
                         [] -> trim(DTDocs);
                         Ids -> [trim(DTDocs),"{:",[[" #",Id] || Id <- Ids]," }"]
                     end,
    {DDDocs, DDNewPos} = render_docs(DDContent, [li, dl | State], Pos + 2, Ind + 2, D),
    {Docs, NewPos} =
        case string:find(DTDocs, "\n") of
            nomatch when Since =:= undefined, is_binary(hd(DDContent)) orelse element(1,hd(DDContent)) =/= pre ->
                trimnlnl({["* ", trim(DTDocsWAnchors), " - ", string:trim(string:trim(DDDocs, both, "\n"), leading, " ")], DDNewPos});
            _ ->
                trimnlnl({["* ", trim(DTDocsWAnchors), [["(Since ",Since,")"] || Since =/= undefined],"  \n",
                           pad(2 + Ind - Pos), DDDocs], DDNewPos})
        end,
    {DLDocs, DLPos} = render_element({dl, [], Content}, State, NewPos, Ind, D),
    {[Docs,DLDocs], DLPos};
render_element({dl, [], []}, _State, Pos, _Ind, _D) ->
    {"", Pos};
render_element({table, _, Rows}, State, Pos, Ind, D) ->
    [{tr,_,Head} | RowsNoCaption] = [Row || {tr,_,_} = Row <- Rows],
    trimnlnl(
      render_docs(
        [{th, [], Head} | RowsNoCaption] ++
            [{em, [], [<<"Table: ">>, C]} || {caption,_,C} <- Rows],
        [table|State], Pos, Ind, D));
render_element({th, [], Head}, State, _Pos, _Ind, D) ->
    Header =
        [begin {Docs, _} = render_docs(Td, [th|State], 0, 0, D),
               {["| ", Docs, " "], ["|-", lists:duplicate(string:length(Docs), $-), "-"]}
         end || Td <- Head],
    trimnl({[[ Docs || {Docs,_} <- Header ], "|\n",
             [ Lines || {_, Lines} <- Header ], "|\n"], 0});
render_element({tr, [], Row}, State, _Pos, _Ind, D) ->
    Rows =
        [begin {Docs, _} = render_docs(Td, [tr|State], 0, 0, D),
               ["| ", Docs, " "]
         end || Td <- Row],
    trimnl({[ Rows, "|"], 0});
render_element({td, _, TDContent}, State, Pos, Ind, D) ->
    render_docs(TDContent, [td|State], Pos, Ind, D);
render_element({img,Attr,Content}, _State, Pos, _Ind, _D) ->
    Caption = case lists:keyfind(caption, 1, Content) of
                  false -> "";
                  {caption, _, C} ->
                      C
              end,
    trimnlnl({["![",Caption,"](",filename:join("assets",filename:basename(proplists:get_value(file,Attr))),
               " \"",Caption,"\")\n"], Pos});
render_element({quote, [], Content}, State, _Pos, Ind, D) ->
    {Docs, 0} = render_element({'div', [], Content}, ['div' | State], 0, 0, D),
    trimnlnl([[pad(Ind), "> ",Line,"\n"] || Line <- string:split(trim(Docs),"\n",all)]);
render_element(B, State, Pos, Ind, _D) when is_binary(B) ->
    Pre = string:replace(B, "\n", [nlpad(Ind)], all),
    EscapeChars = [
        "\\",
        "`",
        "*",
        "_",
        "{",
        "}",
        "[",
        "]",
        "<",
        ">",
        "(",
        ")",
        "#",
        "+",
        "-",
        ".",
        "!",
        "|"
    ],
    Str =
        case State of
            [pre | _] ->
                Pre;
            [code | _] ->
                Pre;
            [h4 | _] ->
                Pre;
            _ ->
                re:replace(
                    Pre,
                    ["(", lists:join($|, [["\\", C] || C <- EscapeChars]), ")"],
                    "\\\\\\1",
                    [global, unicode]
                )
        end,
    {Str, Pos + lastline(Str)};
render_element({Tag, Attr, Content}, State, Pos, Ind, D) ->
    case lists:member(Tag, ?ALL_ELEMENTS) of
        true ->
            throw({unhandled_element, Tag, Attr, Content});
        false ->
            throw({unknown_element, Tag, Attr, Content}),
            ok
    end,
    render_docs(Content, State, Pos, Ind, D).

-spec render_type_signature(atom(), #config{}) -> 'undefined' | unicode:chardata().
render_type_signature(Name, #config{docs = #docs_v1{metadata = #{types := AllTypes}}}) ->
    case [Type || Type = {TName, _} <- maps:keys(AllTypes), TName =:= Name] of
        [] ->
            undefined;
        Types ->
            [erl_pp:attribute(maps:get(Type, AllTypes)) || Type <- Types]
    end.

%% Pad N spaces (and possibly pre-prend newline), disabling any ansi formatting while doing so.
-spec pad(non_neg_integer()) -> unicode:chardata().
pad(N) ->
    pad(N, "").
-spec nlpad(non_neg_integer()) -> unicode:chardata().
nlpad(N) ->
    pad(N, "\n").
-spec pad(non_neg_integer(), unicode:chardata()) -> unicode:chardata().
pad(N, Extra) ->
    Pad = lists:duplicate(N, [$\s]),
    [Extra, Pad].

-spec lastline(unicode:chardata()) -> non_neg_integer().
%% Look for the length of the last line of a string
lastline(Str) ->
    LastStr =
        case string:find(Str, "\n", trailing) of
            nomatch ->
                Str;
            Match ->
                tl(string:next_codepoint(Match))
        end,
    string:length(LastStr).

strip_tags([H|T]) when is_binary(H) ->
    [H|strip_tags(T)];
strip_tags([{_Tag,_,C}|T]) ->
    [strip_tags(C)|strip_tags(T)];
strip_tags([]) ->
    [].

%% These functions make sure that we trim extra newlines added
%% by the renderer. For example if we do <li><p></p></li>
%% that would add 4 \n at after the last </li>. This is trimmed
%% here to only be 2 \n
-spec trimnlnl(unicode:chardata() | {unicode:chardata(), non_neg_integer()}) ->
    {unicode:chardata(), 0}.
trimnlnl({Chars, _Pos}) ->
    nl(nl(string:trim(Chars, trailing, "\n")));
trimnlnl(Chars) ->
    nl(nl(string:trim(Chars, trailing, "\n"))).
-spec trimnl(unicode:chardata() | {unicode:chardata(), non_neg_integer()}) ->
    {unicode:chardata(), 0}.
trimnl({Chars, _Pos}) ->
    nl(string:trim(Chars, trailing, "\n"));
trimnl(Chars) ->
    nl(string:trim(Chars, trailing, "\n")).
trim(Chars) ->
    string:trim(Chars, trailing, "\n").
-spec nl(unicode:chardata() | {unicode:chardata(), non_neg_integer()}) -> {unicode:chardata(), 0}.
nl({Chars, _Pos}) ->
    nl(Chars);
nl(Chars) ->
    {[Chars, "\n"], 0}.
