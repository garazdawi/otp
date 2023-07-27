#!/usr/bin/env escript
-feature(maybe_expr, enable).
-mode(compile).

-include_lib("kernel/include/eep48.hrl").

main([BeamFile | T]) ->
    try
        {ok, Module, Chunks} = beam_lib:all_chunks(BeamFile),
        {debug_info_v1, _, {AST, Meta}} = binary_to_term(proplists:get_value("Dbgi", Chunks)),
        put(meta, Meta),
        NewDocs =
            case lists:keysearch(moduledoc, 3, AST) of
                {value, {attribute, ModuleDocAnno, moduledoc, ModuleDoc}} ->
                    D = #docs_v1{ format = <<"text/markdown">> },
                    MD = D#docs_v1{
                           anno = ModuleDocAnno,
                           module_doc = #{ <<"en">> => unicode:characters_to_binary(ModuleDoc) } },
                    MD#docs_v1{ docs = extract_docs(AST) };
                _ ->
                    case get_doc_chunk(BeamFile, atom_to_list(Module)) of
                        {ok, Docs} ->
                            case lists:reverse(filename:split(code:which(Module))) of
                                ["preloaded"] ->
                                    put(application, "erts");
                                [_File,_Ebin,App | _] ->
                                    put(application, App)
                            end,
                            convert_docs(Docs);
                        Else ->
                            exit(Else)
                    end
            end,
        {ok, NewBeamFile} = beam_lib:build_module([{"Docs",term_to_binary(NewDocs)} | proplists:delete("Docs", Chunks)]),
        file:write_file(BeamFile, NewBeamFile)
    catch E:R:ST ->
            io:format("Failed to convert ~ts~n",[BeamFile]),
            erlang:raise(E, R, ST)
    end,
    main(T);
main([]) ->
    ok.

get_doc_chunk(Filename, Mod) ->
    RootDir = code:root_dir(),
    case filename:dirname(Filename) of
        Filename ->
            {error,missing};
        RootDir ->
            {error,missing};
        Dir ->
            ChunkFile = filename:join([Dir,"doc","chunks",Mod ++ ".chunk"]),
            case file:read_file(ChunkFile) of
                {ok, Bin} ->
                    {ok, binary_to_term(Bin)};
                {error,enoent} ->
                    get_doc_chunk(Dir, Mod);
                {error,Reason} ->
                    {error,Reason}
            end
    end.

extract_docs(AST) ->
    extract_docs(expand_anno(AST), {undefined, #{}}).
extract_docs([{attribute, _Anno, doc, MoreMeta}|T], {Doc, Meta}) when is_map(MoreMeta) ->
        extract_docs(T, {Doc, maps:merge(Meta, MoreMeta)});
extract_docs([{attribute, _Anno, doc, Doc}|T], {undefined, Meta}) ->
    extract_docs(T, {string:trim(Doc), Meta});
extract_docs([{function, Anno, F, A, Body}|T],{Doc, Meta}) when Doc =/= undefined ->

    %% io:format("Converting ~p/~p~n",[F,A]),

    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, F, A) of
            undefined ->
                %% Then we check if we can get good names from function arguments
                %% io:format("What: ~p~n",[_E]),
                maybe
                    [{clause, _, ClauseArgs, _, _}] ?= Body,
                    true ?= lists:all(fun(E) -> element(1, E) =:= var end, ClauseArgs),
                    {extract_slogan_from_args(F, ClauseArgs), Doc}
                else
                    _E2 ->
                        %% io:format("What: ~p~n",[_E2]),
                        %% Lastly we just print name/arity
                        {io_lib:format("~p/~p",[F,A]), Doc}
                end;
            SloganDocs ->
                SloganDocs
        end,
    [{{function, F, A}, Anno, [unicode:characters_to_binary(Slogan)],
      #{ <<"en">> => unicode:characters_to_binary(string:trim(DocsWithoutSlogan)) }, Meta} | extract_docs(T, {undefined, #{}})];
extract_docs([{attribute, Anno, type, {Type, _, Args}}|T],{Doc, Meta}) when Doc =/= undefined ->

    io:format("Converting ~p/~p~n",[Type,length(Args)]),

    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, Type, length(Args)) of
            undefined ->
                maybe
                    true ?= lists:all(fun(E) -> element(1, E) =:= var end, Args),
                    {extract_slogan_from_args(Type, Args), Doc}
                else
                    _ -> {io_lib:format("~p/~p",[Type,length(Args)]), Doc}
                end;
            SloganDocs ->
                SloganDocs
        end,
    [{{type, Type, length(Args)}, Anno, [unicode:characters_to_binary(Slogan)],
      #{ <<"en">> => unicode:characters_to_binary(string:trim(DocsWithoutSlogan)) }, Meta} | extract_docs(T, {undefined, #{}})];
extract_docs([_|T], Doc) ->
    extract_docs(T, Doc);
extract_docs([], {undefined, _}) ->
    [].

extract_slogan(Doc, F, A) ->
    maybe
        [MaybeSlogan | Rest] = string:split(Doc, "\n"),
        %% io:format("  MaybeSlogan: ~p~n",[MaybeSlogan]),
        {ok, Toks, _} ?= erl_scan:string(unicode:characters_to_list([MaybeSlogan,"."])),
        {ok, [{call,_,{atom,_,F},Args}]} ?= erl_parse:parse_exprs(Toks),
        A ?= length(Args),
        {MaybeSlogan, Rest}
    else
        _ -> undefined
    end.

extract_slogan_from_args(F, Args) ->
    io_lib:format("~p(~ts)",[F, lists:join($,,[string:trim(atom_to_list(Arg),leading,"_") || {var, _, Arg} <- Args])]).

expand_anno(AST) ->
    {NewAST, _} =
        lists:mapfoldl(fun F({attribute, _, file, {NewFile, _}} = E, File) when NewFile =/= File ->
                               F(E, NewFile);
                           F(E, File) ->
                               {setelement(2, E, erl_anno:set_file(File, element(2, E))), File}
                       end, undefined, AST),
    %% io:format("NewAST: ~p~n",[NewAST]),
    NewAST.

convert_docs(#docs_v1{ format = ?NATIVE_FORMAT, module_doc = #{ <<"en">> := ModuleDoc } } = D) ->
    D#docs_v1{ format = <<"text/markdown">>,
               module_doc = #{ <<"en">> =>
                                   try eep48_to_markdown:render_docs(shell_docs:normalize(ModuleDoc), D)
                                   catch E:R:ST ->
                                           io:format("Failed to convert moduledoc~n"),
                                           erlang:raise(E,R,ST)
                                   end},
               docs = [convert_docs(F, D) || F <- D#docs_v1.docs] };
convert_docs(#docs_v1{ format = ?NATIVE_FORMAT, module_doc = hidden } = D) ->
    D#docs_v1{ format = <<"text/markdown">> };
convert_docs(#docs_v1{ format = <<"text/markdown">> } = D) ->
    %% Already converted
    D.

convert_docs({What, Anno, Sig, #{ <<"en">> := Docs }, Meta}, D) ->
    try
        {What, Anno, Sig, #{ <<"en">> => eep48_to_markdown:render_docs(shell_docs:normalize(Docs), D) }, Meta}
    catch E:R:ST ->
            io:format("Failed to convert ~p~n",[What]),
            erlang:raise(E,R,ST)
    end;
convert_docs(F, _) ->
    F.
