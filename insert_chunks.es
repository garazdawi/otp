#!/usr/bin/env escript

-include_lib("kernel/include/eep48.hrl").

main([BeamFile | T]) ->
    try
        {ok, Module, Chunks} = beam_lib:all_chunks(BeamFile),
        {debug_info_v1, _, {AST, _Meta}} = binary_to_term(proplists:get_value("Dbgi", Chunks)),
        NewDocs =
            case lists:keysearch(moduledoc, 3, AST) of
                {value, {attribute, ModuleDocAnno, moduledoc, ModuleDoc}} ->
                    D = #docs_v1{ format = <<"text/markdown">> },
                    MD = D#docs_v1{ anno = ModuleDocAnno, module_doc = #{ <<"en">> => unicode:characters_to_binary(ModuleDoc) } },
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
    extract_docs(expand_anno(AST), undefined).
extract_docs([{attribute, Anno, doc, Doc}|T], undefined) ->
    extract_docs(T, {Anno, Doc});
extract_docs([{function, _, F, A,_}|T],{Anno, Doc}) ->
    [{{function, F, A}, Anno, [unicode:characters_to_binary(io_lib:format("~p/~p",[F,A]))],
      #{ <<"en">> => unicode:characters_to_binary(Doc) }, #{ }} | extract_docs(T, undefined)];
extract_docs([_|T], Doc) ->
    extract_docs(T, Doc);
extract_docs([], undefined) ->
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

convert_docs(#docs_v1{ format = ?NATIVE_FORMAT, module_doc = #{ <<"en">> := ModuleDoc } } = D) ->
    D#docs_v1{ format = <<"text/markdown">>,
               module_doc = #{ <<"en">> =>
                                   try eep48_to_markdown:render_docs(ModuleDoc, D)
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
        {What, Anno, Sig, #{ <<"en">> => eep48_to_markdown:render_docs(Docs, D) }, Meta}
    catch E:R:ST ->
            io:format("Failed to convert ~p~n",[What]),
            erlang:raise(E,R,ST)
    end;
convert_docs(F, _) ->
    F.
