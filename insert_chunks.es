#!/usr/bin/env escript

-include_lib("kernel/include/eep48.hrl").

main([BeamFile | T]) ->
    {ok, _Module, Chunks} = beam_lib:all_chunks(BeamFile),
    {debug_info_v1, _, {AST, _Meta}} = binary_to_term(proplists:get_value("Dbgi", Chunks)),
    D = #docs_v1{ format = <<"text/markdown">> },
    MD =
        case lists:keysearch(moduledoc, 3, AST) of
            {value, {attribute, ModuleDocAnno, moduledoc, ModuleDoc}} ->
                D#docs_v1{ anno = ModuleDocAnno, module_doc = #{ <<"en">> => unicode:characters_to_binary(ModuleDoc) } };
            false ->
                D#docs_v1{ anno = erl_anno:new(0), module_doc = none }
        end,
    NewChunks = [{"Docs",term_to_binary(MD#docs_v1{ docs = extract_docs(AST) } )} | proplists:delete("Docs", Chunks)],
    {ok, NewBeamFile} = beam_lib:build_module(NewChunks),
    file:write_file(BeamFile, NewBeamFile),
    main(T);
main([]) ->
    ok.

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

