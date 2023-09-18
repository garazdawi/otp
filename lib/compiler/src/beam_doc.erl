-module(beam_doc).

-feature(maybe_expr, enable).

-export([main/2]).

-include_lib("kernel/include/eep48.hrl").

-spec main(term(), term()) -> term().
main(Dirname, AST) ->
    try
        {value, {attribute, ModuleDocAnno, moduledoc, ModuleDoc}} = lists:keysearch(moduledoc, 3, AST),
        _Docs = extract_documentation(AST, new_docs(Cwd)),
        #docs_v1{
           format = <<"text/markdown">>,
           anno = ModuleDocAnno,
           module_doc = #{ <<"en">> => unicode:characters_to_binary(ModuleDoc) },
           docs = extract_docs(AST, Dirname) }
    catch E:R:ST ->
            io:format("Failed to convert ~ts~n",[AST]),
            erlang:raise(E, R, ST)
    end.

%% State Module:
%% moduledoc format
%% moduledoc file
%% CWD

%% State function
%% Meta function docs are merged


%% Commons:
%% - trim Doc for function and module
%% AccDoc :: { Doc, map with meta info }

-record(docs, {cwd  :: unicode:chardata(),
               doc  = undefined :: unicode:chardata(),
               meta = maps:new() :: maps:map()}).
-type internal_docs() :: #docs{}.

-spec new_docs(Cwd :: unicode:chardata()) -> internal_docs().
new_docs(Cwd) ->
    #docs{cwd = Cwd}.

update_meta(#docs{meta = Meta0}=State, Meta1) ->
    State#docs{meta = maps:merge(Meta0, Meta1)}.

update_doc(#docs{}=State, Doc) ->
    State#docs{doc = string:trim(Doc)}.


extract_documentation([{attribute, _Anno, doc, Meta0} | T], State) when is_map(Meta) ->
    extract_documentation(T, update_meta(State, Meta0));
extract_documentation([{attribute, _Anno, doc, {file, Path}}|T], #docs{cwd = Cwd}=State) ->
    maybe
        %% TODO: treat this as an include file, epp module?
        {ok, Doc} ?= file:read_file(filename:join(Cwd, Path)),
        extract_documentation(T, update_doc(State, Doc))
    else
        _ ->
            io:format("Failed to open: ~p~n",[filename:join(Cwd, Path)]),
            exit(1)
    end;
extract_documentation([{attribute, _Anno, doc, Doc} | T], State) ->
    extract_documentation(T, update_doc(State, Doc));
extract_documentation([{Kind, _Anno, _F, _A, _Body} | _T]=AST,
                      #docs{ doc = undefined,
                             meta = #{ equiv := {EquivF,EquivA}}}=State) ->
    Doc = io_lib:format("Equivalent to `~ts~p/~p`",[prefix(Kind), EquivF,EquivA]),
    extract_documentation(AST, update_doc(State, Doc));
extract_documentation([{Kind, _Anno, _F, _A, _Body} | _T]=AST,
                      #docs{ doc = undefined,
                             meta = #{ equiv := {call,_,{atom,_,EquivF},Args} = Call} = Meta}=State) ->
    Doc = io_lib:format("Equivalent to `~ts~ts`",[prefix(Kind),erl_pp:exprs([Call])]),
    Meta1 = Meta#{ equiv := {EquivF, length(Args)}},
    extract_documentation(AST, State#docs{doc = Doc, meta = Meta1});
extract_documentation([{function, Anno, F, A, Body}=Fun | T], #docs{meta = Doc}=State) when Doc =/= undefined ->
    %% io:format("Converting ~p/~p~n",[F,A]),

    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, F, A) of
            undefined ->
                %% Then we check if we can get good names from function arguments
                %% io:format("What: ~p~n",[_E]),
                maybe
                    [{clause, _, ClauseArgs, _, _}] ?= Body,
                    true ?= lists:all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, ClauseArgs),
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
    AttrBody = {function, F, A},
    FunDoc = gen_doc(Anno, AttrBody, Slogan, DocsWithoutSlogan, State),
    [FunDoc | extract_docs(T, new_docs(State#docs.cwd))];

extract_documentation([{attribute, Anno, TypeOrOpaque, {Type, _, TypeArgs}}|T], #docs{doc = Doc}=State)
  when Doc =/= undefined, TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque ->

    %% io:format("Converting ~p/~p~n",[Type,length(Args)]),
    Args = fun_to_varargs(TypeArgs),

    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, Type, length(Args)) of
            undefined ->
                maybe
                    true ?= lists:all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, Args),
                    {extract_slogan_from_args(Type, Args), Doc}
                else
                    _ -> {io_lib:format("~p/~p",[Type,length(Args)]), Doc}
                end;
            SloganDocs ->
                SloganDocs
        end,
    AttrBody = {type, Type, length(Args)},
    Doc = gen_doc(Anno, AttrBody, Slogan, DocsWithoutSlogan, State),
    [Doc | extract_documentation(T, new_docs(State#docs.cwd))];
extract_documentation([{attribute, Anno, callback, {{CB, A}, [Fun]}}|T],#docs{doc = Doc}=State)
  when Doc =/= undefined ->

    %% io:format("Converting ~p/~p~n",[CB,A]),

    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, CB, A) of
            undefined ->
                Args = fun_to_varargs(Fun),
                maybe
                    true ?= lists:all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, Args),
                    {extract_slogan_from_args(CB, Args), Doc}
                else
                    _ -> {io_lib:format("~p/~p",[CB,A]), Doc}
                end;
            SloganDocs ->
                SloganDocs
        end,
    AttrBody = {callback, CB, A},
    Doc = gen_doc(Anno, AttrBody, Slogan, DocsWithoutSlogan, State),

    [Doc | extract_documentation(T, new_docs(State#docs.cwd))];
extract_documentation([_H|T], State) ->
    extract_documentation(T, State);
extract_documentation([], #docs{doc = undefined}) ->
    [].

gen_doc(Anno, AttrBody, Slogan, Docs, #docs{meta = Meta}) ->
    %% TODO: missing expand anno
    {AttrBody, Anno, [unicode:characters_to_binary(Slogan)],
      #{ <<"en">> => unicode:characters_to_binary(string:trim(Docs)) }, Meta}.


extract_docs(AST, Cwd) ->
    extract_docs(expand_anno(AST), {undefined, #{}}, Cwd).

%% Done
extract_docs([{attribute, _Anno, doc, MoreMeta}|T], {Doc, Meta}, Cwd) when is_map(MoreMeta) ->
        extract_docs(T, {Doc, maps:merge(Meta, MoreMeta)}, Cwd);

%% Done
extract_docs([{attribute, _Anno, doc, {file, Path}}|T], {_, Meta}, Cwd) ->
    maybe
        %% TODO: treat this as an include file, epp module?
        {ok, Doc} ?= file:read_file(filename:join(Cwd, Path)),
        extract_docs(T, {string:trim(Doc), Meta}, Cwd)
    else
        _ ->
            io:format("Failed to open: ~p~n",[filename:join(Cwd, Path)]),
            exit(1)
    end;

%% Done
extract_docs([{attribute, _Anno, doc, Doc}|T], {_, Meta}, Cwd) ->
    extract_docs(T, {string:trim(Doc), Meta}, Cwd);
%% Done
extract_docs([{Kind, Anno, F, A, Body}|T],{undefined, #{ equiv := {EquivF,EquivA} } = Meta}, Cwd) ->
    extract_docs([{Kind, Anno, F, A, Body}|T],
                 {io_lib:format("Equivalent to `~ts~p/~p`",[prefix(Kind), EquivF,EquivA]), Meta}, Cwd);
%% Done
extract_docs([{Kind, Anno, F, A, Body}|T],{undefined, #{ equiv := {call,_,{atom,_,EquivF},Args} = Call} = Meta}, Cwd) ->
    extract_docs([{Kind, Anno, F, A, Body}|T],
                 {io_lib:format("Equivalent to `~ts~ts`",[prefix(Kind),erl_pp:exprs([Call])]),
                  Meta#{ equiv := {EquivF, length(Args)}}}, Cwd);

%% Done
extract_docs([{function, Anno, F, A, Body}|T],{Doc, Meta}, Cwd) when Doc =/= undefined ->

    %% io:format("Converting ~p/~p~n",[F,A]),

    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, F, A) of
            undefined ->
                %% Then we check if we can get good names from function arguments
                %% io:format("What: ~p~n",[_E]),
                maybe
                    [{clause, _, ClauseArgs, _, _}] ?= Body,
                    true ?= lists:all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, ClauseArgs),
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
      #{ <<"en">> => unicode:characters_to_binary(string:trim(DocsWithoutSlogan)) }, Meta} | extract_docs(T, {undefined, #{}}, Cwd)];
extract_docs([{attribute, Anno, TypeOrOpaque, {Type, _, TypeArgs}}|T],{Doc, Meta}, Cwd)
  when Doc =/= undefined, TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque ->

    %% io:format("Converting ~p/~p~n",[Type,length(Args)]),
    Args = fun_to_varargs(TypeArgs),

    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, Type, length(Args)) of
            undefined ->
                maybe
                    true ?= lists:all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, Args),
                    {extract_slogan_from_args(Type, Args), Doc}
                else
                    _ -> {io_lib:format("~p/~p",[Type,length(Args)]), Doc}
                end;
            SloganDocs ->
                SloganDocs
        end,
    [{{type, Type, length(Args)}, Anno, [unicode:characters_to_binary(Slogan)],
      #{ <<"en">> => unicode:characters_to_binary(string:trim(DocsWithoutSlogan)) }, Meta} | extract_docs(T, {undefined, #{}}, Cwd)];
extract_docs([{attribute, Anno, callback, {{CB, A}, [Fun]}}|T],{Doc, Meta}, Cwd) when Doc =/= undefined ->

    %% io:format("Converting ~p/~p~n",[CB,A]),

    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, CB, A) of
            undefined ->
                Args = fun_to_varargs(Fun),
                maybe
                    true ?= lists:all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, Args),
                    {extract_slogan_from_args(CB, Args), Doc}
                else
                    _ -> {io_lib:format("~p/~p",[CB,A]), Doc}
                end;
            SloganDocs ->
                SloganDocs
        end,

    [{{callback, CB, A}, Anno, [unicode:characters_to_binary(Slogan)],
      #{ <<"en">> => unicode:characters_to_binary(string:trim(DocsWithoutSlogan)) }, Meta} | extract_docs(T, {undefined, #{}}, Cwd)];
extract_docs([_H|T], Doc, Cwd) ->
    %% [io:format("Skipping: ~p ~p~n",[{element(3,_H),element(4,_H)}, Doc]) || element(1,_H) =:= function],
    extract_docs(T, Doc, Cwd);
extract_docs([], {undefined, _}, _Cwd) ->
    [].

prefix(function) -> "";
prefix(type) -> "t:";
prefix(callback) -> "c:".

fun_to_varargs({type, _, bounded_fun, [T|_]}) ->
    fun_to_varargs(T);
fun_to_varargs({type, _, 'fun', [{type,_,product,Args}|_] }) ->
    [fun_to_varargs(Arg) || Arg <- Args];
fun_to_varargs({ann_type, _, [Name|_]}) ->
    Name;
fun_to_varargs({var,_,_} = Name) ->
    Name;
fun_to_varargs(Else) ->
    Else.

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
    io_lib:format("~p(~ts)",[F, lists:join(", ",[string:trim(atom_to_list(Arg),leading,"_") || {var, _, Arg} <- Args])]).

expand_anno(AST) ->
    {NewAST, _} =
        lists:mapfoldl(fun F({attribute, _, file, {NewFile, _}} = E, File) when NewFile =/= File ->
                               F(E, NewFile);
                           F(E, File) ->
                               {setelement(2, E, erl_anno:set_file(File, element(2, E))), File}
                       end, undefined, AST),
    %% io:format("NewAST: ~p~n",[NewAST]),
    NewAST.
