-module(beam_doc).

-feature(maybe_expr, enable).

-export([main/2]).

-include_lib("kernel/include/eep48.hrl").


-record(docs, {cwd                 :: unicode:chardata(),             % Cwd
               module = undefined  :: unicode:chardata() | undefined,
               doc    = undefined  :: unicode:chardata(), % Function/type/callback local doc
               meta   = maps:new() :: maps:map()}).      % Function/type/callback local meta
-type internal_docs() :: #docs{}.


-spec main(term(), term()) -> term().
main(Dirname, AST) ->
    try
        io:format("~p", [AST]),
        {value, {attribute, ModuleDocAnno, moduledoc, ModuleDoc}} = lists:keysearch(moduledoc, 3, AST),
        DocFormat = extract_docformat(AST),
        Docs = extract_documentation(AST, reset_state(#docs{cwd = Dirname})),
        #docs_v1{
           format = DocFormat,
           anno = ModuleDocAnno,
           module_doc = #{ <<"en">> => unicode:characters_to_binary(ModuleDoc) }, % TODO
           docs = Docs }
    catch E:R:ST ->
            erlang:raise(E, R, ST)
    end.

%% State Module:
%% moduledoc format
%% moduledoc file
%% CWD

extract_docformat(AST) ->
    case lists:keysearch(docformat, 3, AST) of
        false -> <<"text/markdown">>;
        {value, {attribute, _DocAnno, docformat, DocFormat}} ->
            unicode:characters_to_binary(DocFormat)
    end.

-spec reset_state(State :: internal_docs()) -> internal_docs().
reset_state(State) ->
    State#docs{doc = undefined, meta = maps:new()}.

update_meta(#docs{meta = Meta0}=State, Meta1) ->
    State#docs{meta = maps:merge(Meta0, Meta1)}.

update_doc(#docs{}=State, Doc) ->
    State#docs{doc = string:trim(Doc)}.

update_module(#docs{}=State, ModuleName) ->
    State#docs{module = ModuleName}.

extract_documentation([{attribute, _Anno, file, {ModuleName, _A}} | T], State) ->
    extract_documentation(T, update_module(State, ModuleName));
extract_documentation([{attribute, _Anno, doc, Meta0} | T], State) when is_map(Meta0) ->
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
extract_documentation([{function, Anno, F, A, [{clause, _, ClauseArgs, _, _}]} | T],
                      #docs{doc = Doc}=State) when Doc =/= undefined ->
    FunDoc = template_gen_doc({function, Anno, F, A, ClauseArgs}, State),
    [FunDoc | extract_documentation(T, reset_state(State))];
extract_documentation([{function, Anno, F, A, _Body} | T],
                      #docs{doc = Doc}=State) when Doc =/= undefined ->
    {Slogan, DocsWithoutSlogan} =
        %% First we check if there is a doc prototype
        case extract_slogan(Doc, F, A) of
            undefined -> {io_lib:format("~p/~p",[F,A]), Doc};
            SloganDocs -> SloganDocs
        end,
    AttrBody = {function, F, A},
    FunDoc = gen_doc(Anno, AttrBody, Slogan, DocsWithoutSlogan, State),
    [FunDoc | extract_documentation(T, reset_state(State))];


extract_documentation([{attribute, Anno, TypeOrOpaque, {Type, _, TypeArgs}} | T], #docs{doc = Doc}=State)
  when Doc =/= undefined, TypeOrOpaque =:= type orelse TypeOrOpaque =:= opaque ->
    Args = fun_to_varargs(TypeArgs),
    FunDoc = template_gen_doc({type, Anno, Type, length(Args), Args}, State),
    [FunDoc | extract_documentation(T, reset_state(State))];
extract_documentation([{attribute, Anno, callback, {{CB, A}, [Fun]}} | T],#docs{doc = Doc}=State)
  when Doc =/= undefined ->
    Args = fun_to_varargs(Fun),
    FunDoc = template_gen_doc({callback, Anno, CB, A, Args}, State),
    [FunDoc | extract_documentation(T, reset_state(State))];

extract_documentation([_H|T], State) ->
    extract_documentation(T, State);
extract_documentation([], #docs{doc = undefined}) ->
    [].

gen_doc(Anno0, AttrBody, Slogan, Docs, #docs{meta = Meta, module = Module}) ->
    %% DONE: missing expand anno
    %% TODO: <<"en">> should be read from meta info, and use as default
    Anno1 = erl_anno:set_file(Module, Anno0),
    {AttrBody, Anno1, [unicode:characters_to_binary(Slogan)],
      #{ <<"en">> => unicode:characters_to_binary(string:trim(Docs)) }, Meta}.

template_gen_doc({Attr, Anno, F, A, Args}, #docs{doc = Doc}=State) ->
    {Slogan, DocsWithoutSlogan} =
        case extract_slogan(Doc, F, A) of
            undefined ->
                maybe
                    true ?= lists:all(fun({var,_,N}) when N =/= '_' -> true; (_) -> false end, Args),
                    {extract_slogan_from_args(F, Args), Doc}
                else
                    _E2 ->
                        {io_lib:format("~p/~p",[F,A]), Doc}
                end;
            SloganDocs ->
                SloganDocs
        end,
    AttrBody = {Attr, F, A},
    gen_doc(Anno, AttrBody, Slogan, DocsWithoutSlogan, State).

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
