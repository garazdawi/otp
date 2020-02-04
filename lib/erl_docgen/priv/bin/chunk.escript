#!/usr/bin/env escript
%% -*- erlang -*-
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2020. All Rights Reserved.
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
%%----------------------------------------------------------------------
%% File    : chunk.escript
%%
%% Created : 1 Nov 2018 by Kenneth Lundin <uabkeld@elxa31hr002>
%%
%% Does translation of Erlang XML docs to EEP-48 doc chunks.
%%----------------------------------------------------------------------

-mode(compile).

main([FromXML, ToChunk]) ->
    case docs(FromXML) of
        {error, Reason} ->
            io:format("Failed to create chunks: ~p~n",[Reason]),
            erlang:halt(1);
        {docs_v1,_,_,_,_,#{ source := S },[]} when
              S =/= "../xml/gen_fsm.xml",
              S =/= "../xml/shell_default.xml",
              S =/= "../xml/user.xml",
              S =/= "../xml/erlang_mode.xml",
              S =/= "../xml/wxClipboardTextEvent.xml",
              S =/= "../xml/wxDisplayChangedEvent.xml",
              S =/= "../xml/wxGBSizerItem.xml",
              S =/= "../xml/wxGraphicsBrush.xml",
              S =/= "../xml/wxGraphicsFont.xml",
              S =/= "../xml/wxGraphicsPen.xml",
              S =/= "../xml/wxInitDialogEvent.xml",
              S =/= "../xml/wxMaximizeEvent.xml",
              S =/= "../xml/wxMouseCaptureLostEvent.xml",
              S =/= "../xml/wxPaintEvent.xml",
              S =/= "../xml/wxPreviewCanvas.xml",
              S =/= "../xml/wxSysColourChangedEvent.xml",
              S =/= "../xml/wxTaskBarIconEvent.xml",
              S =/= "../xml/wxWindowCreateEvent.xml",
              S =/= "../xml/wxWindowDestroyEvent.xml",
              S =/= "../xml/wxDataObject.xml"
              ->
            io:format("Failed to create chunks: no functions found ~s~n",[S]),
            erlang:halt(1),
            ok;
        Docs ->
            ok = file:write_file(ToChunk, term_to_binary(Docs,[compressed]))
    end;
main(_) ->
    refman(ok),
    file(ok),
    func_to_tuple(ok).

%% dbg(Fmt,Args) ->
%%     io:format(Fmt,Args);
dbg(_,_) ->
    ok.


%% Error handling
%%----------------------------------------------------------------------

-define(error(Reason), 
	throw({dom_error, Reason})).

%%----------------------------------------------------------------------

%%======================================================================
%% Records
%%======================================================================

%%----------------------------------------------------------------------
%% State record for the validator
%%----------------------------------------------------------------------
-record(state, {
          normalize=true,  %% Should we normalize characters
	  tags=[],         %% Tag stack
	  cno=[],          %% Current node number
	  namespaces = [], %% NameSpace stack
	  dom=[]           %% DOM structure
	 }).

%%======================================================================
%% External functions
%%======================================================================

%%----------------------------------------------------------------------
%% Function: initial_state() -> Result
%% Parameters: 
%% Result: 
%% Description:
%%----------------------------------------------------------------------
initial_state() ->
    #state{}.

%%----------------------------------------------------------------------
%% Function: get_dom(State) -> Result
%% Parameters: 
%% Result: 
%% Description:
%%----------------------------------------------------------------------
get_dom(#state{dom=Dom}) ->
    Dom.

%%----------------------------------------------------------------------
%% Function: event(Event, LineNo, State) -> Result
%% Parameters: 
%% Result: 
%% Description:
%%----------------------------------------------------------------------
event(Event, _LineNo, State) ->
    build_dom(Event, State).


%%======================================================================
%% Internal functions
%%======================================================================

%%----------------------------------------------------------------------
%% Function  : build_dom(Event, State) -> Result
%% Parameters: Event = term()
%%             State = #xmerl_sax_simple_dom_state{}
%% Result    : #xmerl_sax_simple_dom_state{} |
%% Description: 
%%----------------------------------------------------------------------

%% Document
%%----------------------------------------------------------------------
build_dom(startDocument, State) ->
    State#state{dom=[startDocument]};
build_dom(endDocument, 
	  #state{dom=[{Tag, Attributes, Content} |D]} = State) ->
    case D of
	[startDocument] ->
	    State#state{dom=[{Tag, Attributes, 
                              lists:reverse(Content)}]};
	[Decl, startDocument] ->
	    State#state{dom=[Decl, {Tag, Attributes, 
                                    lists:reverse(Content)}]};
	_ ->
            %% endDocument is also sent by the parser when a fault occur to tell 
            %% the event receiver that no more input will be sent
	    State
    end;

%% Element
%%----------------------------------------------------------------------
build_dom({startElement, _Uri, LocalName, _QName, Attributes}, 
	  #state{tags=T, dom=D} = State) ->

    A = parse_attributes(LocalName, Attributes),
    CName = list_to_atom(LocalName),

    Normalize =
        if CName =:= pre; CName =:= code ->
                %% Assert that we don't nest pre/code tags
                true = State#state.normalize,
                false;
           CName =:= c ->
                true = State#state.normalize,
                trim;
           true -> State#state.normalize
        end,

    State#state{normalize = Normalize,
                tags=[CName |T],
                dom=[{CName,
                      lists:reverse(A),
                      []
                     } | D]};
build_dom({endElement, _Uri, LocalName, _QName}, 
	  #state{tags=[_ |T],
                 dom=[{CName, CAttributes, CContent}, 
                      {PName, PAttributes, PContent} = _Parent | D]} = State) ->
    case list_to_atom(LocalName) of
	CName ->
            Normalize =
                if CName =:= pre; CName =:= code ->
                        %% Assert that we don't nest pre/code tags
                        false = State#state.normalize,
                        true;
                   CName =:= c ->
                        trim = State#state.normalize,
                        true;
                   true -> State#state.normalize
                end,
            State#state{normalize = Normalize,
                        tags=T,
                        dom=[{PName, PAttributes, 
                              [{CName, CAttributes, 
                                lists:reverse(CContent)}
                               |PContent]
                             } | D]};
        _ ->
            ?error("Got end of element: " ++ LocalName ++ " but expected: " ++ 
                       CName)
    end;

%% Text 
%%----------------------------------------------------------------------
build_dom({characters, String},
	  #state{dom=[{Name, Attributes, Content}| D]} = State) ->
    NewContent =
        if
            State#state.normalize =/= false ->
                normalize(State#state.normalize, unicode:characters_to_binary(String, utf8), Content);
           State#state.normalize =:= false ->
                [unicode:characters_to_binary(String, utf8) | Content]
        end,
    State#state{dom=[{Name, Attributes, NewContent} | D]};

%% Default
%%----------------------------------------------------------------------
build_dom(_E, State) ->
    State. 

%%----------------------------------------------------------------------
%% Function  : parse_attributes(ElName, Attributes) -> Result
%% Parameters: 
%% Result    : 
%% Description: 
%%----------------------------------------------------------------------
parse_attributes(ElName, Attributes) ->
    parse_attributes(ElName, Attributes, 1, []).

parse_attributes(_, [], _, Acc) ->
    Acc;
parse_attributes(ElName, [{_Uri, _Prefix, LocalName, AttrValue} |As], N, Acc) ->  
    parse_attributes(ElName, As, N+1, [{list_to_atom(LocalName), AttrValue} |Acc]).

normalize(Trim, B, [Tuple | _] = T) when is_tuple(Tuple) ->
    [normalize(Trim, B) | T];
normalize(Trim, B1,[B2|T]) ->
    [normalize(Trim, [B1," ",B2]) | T];
normalize(Trim, B,[]) ->
    [normalize(Trim, B)].
normalize(trim, CharData) ->
    string:trim(normalize(CharData));
normalize(_, CharData) ->
    normalize(CharData).
normalize(CharData) ->
    re:replace(CharData,"\\s+"," ",[unicode,global,{return,binary}]).

refman(RefMan) ->
    case catch xmerl_sax_parser:file(RefMan,
                               [skip_external_dtd,
                                {event_fun,fun event/3},
                                {event_state,initial_state()}]) of
        {ok,Tree,_} ->
            get_dom(Tree);
        Else ->
            {error,Else}
    end.
    

file(OTPXml)->
    case catch xmerl_sax_parser:file(OTPXml,
                               [skip_external_dtd,
                                {event_fun,fun event/3},
                                {event_state,initial_state()}]) of
        {ok,Tree,_} ->
            Dom = get_dom(Tree),
            transform(Dom,[],#{ source => OTPXml });
        Else ->
            {error,Else}
    end.

docs(OTPXml)->
    try xmerl_sax_parser:file(OTPXml,
                               [skip_external_dtd,
                                {event_fun,fun event/3},
                                {event_state,initial_state()}]) of
        {ok,Tree,_} ->
            Dom = get_dom(Tree),
            NewDom = transform(Dom,[],#{ source => OTPXml }),
            to_chunk(NewDom);
        Else ->
            {error,Else}
    catch E:R:ST ->
            {error,{E,R,ST}}
    end.



%% skip <erlref> but transform and keep its content            
transform([{erlref,_Attr,Content}|T],Acc,Meta) ->
    Module = [Mod || Mod = {module,_,_} <- Content], 
    NewContent = Content -- Module,
    [{module,Since,[Mname]}] = Module,
    SinceMeta = case Since of
                    [{since,S}] ->
                        Meta#{since=>S};
                    [] ->
                        Meta
                end,
    transform([{module,[{name,Mname}],NewContent}|T],Acc,SinceMeta);

%% skip <header> and all of its content
transform([{header,_Attr,_Content}|T],Acc,Meta) ->
    transform(T,Acc,Meta);
%% transform([{module,[{since,[]}],Content}|T],Acc,Meta) ->
%%     {Dom1,Meta2} = transform(Content,Acc,Meta),
%%     transform(T,[Dom1|Acc],Meta2);
%% transform([{module,[{since,Since}],[Mname]}|T],Acc,Meta) ->
%%     transform(T,[{module,[{name,Mname}],[]}|Acc],Meta#{since=>Since});

%% transform <list><item> to <ul><li> or <ol><li> depending on type attribute 
transform([{list,Attr,Content}|T],Acc,Meta) ->
    Dom = transform_list(Attr,Content),
    transform([Dom|T],Acc,Meta);

%% transform <taglist>(tag,item+)+ to <dl>(dt,item+)+
transform([{taglist,Attr,Content}|T],Acc,Meta) ->
    Dom = transform_taglist(Attr,Content),
    transform([Dom|T],Acc,Meta);

%% transform <c><anno>text</anno></c> to <anno>text</anno>
transform([{c,[],[{anno,[],AnnoContent}]}|T],Acc,Meta) ->
    transform(T,[{a,[{type,anno}],AnnoContent}|Acc],Meta);

%% transform <funcs> with <func> as children
transform([{funcs,_Attr,Content}|T],Acc,Meta) ->
    {Dom,Meta} = transform_funcs(Content, [], Meta),
    transform(T,[{functions,[],Dom}|Acc],Meta);
%% transform <datatypes> with <datatype> as children
transform([{datatypes,_Attr,Content}|T],Acc,Meta) ->
    {Dom,Meta} = transform_datatypes(Content, [], Meta),
    transform(T,[{datatypes,[],Dom}|Acc],Meta);
%% transform <fsummary> to <p>
transform([{fsummary,Attr,Content}|T],Acc,Meta) ->
    {Content2,Meta2} = transform(Content,[],Meta),
    transform(T,[{p,Attr,Content2}|Acc],Meta2);
%% transform <desc>Content</desc> to Content
transform([{desc,_Attr,Content}|T],Acc,Meta) ->
    {Content2,Meta2} = transform(Content,[],Meta),
    transform(T,[Content2|Acc],Meta2);
%% transform <marker id="name"/>  to <a id="name"/>....
transform([{br,Attr,Content}|T],Acc,Meta) ->
    {Content2,Meta2} = transform(Content,[],Meta),
    transform(T,[{br,Attr,Content2}|Acc],Meta2);
transform([{c,Attr,Content}|T],Acc,Meta) ->
    {Content2,Meta2} = transform(Content,[],Meta),
    transform(T,[{c,Attr,Content2}|Acc],Meta2);
%% transform <marker id="name"/>  to <a id="name"/>....
transform([{marker,Attr,Content}|T],Acc,Meta) ->
    {Content2,Meta2} = transform(Content,[],Meta),
    transform(T,[{a,Attr,Content2}|Acc],Meta2);
%% transform <url href="external URL"> Content</url> to <a href....
transform([{url,Attr,Content}|T],Acc,Meta) ->
    {Content2,Meta2} = transform(Content,[],Meta),
    transform(T,[{a,Attr,Content2}|Acc],Meta2);

transform([Tag = {seealso,_Attr,_Content}|T],Acc,Meta) ->
    NewTag = transform_seealso(Tag),    
    transform(T,[NewTag|Acc],Meta);
%% Tag and Attr is used as is but Content is transformed
transform([{Tag,Attr,Content}|T],Acc,Meta) ->
    dbg("Tag=~p~n",[Tag]),
    {Content2,Meta2} = transform(Content,[],Meta), 
    transform(T,[{Tag,Attr,Content2}|Acc],Meta2);
transform([Binary|T],Acc,Meta) ->
    transform(T,[Binary|Acc],Meta);
transform([],Acc,Meta) ->
    {lists:flatten(lists:reverse(Acc)),Meta}.

transform_list([{type,"ordered"}],Content) ->
    {ol,[],[{li,A2,C2}||{item,A2,C2}<-Content]};
transform_list(_,Content) ->
    {ul,[],[{li,A2,C2}||{item,A2,C2}<-Content]}.

transform_taglist(Attr,Content) ->
    Items =
        lists:map(fun({tag,A,C}) ->
                          {NewC,_} = transform(C, [], #{}),
                          {dt,A,NewC};
                     ({item,A,C}) ->
                          {NewC,_} = transform(C, [], #{}),
                          {dd,A,NewC}
                  end, Content),
    {dl,Attr,Items}.

%% if we have {func,[],[{name,...},{name,....},...]}
%% we convert it to one {func,[],[{name,...}] per arity lowest first.    
transform_funcs([Func|T],Acc,Meta) ->
    {Functions,Meta2} = func2func(Func,Meta),
    transform_funcs(T,Functions ++ Acc, Meta2);
transform_funcs([],Acc,Meta) ->
    {lists:reverse(Acc),Meta}.

func2func({func,_,Contents},Meta) ->
    dbg("F ~p~n",[Contents]),

    ContentsNoName = [NC||NC <- Contents, element(1,NC) /= name],

    case [Name || {name,_,_} = Name <- Contents] of
        [{name,_,[]}|_] = NameList ->
            %% Spec style function docs
            TagsToFA =
                fun(Tags) ->
                        {proplists:get_value(name,Tags),
                         proplists:get_value(arity,Tags)}
                end,
            Equiv = [TagsToFA(FAttr) || {name,FAttr,[]} <- NameList],
            Functions = [{function,FAttr ++ [{equiv,Equiv -- [TagsToFA(FAttr)]}],ContentsNoName} ||
                            {name,FAttr,[]} <- NameList ],
            transform(Functions,[],Meta);
        NameList ->
            %% Manual style function docs
            FFA = [{func_to_tuple(NameString),Attr} || {name, Attr, NameString} <- NameList],
            Equiv = [FA || {FA,_} <- FFA],
            Functions = [{function,[{name,Name},{arity,Arity},{equiv,Equiv -- [FA]} | Attr], ContentsNoName}
                         || {{Name,Arity} = FA,Attr} <- FFA],
            transform(Functions,[], Meta)
    end.

func_to_tuple(Chars) ->
    try
        NoMod =
            case string:split(strip_tags(Chars),":") of
                [_, Tl] ->
                    Tl;
                [StrippedChars] ->
                    StrippedChars
            end,
        [S|_] = string:lexemes(NoMod,[$)]),
        [Name|Args] = string:lexemes(S,[$(,$,]),
        Arity = integer_to_list(length(Args)),
        {unicode:characters_to_list(Name),Arity}
    catch E:R:ST ->
            io:format("Failed to parse: ~p~n",[Chars]),
            erlang:raise(E,R,ST)
    end.

strip_tags([{_Tag,_Attr,Content}|T]) ->
    [Content | strip_tags(T)];
strip_tags([H|T]) when not is_tuple(H) ->
    [H | strip_tags(T)];
strip_tags([]) ->
    [].

transform_datatypes([_DT|_T],_Acc,Meta) ->
    %% TODO: Should also export datatype docs
    {[],Meta}.


transform_seealso(S = {seealso,_Attr,_Content}) ->
    dbg("~p~n",[S]),
    _Content.
%%transform_seealso({seealso,[{marker,MarkerStr}],Content}) ->
%%    {a,[{type,seealso}],Content}.
%% render([{module,[{name,M}|_],[]}|T],Acc,Meta) ->
%%     render(T,[[<<"Module ">>,M,"\n"]|Acc],Meta);
%% render([H|T],Acc,Meta) ->
%%     render(T, [render(H)|Acc], Meta);

%% render({Tag,Attr,[H|T]}) when is_binary(H) ->
%%     H;
%% render(Bin) ->
%%     Bin.

to_chunk({Dom,Meta}) ->
    [{module,_Mattr,Mcontent}] = Dom,
%    Module = proplists:get_value(name,Mattr),
    {description, _Meta, Mdoc} = lists:keyfind(description,1,Mcontent),
    FuncEntrys =
        case lists:keyfind(functions,1,Mcontent) of
            false ->
                [];
            {_,_,Functions} ->
                [docs_v1_entry(function,Fname,Arity,maps:from_list(FMeta),Fdoc)||
                    {function,[{name,Fname},{arity,Arity}|FMeta],Fdoc} <- Functions]
        end,
    docs_v1(Mdoc, Meta, FuncEntrys).

docs_v1(DocContents, Metadata, Docs) ->
    % TODO fill these in
    Anno = 0,
    BeamLanguage = erlang,
    Format = <<"text/erlang_doc">>, % FIXME decide about format
    {docs_v1, Anno, BeamLanguage, Format, #{<<"en">> => term_to_binary(DocContents)}, Metadata, Docs}.

docs_v1_entry(Kind, Name, Arity, Metadata, DocContents) ->
    % TODO fill these in
    Anno = 0,
    % TODO get signature from abstract code
    Signature = [list_to_binary(Name ++ "/" ++ Arity)],
    {{Kind, Name, list_to_integer(Arity)}, Anno, Signature, #{ <<"en">> => term_to_binary(DocContents)}, Metadata}.
