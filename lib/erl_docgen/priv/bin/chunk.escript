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

    State#state{tags=[list_to_atom(LocalName) |T],
                dom=[{list_to_atom(LocalName), 
                      lists:reverse(A),
                      []
                     } | D]};
build_dom({endElement, _Uri, LocalName, _QName}, 
	  #state{tags=[_ |T],
                 dom=[{CName, CAttributes, CContent}, 
                      {PName, PAttributes, PContent} = _Parent | D]} = State) ->
    case list_to_atom(LocalName) of
	CName ->
            State#state{tags=T,
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
    State#state{dom=[{Name, Attributes, [unicode:characters_to_binary(String, utf8)|Content]} | D]};

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
            transform(Dom,[],#{});
        Else ->
            {error,Else}
    end.

docs(OTPXml)->
    case catch xmerl_sax_parser:file(OTPXml,
                               [skip_external_dtd,
                                {event_fun,fun event/3},
                                {event_state,initial_state()}]) of
        {ok,Tree,_} ->
            Dom = get_dom(Tree),
            NewDom = transform(Dom,[],#{}),
            to_chunk(NewDom);
        Else ->
            {error,Else}
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
    Content2 = [{dt,A2,C2}||{tag,A2,C2} <- Content],
    Content3 = [{dd,A3,C3}||{item,A3,C3} <- Content2],
    {dl,Attr,Content3}.

%% if we have {func,[],[{name,...},{name,....},...]}
%% we convert it to one {func,[],[{name,...}] per arity lowest first.    
transform_funcs([Func|T],Acc,Meta) ->
    {Functions,Meta2} = func2func(Func,Meta),
    transform_funcs(T,Functions ++ Acc, Meta2);
transform_funcs([],Acc,Meta) ->
    {lists:reverse(Acc),Meta}.

func2func({func,_,Contents},Meta) ->
    NameAttrList = 
        [NameAttr || {name,NameAttr,[]} <- Contents], % FIXME will only work for new style
    NewContents = [NC||{Tag,_,_} = NC <- Contents, Tag /= name],
    Functions = [{function,FAttr,NewContents} || FAttr <- NameAttrList],
    transform(Functions,[],Meta).


func_to_tuple("erlang:" ++ Chars) ->
    func_to_tuple(Chars);
func_to_tuple(Chars) ->
    [S|_] = string:tokens(Chars,[$)]),
    [Name|Args] = string:tokens(S,[$(,$,]),
    Arity = integer_to_list(length(Args)),
    {Name,Arity}.


transform_seealso(S = {seealso,_Attr,_Content}) ->
    dbg("~p~n",[S]).
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
    Mdoc = [M ||{Tag,_,_} = M <- Mcontent, Tag =/= functions, Tag =/= modulesummary],
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
