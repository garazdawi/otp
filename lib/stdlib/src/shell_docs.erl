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
-module(shell_docs).

-include("eep48.hrl").

-export([render/2, render/3, render/4]).
-export([get_doc/1,get_doc/3]).

get_doc(Module) ->
    {ok, #docs_v1{ module_doc = #{ <<"en">> := Docs } } } = code:get_doc(Module),
    binary_to_term(Docs).

get_doc(Module, Function, Arity) ->
    {ok, #docs_v1{ docs = Docs } } = code:get_doc(Module),
    FnFunctions =
        lists:filter(fun({{function, F, A},_Anno,_Sig,_Doc,_Meta}) ->
                             F =:= atom_to_list(Function) andalso A =:= Arity
                     end, Docs),
    [{F,A,S,maps:map(fun(_,BD) -> binary_to_term(BD) end, D),M} || {F,A,S,D,M} <- FnFunctions].

render(Module, #docs_v1{ module_doc = #{<<"en">> := DocContents} }) ->
    render_docs(atom_to_list(Module), DocContents).

render(_Module, Function, #docs_v1{ docs = Docs }) ->
    render_function(
        lists:filter(fun({{function, F, _},_Anno,_Sig,_Doc,_Meta}) ->
                             F =:= atom_to_list(Function)
                     end, Docs)).
render(_Module, Function, Arity, #docs_v1{ docs = Docs }) ->
    render_function(
      lists:filter(fun({{function, F, A},_Anno,_Sig,_Doc,_Meta}) ->
                           F =:= atom_to_list(Function) andalso A =:= Arity
                   end, Docs)).

render_function([]) ->
    {error,doc_missing};
render_function(FDocs) ->
    [render_docs(Sig, Doc) || {_,_Anno,Sig,#{ <<"en">> := Doc },_Meta} <- lists:sort(FDocs)],
    ok.

render_docs(Header, DocContents) ->
    {Doc,_} = render_docs(binary_to_term(DocContents),[],0),
    io:format("~n\t\033[;1m~ts\033[0m~ts~n",[Header, Doc]),
    ok.

render_docs(Elems,State,Pos) when is_list(Elems) ->
    lists:mapfoldl(fun(Elem,P) ->
                           io:format("Elem: ~p (~p) (~p)~n",[Elem,State,P]),
                           render_docs(Elem,State,P)
                   end,Pos,Elems);
render_docs(Elem,State,Pos) ->
    render_element(Elem,State,Pos).

render_element({p,_,Content},[],_Pos) ->
    %% Add new lines if <p> at top level
    {Docs, NewPos} = render_docs(Content, [p], 0),
    {["\r\n\r\n", Docs], NewPos};
render_element({p,_,Content},State,Pos) ->
    render_docs(Content, [p|State], Pos);
render_element({c,_,Content},State, Pos) ->
    {Docs, NewPos} = render_docs(Content, [c|State], Pos),
    {["\033[;;4m",Docs,"\033[0m"], NewPos};
render_element({em,_,Content},State, Pos) ->
    {Docs, NewPos} = render_docs(Content, State, Pos),
    {["\033[;1m",Docs,"\033[0m"], NewPos};
render_element({pre,_,Content},State,_Pos) ->
    %% For pre we start with two new lines and make sure to respect the newlines
    {Docs, NewPos} = render_docs(Content, [pre|State], 0),
    {["\r\n\r\n", Docs], NewPos};
render_element({ul,_,Content},State,_Pos) ->
    render_docs(Content, [l|State], 0);
render_element({ol,_,Content},State,_Pos) ->
    %% For now ul and ol does the same thing
    render_docs(Content, [l|State], 0);
render_element({li,_,Content},[l | _] = State,_Pos) ->
    Bullet = <<" • "/utf8>>,
    {Docs, NewPos} = render_docs(Content, [li | State], string:length(Bullet)),
    {["\r\n",Bullet,Docs],NewPos};
render_element({dl,_,Content},State,_Pos) ->
    render_docs(Content, [dl|State], 0);
render_element({dt,_,Content},[dl | _] = State,_Pos) ->
    {Docs, NewPos} = render_docs(Content, [li | State], 2),
    {["\r\n","  \033[;1m",Docs,":\033[0m"],NewPos};
render_element({dd,_,Content},[dl | _] = State,_Pos) ->
    {Docs, NewPos} = render_docs(Content, [li | State], 4),
    {["\r\n","    ",Docs],NewPos};
render_element(B, State, Pos) when is_binary(B) ->
    case State of
        [pre|_] ->
            {B, 0};
        [c|_] ->
            do_render_words([B],Pos,State,[]);
        _ ->
            do_render_words(split_to_words(B),Pos,State,[])
    end;
render_element({a,_,Content},State, Pos) ->
    %% Ignore links
    render_docs(Content, State, Pos);
render_element({Tag,Attr,Content}, State, Pos) ->
    io:format("Unhandled:{~p, ~p, Content}~n",[Tag,Attr]),
    render_docs(Content, State, Pos).

do_render_words([Word|T],Pos,State,Acc) ->
    NewPos = Pos + string:length(Word),
    do_render_words2(T,NewPos,State,[Word|Acc]);
do_render_words([],Pos,_State,Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), Pos}.

do_render_words2([Word|T],Pos,State,Acc) ->
    WordLength = string:length(Word),
    NewPos = WordLength + Pos,
    if
        NewPos > 60 ->
            %% Word does not fit , time to add a newline
            do_render_words2(T,WordLength,State,[Word,"\n"|Acc]);
         true ->
            %% Word does fit on line
            {Acc1,Pos1} = maybe(" ",Pos,State,Acc),
            do_render_words2(T,Pos1+WordLength,State,[Word|Acc1])
    end;
do_render_words2([],Pos,_State,Acc) ->
    {iolist_to_binary(lists:reverse(Acc)), Pos}.

maybe(_Chars,0,_State,Acc) ->
    {Acc,0};
maybe(" ",_Pos,[c|_],Acc) ->
    {Acc,0};
maybe(" ",Pos,_State,Acc) ->
    {[$\s|Acc],Pos+1};
maybe(Chars = "\n",_Pos,_State,Acc) ->
    {[Chars|Acc],0};
maybe(Chars = "\n\n",_Pos,_State,Acc) ->
    {[Chars|Acc],0}.

split_to_words(B) ->
    Wl1 = binary:split(B,
                       [<<" ">>,
                        <<"\r\n">>,
                        <<"\n">>],[global]),
    split_to_words1(Wl1,[]).

split_to_words1([<<>>|T],Acc) ->
    split_to_words2(T,[<<>>|Acc]);
split_to_words1(L,Acc) ->
    split_to_words2(L,Acc).

split_to_words2([<<>>],Acc) ->
    split_to_words2([],[<<>>|Acc]);
split_to_words2([<<>>|T],Acc) ->
    split_to_words2(T,Acc);
split_to_words2([H|T],Acc) ->
    split_to_words2(T,[H|Acc]);
split_to_words2([],Acc) ->
    lists:reverse(Acc).
