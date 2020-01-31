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

render(Module, #docs_v1{ module_doc = #{<<"en">> := DocContents} }) ->
    do_render(atom_to_list(Module), DocContents).

render(_Module, Function, #docs_v1{ docs = Docs }) ->
    render(
        lists:filter(fun({{function, F, _},_Anno,_Sig,_Doc,_Meta}) ->
                             F =:= atom_to_list(Function)
                     end, Docs)).
render(_Module, Function, Arity, #docs_v1{ docs = Docs }) ->
    render(
      lists:filter(fun({{function, F, A},_Anno,_Sig,_Doc,_Meta}) ->
                           F =:= atom_to_list(Function) andalso A =:= Arity
                   end, Docs)).

render([]) ->
    {error,doc_missing};
render(FDocs) ->
    [do_render(Sig, Doc) || {_,_Anno,Sig,#{ <<"en">> := Doc },_Meta} <- lists:sort(FDocs)],
    ok.

do_render(Header, DocContents) ->
    {_,_,Doc} = render2(binary_to_term(DocContents),0,[],[]),
    io:format("~n\t~ts~n~n~ts~n",[Header, Doc]),
    ok.

render2([H|T],Pos,State,Acc) ->
    {NewPos,NewState,Acc2} = render2(H,Pos,State,[]),
    render2(T,NewPos,NewState,[Acc2|Acc]);
render2([],Pos,[_|T],Acc) ->
    {Pos,T,lists:reverse(Acc)};
render2([],Pos,State,Acc) ->
    {Pos,State,lists:reverse(Acc)};
render2(Element, Pos,State,Acc) ->
    do_render(Element, Pos, State,Acc).

do_render({p,_,Content},Pos,State,Acc) ->
    {_,State1,Acc1} = maybe("\n\n",Pos,[p|State],Acc),
    render2(Content, 0, State1, Acc1);
%% Assume that c only contains 1 binary which should not be split into words
do_render({c,_,Content},Pos,State,Acc) ->
    render2(Content, Pos, [c|State], Acc);
do_render(B, Pos, State = [c|_], Acc) when is_binary(B) ->
    do_render_words([B],Pos,State,Acc);
do_render(B, Pos, State, Acc) when is_binary(B) ->
    Words = split_to_words(B),
%%    Words = string:lexemes(B," " ++[[$\r,$\n],$\n]),
    do_render_words(Words,Pos,State,Acc);
do_render({_Tag,_Attr,Content}, Pos, State, Acc) ->
%    io:format("Unhandled:{~p, ~p, Content}~n",[Tag,Attr]),
    render2(Content, Pos, State, Acc).

do_render_words([Word|T],Pos,State,Acc) ->
    NewPos = Pos + string:length(Word),
    do_render_words2(T,NewPos,State,[Word|Acc]);
do_render_words([],Pos,State,Acc) ->
    {Pos,State,iolist_to_binary(lists:reverse(Acc))}.

do_render_words2([Word|T],Pos,State,Acc) ->
    WordLength = string:length(Word),
    NewPos = WordLength + Pos,
    if 
        NewPos > 60 ->
            %% Word does not fit , time to add a newline 
            do_render_words2(T,WordLength,State,[Word,"\n"|Acc]);
         true ->
            %% Word does fit on line
            {Pos1,State1,Acc1} = maybe(" ",Pos,State,Acc),
            do_render_words2(T,Pos1+WordLength,State1,[Word|Acc1])
    end;
do_render_words2([],Pos,State,Acc) ->
    {Pos,State,iolist_to_binary(lists:reverse(Acc))}.

maybe(_Chars,0,State,Acc) ->
    {0,State,Acc};
maybe(" ",_Pos,State=[c|_],Acc) ->
    {0,State,Acc};
maybe(" ",Pos,State,Acc) ->
    {Pos+1,State,[$\s|Acc]};
maybe(Chars = "\n",_Pos,State,Acc) ->
    {0,State,[Chars|Acc]};
maybe(Chars = "\n\n",_Pos,State,Acc) ->
    {0,State,[Chars|Acc]}.
    
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
