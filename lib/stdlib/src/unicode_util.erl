%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2017-2026. All Rights Reserved.
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

-module(unicode_util).
-moduledoc false.
-export([gc_1/2]).
-export([nfd/1, nfc/1, nfkd/1, nfkc/1]).
-export([pattern_whitespace/0, is_whitespace/1]).
-export([uppercase/1, lowercase/1, titlecase/1, casefold/1]).

-export([spec_version/0, lookup/1, category/1, get_case/1]).
-export([is_wide/1]).
-export([is_other_id_start/1, is_other_id_continue/1, is_letter_not_pattern_syntax/1]).
-compile({inline, [class/1]}).
-compile(nowarn_unused_vars).
-import(string, [gc/1, cp/1]).
-dialyzer({no_improper_lists, [gc_prepend/2]}).
-type gc() :: char()|[char()].
-type category() ::
     {letter,uppercase} |
     {letter,lowercase} |
     {letter,titlecase} |
     {mark,non_spacing} |
     {mark,spacing_combining} |
     {mark,enclosing} |
     {number,decimal} |
     {number,letter} |
     {number,other} |
     {separator,space} |
     {separator,line} |
     {separator,paragraph} |
     {other,control} |
     {other,format} |
     {other,surrogate} |
     {other,private} |
     {other,not_assigned} |
     {letter,modifier} |
     {letter,other} |
     {punctuation,connector} |
     {punctuation,dash} |
     {punctuation,open} |
     {punctuation,close} |
     {punctuation,initial} | % Punctuation, Initial quote (may behave like open or close depending on usage)
     {punctuation,final} |   % Punctuation, Final quote (may behave like open or close depending on usage)
     {punctuation,other} |
     {symbol,math} |
     {symbol,currency} |
     {symbol,modifier} |
     {symbol,other}.

-include("unicode_util.hrl").

-spec lookup(char()) ->
     #{'canon':= [{byte(),char()}],
       'ccc':= byte(),
       'compat':= [] | {atom(),[{byte(),char()}]},
       'category':= category()}.
lookup(Codepoint) when ?IS_CP(Codepoint) ->
    {CCC,Can,Comp,Cat} = unicode_table(Codepoint),
    #{ccc=>CCC, canon=>Can, compat=>Comp, category=>category(Cat,Codepoint)}.

-spec category(char()) -> category().
category(Codepoint) when ?IS_CP(Codepoint) ->
    {_,_,_,Cat} = unicode_table(Codepoint),
    category(Cat,Codepoint).

-spec get_case(char()) -> #{'fold':=gc(), 'lower':=gc(), 'title':=gc(), 'upper':=gc()}.
get_case(Codepoint) when ?IS_CP(Codepoint) ->
    case case_table(Codepoint) of
        {U,L} -> #{upper=>U,lower=>L,title=>U,fold=>L};
        {U,L,T,F} -> #{upper=>U,lower=>L,title=>T,fold=>F}
    end.

spec_version() -> {17,0}.


class(Codepoint) when ?IS_CP(Codepoint) -> 
    {CCC,_,_,_} = unicode_table(Codepoint),
    CCC.

-spec uppercase(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).
uppercase(Str0) ->
    case cp(Str0) of
        [CP|Str] = Str1 ->
            case case_table(CP) of
                {Upper,_} -> [Upper|Str];
                {Upper,_,_,_} -> [Upper|Str]
            end;
        [] -> [];
        {error,Err} -> error({badarg, Err})
    end.

-spec lowercase(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).
lowercase(Str0) ->
    case cp(Str0) of
        [CP|Str] = Str1 ->
            case case_table(CP) of
                {_,Lower} -> [Lower|Str];
                {_,Lower,_,_} -> [Lower|Str]
            end;
        [] -> [];
        {error,Err} -> error({badarg, Err})
    end.

-spec titlecase(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).
titlecase(Str0) ->
    case cp(Str0) of
        [CP|Str] = Str1 ->
            case case_table(CP) of
                {_,_,Title,_} -> [Title|Str];
                {Upper,_} -> [Upper|Str]
            end;
        [] -> [];
        {error,Err} -> error({badarg, Err})
    end.

-spec casefold(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()).
casefold(Str0) ->
    case cp(Str0) of
        [CP|Str] = Str1 ->
            case case_table(CP) of
                {_,_,_,Fold} -> [Fold|Str];
                {_,Lower} -> [Lower|Str]
            end;
        [] -> [];
        {error,Err} -> error({badarg, Err})
    end.

%% Returns true if the character is considered wide in non east asian context.
-spec is_wide(gc()) -> boolean().
is_wide(C) when ?IS_ASCII(C) ->
    false;
is_wide(C) when ?IS_CP(C) ->
    is_wide_cp(C);
is_wide([_, 16#FE0E|Cs]) -> true; %% Presentation sequence
is_wide([_, 16#FE0F|Cs]) -> true; %% Presentation sequence
is_wide([C|Cs]) when ?IS_CP(C) ->
    is_wide_cp(C) orelse is_wide(Cs);
is_wide([]) ->
    false.

category(lookup_category, Cp) ->
    lookup_category(Cp);
category(Def, _) -> Def.

-spec nfd(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()) | {error, unicode:chardata()}.
nfd(Str0) ->
    case gc(Str0) of
        [GC|R] when ?IS_ASCII(GC) -> [GC|R];
        [GC|Str] -> [decompose(GC)|Str];
        [] -> [];
        {error,_}=Error -> Error
    end.

-spec nfkd(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()) | {error, unicode:chardata()}.
nfkd(Str0) ->
    case gc(Str0) of
        [GC|R] when ?IS_ASCII(GC) -> [GC|R];
        [GC|Str] -> [decompose_compat(GC)|Str];
        [] -> [];
        {error,_}=Error -> Error
    end.

-spec nfc(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()) | {error, unicode:chardata()}.
nfc(Str0) ->
    case gc(Str0) of
        [GC|R] when ?IS_LATIN1(GC) -> [GC|R];
        [GC|Str] -> [compose(decompose(GC))|Str];
        [] -> [];
        {error,_}=Error -> Error
    end.

-spec nfkc(unicode:chardata()) -> maybe_improper_list(gc(),unicode:chardata()) | {error, unicode:chardata()}.
nfkc(Str0) ->
    case gc(Str0) of
        [GC|R] when ?IS_ASCII(GC) -> [GC|R];
        [GC|Str] -> [compose_compat_0(decompose_compat(GC))|Str];
        [] -> [];
        {error,_}=Error -> Error
    end.

decompose(CP) when is_integer(CP), not ?IS_HANGUL(CP) ->
    case unicode_table(CP) of
        {_,[],_,_} -> CP;
        {_,CPs,_,_} -> canonical_order(CPs)
    end;
decompose(CP) ->
   canonical_order(decompose_1(CP)).

decompose_1(CP) when ?IS_HANGUL(CP) ->
    Syll = CP-16#AC00,
    T = 28,
    N = 588,
    Lead = 16#1100 + Syll div N,
    Vowel = 16#1161 + (Syll rem N) div T,
    case Syll rem T of
        0 -> [{0,Lead},{0,Vowel}];
        Trail -> [{0,Lead}, {0,Vowel}, {0,Trail+16#11A7}]
    end;
decompose_1(CP) when is_integer(CP) ->
    case unicode_table(CP) of
        {CCC, [],_,_} -> [{CCC,CP}];
        {_,CPs,_,_} -> CPs
    end;
decompose_1([CP|CPs]) ->
    decompose_1(CP) ++ decompose_1(CPs);
decompose_1([]) -> [].

canonical_order([{_,CP}]) -> CP;
canonical_order(CPs) ->
    canonical_order_1(CPs).

canonical_order_1([{0,CP}|TaggedCPs]) ->
    [CP|canonical_order_1(TaggedCPs)];
canonical_order_1([_|_]=TaggedCPs) ->
    canonical_order_2(TaggedCPs, []);
canonical_order_1([]) -> [].

canonical_order_2([{CCC,_}=First|Cont], Seq) when CCC > 0 ->
    canonical_order_2(Cont, [First|Seq]);
canonical_order_2(Cont, Seq) ->
     [CP || {_, CP} <- lists:keysort(1,lists:reverse(Seq))] ++ canonical_order_1(Cont).

decompose_compat(CP) when is_integer(CP), not ?IS_HANGUL(CP) ->
    case unicode_table(CP) of
        {_, [], [], _} -> CP;
        {_, _, {_,CPs}, _} -> canonical_order(CPs);
        {_, CPs, _, _} -> canonical_order(CPs)
    end;
decompose_compat(CP) ->
   canonical_order(decompose_compat_1(CP)).

decompose_compat_1(CP) when ?IS_HANGUL(CP) ->
    Syll = CP-16#AC00,
    T = 28,
    N = 588,
    Lead = 16#1100 + Syll div N,
    Vowel = 16#1161 + (Syll rem N) div T,
    case Syll rem T of
        0 -> [{0,Lead},{0,Vowel}];
        Trail -> [{0,Lead}, {0,Vowel}, {0,Trail+16#11A7}]
    end;
decompose_compat_1(CP) when is_integer(CP) ->
    case unicode_table(CP) of
        {CCC, [], [], _} -> [{CCC,CP}];
        {_, _, {_,CPs}, _} -> CPs;
        {_, CPs, _, _} -> CPs
    end;
decompose_compat_1([CP|CPs]) ->
    decompose_compat_1(CP) ++ decompose_compat_1(CPs);
decompose_compat_1([]) -> [].

compose(CP) when is_integer(CP) -> CP;
compose([Lead,Vowel|Trail]) %% Hangul
  when is_integer(Lead, 16#1100, 16#1112), is_integer(Vowel) ->
    if 16#1161 =< Vowel, Vowel =< 16#1175 ->
            CP = 16#AC00 + ((Lead - 16#1100) * 588) + ((Vowel - 16#1161) * 28),
            case Trail of
                [T|Acc] when is_integer(T, 16#11A7, 16#11C2) ->
                     nolist(CP+T-16#11A7,Acc);
                Acc -> nolist(CP,Acc)
            end;
       true ->
            case compose([Vowel|Trail]) of
                [_|_] = CPs -> [Lead|CPs];
                CP -> [Lead,CP]
            end
    end;
compose([Base,Accent]=GC0) ->
    case compose_pair(Base,Accent) of
        false -> GC0;
        GC -> GC
    end;
compose([CP|Many]) ->
    compose_many(Many, CP, [], class(CP)).

compose_many([CP|Rest], Base, Accents, Prev) ->
    Class = class(CP),
    case (Prev =:= 0 orelse Prev < Class) andalso compose_pair(Base, CP) of
        false ->
            if Class =:= 0 ->
                  Begin = [Base|lists:reverse(Accents)],
                  case compose_many(Rest, CP, [], 0) of
                      [_|_] = GC -> Begin ++ GC;
                      Composed -> Begin ++ [Composed]
                  end;
               true ->
                  compose_many(Rest, Base, [CP|Accents], Class)
            end;
        Combined ->
            compose_many(Rest, Combined, Accents, Prev)
    end;
compose_many([], Base, [], Prev) ->
    Base;
compose_many([], Base, Accents, Prev) ->
    [Base|lists:reverse(Accents)].

compose_compat_0(CP) when is_integer(CP) ->
    CP;
compose_compat_0(L) ->
    case gc(L) of
        [First|Rest] ->
            case compose_compat(First) of
                [_|_] = GC -> GC ++ compose_compat_0(Rest);
                CP -> [CP|compose_compat_0(Rest)]
            end;
        [] -> []
    end.

compose_compat(CP) when is_integer(CP) -> CP;
compose_compat([Lead,Vowel|Trail]) %% Hangul
  when is_integer(Lead, 16#1100, 16#1112), is_integer(Vowel) ->
    if 16#1161 =< Vowel, Vowel =< 16#1175 ->
            CP = 16#AC00 + ((Lead - 16#1100) * 588) + ((Vowel - 16#1161) * 28),
            case Trail of
                [T|Acc] when is_integer(T, 16#11A7, 16#11C2) ->
                    nolist(CP+T-16#11A7,Acc);
                Acc -> nolist(CP,Acc)
            end;
       true ->
            case compose_compat([Vowel|Trail]) of
                [_|_] = CPs -> [Lead|CPs];
                CP -> [Lead,CP]
            end
    end;
compose_compat([Base,Accent]=GC0) ->
    case compose_pair(Base,Accent) of
        false -> GC0;
        GC -> GC
    end;
compose_compat([CP|Many]) ->
    compose_compat_many(Many, CP, [], class(CP)).

compose_compat_many([CP|Rest], Base, Accents, Prev) ->
    Class = class(CP),
    case (Prev =:= 0 orelse Prev < Class) andalso compose_pair(Base, CP) of
        false ->
            if Class =:= 0 ->
                  Begin = [Base|lists:reverse(Accents)],
                  case compose_compat_many(Rest, CP, [], 0) of
                      [_|_] = GC -> Begin ++ GC;
                      Composed -> Begin ++ [Composed]
                  end;
               true ->
                  compose_compat_many(Rest, Base, [CP|Accents], Class)
            end;
        Combined ->
            compose_compat_many(Rest, Combined, Accents, Prev)
    end;
compose_compat_many([], Base, [], Prev) ->
    Base;
compose_compat_many([], Base, Accents, Prev) ->
    [Base|lists:reverse(Accents)].

-include("unicode_util_gen.hrl").
