%% coding: utf-8 -*-
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2021. All Rights Reserved.
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
-module(prim_tty).

%% Todo:
%%  * Look at what windows does
%%  * Try to move buffer handling logic to Erlang
%%    * This may not be possible for performance reasons, but should be tried
%%    * It seems like unix decodes and then encodes utf8 when emitting it
%%  * user_drv module should be able to handle both nif and driver without too many changes.
%%
%% Problems to solve:
%%  * Do not use non blocking io
%%  * Reset tty settings at _exit
%%  * Allow remsh in oldshell (can we do this?)
%%  * See if we can run a tty in windows shell
%%  * Allow unicode detection for noshell/noinput
%%  * ?Allow multi-line editing?
%%    * The current implementation only allows the cursor to move and edit on current line
%%
%% Concepts to keep in mind:
%%   Code point: A single unicode "thing", examples: "a", "😀" (unicode smilie)
%%   Grapheme cluster: One or more code points, "
%%   Logical character: Any character that the user typed or printed.
%%            One unicode grapheme cluster is a logical character
%%        Examples: "a", "\t", "😀" (unicode smilie), "\x{1F600}", "\e[0m" (ansi sequences),
%%                  "^C"
%%      When we step or delete we count logical characters even if they are multiple chars.
%%        (I'm unsure how ansi should be handled with regard to delete?)
%%
%%   Actual characters: The actual unicode grapheme clusters printed
%%   Column: The number of rendered columns for a logical character
%%
%%  When navigating using move(left) and move(right) the terminal will move one
%%  actual character, so if we want to move one logical character we may have to
%%  emit more moves. The same is true when overwriting.
%%
%%  When calculating the current column position we have to use the column size
%%  of the characters as otherwise smilies will becomes incorrect.
%%
%%  In the current ttysl_drv and also this implementation there are never any newlines
%%  in the buffer.
%%
%%  Printing of unicode characters:
%%    Read this post: https://jeffquast.com/post/terminal_wcwidth_solution/
%%    Summary: the wcwidth implementation in libc is often lacking. So we should
%%        create our own. We can get the size of all unicode graphemes by rendering
%%        them on a terminal and see how much the cursor moves. We can query where the
%%        cursor is by using "\033[6n"[1]. How many valid grapheme clusters are there?
%%
%%    On consoles that does support fetching the surrent cursor position, we may get
%%    away with only using that to check where we are and where to go. And on consoles
%%    that do not we just have to make a best effort and use libc wcwidth.
%%
%%    We need to know the width of characters when:
%%      * Printing a \t
%%      * Compensating for xn
%% [1]: https://www.linuxquestions.org/questions/programming-9/get-cursor-position-in-c-947833/
%%    Notes:
%%      [129306,127996] (hand with skintone), seems to move the cursor more than it should...
%%      edlin and user needs to agree on what one "character" is, right now edlin uses
%%      code points and not grapheme clusters.
%%
%%  Windows:
%%    Since 2017:ish we can use Virtual Terminal Sequences[2] to control the terminal.
%%    It seems like these are mostly ANSI escape comparitible, so it should be possible
%%    to just use the same mechanism on windows and unix.

-export([init/1, window_size/1, unicode/1, unicode/2,
         putc_sync/2, putc/2, move/2, insert/2, delete/2,
         beep/1, on_load/0, on_load/1]).
-export_type([tty/0]).

-nifs([isatty/1, tty_init/2, tty_set/1, tty_termcap/2, setlocale/0,
       tty_select/2, tty_window_size/1, write/2, isprint/1, wcwidth/1, wcswidth/1,
       sizeof_wchar/0]).
-export([isprint/1,wcwidth/1, wcswidth/1, sizeof_wchar/0]).

-record(state, {tty, utf8, parent,
                buffer_before = [],  %% Current line before cursor in reverse
                buffer_after = [],   %% Current line after  cursor not in reverse
                width = 80,
                up = "\e[A",
                down = [10],
                left = [8],
                right = "\e[C",
                tab = "\e[1I",  %% Tab to next 8 column
                position = "\e[6n",
                position_reply = "\e[([0-9]+);([0-9]+)R",
                %% Copied from https://github.com/chalk/ansi-regex/blob/main/index.js
                ansi_regexp = <<"^[\e",194,155,"][[\\]()#;?]*(?:(?:(?:(?:;[-a-zA-Z\\d\\/#&.:=?%@~_]+)*|[a-zA-Z\\d]+(?:;[-a-zA-Z\\d\\/#&.:=?%@~_]*)*)?",7,")|(?:(?:\\d{1,4}(?:;\\d{0,4})*)?[\\dA-PR-TZcf-nq-uy=><~]))">>
               }).

-spec on_load() -> ok.

on_load() ->
    on_load(#{}).

-spec on_load(Extra) -> ok when
      Extra :: map().
on_load(Extra) ->
    ok = erlang:load_nif(atom_to_list(?MODULE), Extra).

-opaque tty() :: {pid(), reference()}.

-spec init([{canon | echo | sig, boolean()}]) -> {ok, tty()} | {error, term()}.
init(Options) ->
    ok = on_load(),
    Parent = self(),
    Ref = make_ref(),
    {Pid, _} = spawn_opt(
                    fun() -> init(Ref, Parent, Options) end,
                    [{monitor,[{tag,Ref}]}]),
    receive
        {Ref, _Ref, _, _, Reason} ->
            {error, Reason};
        {Ref, TTY} ->
            erlang:demonitor(Ref, [flush]),
            link(Pid),
            {ok, {Pid, TTY}}
    end.

window_size({_Pid, TTY}) ->
    tty_window_size(TTY).

unicode(TTY) ->
    call(TTY, unicode).

unicode(TTY, Value) ->
    call(TTY, {unicode, Value}).

putc_sync(TTY, Characters) ->
    call(TTY, {putc, Characters}),
    self() ! {TTY, ok},
    ok.

putc(TTY, Characters) ->
    cast(TTY, {putc, Characters}).

move(TTY, Steps) ->
    cast(TTY, {move, Steps}).

insert(TTY, Characters) ->
    cast(TTY, {insert, Characters}).

delete(TTY, NumCharacters) ->
    cast(TTY, {delete, NumCharacters}).

beep(TTY) ->
    cast(TTY, beep).

init(Ref, Parent, Options) ->
    dbg:tracer(
      process,
      {fun F(Event, undefined) ->
               F(Event, element(2, file:open("tty.trace",[write, raw])));
           F(Event, FD) ->
               file:write(FD, io_lib:format("~p~n",[Event])),
               file:sync(FD),
               FD
       end, undefined}
     ),
    dbg:p(self(), [c, r]),
    dbg:p(Parent, [c, r]),
    dbg:tpl(?MODULE, write, x),
    dbg:tpl(?MODULE, dbg, []),
    true = isatty(0),
    true = isatty(1),
    {ok, TTY} = tty_init(1, maps:from_list(Options)),
    ok = tty_set(TTY),
    ok = tty_termcap(TTY, os:getenv("TERM")),

    Utf8Mode =
        case setlocale() of
            primitive ->
                lists:any(
                  fun(Key) ->
                          string:find(os:getenv(Key),"UTF-8") =/= nomatch
                  end, ["LC_ALL","LC_CTYPE", "LANG"]);
            Utf8Locale when is_boolean(Utf8Locale) ->
                Utf8Locale
        end,

    ok = tty_select(TTY, 0),

    true = isprint($a),
    1 = wcwidth($a),
    4 = sizeof_wchar(),
    {ok, 1} = wcswidth(<<$a:(sizeof_wchar() * 8)/native>>),
    {ok, 2} = wcswidth(<<16#1F600:(sizeof_wchar() * 8)/native>>),
    {ok, 3} = wcswidth(<<$a:(sizeof_wchar() * 8)/native,
                         16#1F600:(sizeof_wchar() * 8)/native>>), % 😀
    {error, not_printable} = wcwidth($\n),

    Parent ! {Ref, TTY},
    try
        loop(#state{ tty = TTY, utf8 = Utf8Mode, parent = Parent })
    catch E:R:ST ->
            erlang:display({E,R,ST}),
            erlang:raise(E,R,ST)
    end.

loop(State) ->
    dbg({State#state.buffer_before,
         State#state.buffer_after}),
    receive
        {call, Ref, {unicode, true}} ->
            Ref ! {Ref, utf8},
            loop(State#state{ utf8 = true });
        {call, Ref, {unicode, false}} ->
            Ref ! {Ref, latin1},
            loop(State#state{ utf8 = false });
        {call, Ref, unicode} ->
            Ref ! {Ref, State#state.utf8 },
            loop(State);
        {call, Ref, Request} ->
            NewState = handle_request(State, Request),
            Ref ! {Ref, ok},
            loop(NewState);
        {cast, Request} ->
            loop(handle_request(State, Request));
        {input, Bytes} ->
            State#state.parent ! {{self(), State#state.tty}, {data, Bytes}},
            loop(State);
        _Unknown ->
%            erlang:display({unknown, Unknown}),
            loop(State)
    end.

handle_request(State, {putc, Binary}) ->
    %% Todo should handle invalid utf8?
    insert_buf(State, Binary);
handle_request(State, {delete, N}) when N > 0 ->
    {DelNum, _DelCols, _, NewBA} = split(N, State#state.buffer_after),
    BBCols = cols(State#state.buffer_before),
    NewBACols = cols(NewBA),
    write(State#state.tty,
          [unicode:characters_to_binary(
            [NewBA, lists:duplicate(DelNum, $\s),
             xnfix(State),
             move_cursor(State,
                         BBCols + NewBACols + DelNum,
                         BBCols)])]),
    State#state{ buffer_after = NewBA };
handle_request(State, {delete, N}) when N < 0 ->
    {DelNum, DelCols, _, NewBB} = split(-N, State#state.buffer_before),
    NewBBCols = cols(NewBB),
    BACols = cols(State#state.buffer_after),
    write(State#state.tty,
          [unicode:characters_to_binary(
            [move_cursor(State, NewBBCols + DelCols, NewBBCols),
             State#state.buffer_after, lists:duplicate(DelNum, $\s),
             xnfix(State),
             move_cursor(State, NewBBCols + BACols + DelNum, NewBBCols)])]),
    State#state{ buffer_before = NewBB };
handle_request(State, {delete, 0}) ->
    State;
handle_request(State, {move, N}) when N < 0 ->
    {_DelNum, DelCols, NewBA, NewBB} = split(-N, State#state.buffer_before),
    NewBBCols = cols(NewBB),
    write(State#state.tty,
          [unicode:characters_to_binary(
             [move_cursor(
                State, NewBBCols + DelCols, NewBBCols)])]),
    State#state{ buffer_before = NewBB,
                 buffer_after = NewBA ++ State#state.buffer_after};
handle_request(State, {move, N}) when N > 0 ->
    {_DelNum, DelCols, NewBB, NewBA} = split(N, State#state.buffer_after),
    BBCols = cols(State#state.buffer_before),
    write(State#state.tty,
          [unicode:characters_to_binary(
             [move_cursor(
                State, BBCols, BBCols + DelCols)])]),
    State#state{ buffer_after = NewBA,
                 buffer_before = NewBB ++ State#state.buffer_before};
handle_request(State, {move, 0}) ->
    State;
handle_request(State, beep) ->
    write(State#state.tty, [<<7>>]),
    State;
handle_request(State, Req) ->
    erlang:display({unhandled_request, Req}),
    State.

%% Split the buffer after N logical characters returning
%% the number of real characters deleted and the column length
%% of those characters
split(N, Buff) ->
    split(N, Buff, [], 0, 0).
split(0, Buff, Acc, Chars, Cols) ->
    {Chars, Cols, Acc, Buff};
split(_N, [], Acc, Chars, Cols) ->
    {Chars, Cols, Acc, []};
split(N, [Char | T], Acc, Cnt, Cols) when is_integer(Char) ->
    split(N - 1, T, [Char | Acc], Cnt + 1, Cols + wcwidth(Char));
split(N, [Chars | T], Acc, Cnt, Cols) ->
    split(N - 1, T, [Chars | Acc], Cnt + length(Chars), Cols + length(Chars)).

move_cursor(#state{ width = W } = State, FromCol, ToCol) ->
    [case (ToCol div W) - (FromCol div W) of
         0 -> "";
         N when N < 0 ->
             dbg({move, up, -N}),
             move(up, State, -N);
         N ->
             dbg({move, down, N}),
             move(down, State, N)
     end,
     case (ToCol rem W) - (FromCol rem W) of
         0 -> "";
         N when N < 0 ->
             dbg({down, left, -N}),
             move(left, State, -N);
         N ->
             dbg({down, right, N}),
             move(right, State, N)
     end].

move(up, #state{ up = Up }, N) ->
    lists:duplicate(N, Up);
move(down, #state{ down = Down }, N) ->
    lists:duplicate(N, Down);
move(left, #state{ left = Left }, N) ->
    lists:duplicate(N, Left);
move(right, #state{ right = Right }, N) ->
    lists:duplicate(N, Right).

cols([]) ->
    0;
cols([Char | T]) when is_integer(Char) ->
    wcwidth(Char) + cols(T);
cols([Chars | T]) ->
    length(Chars)  + cols(T).

xnfix(State) ->
    [$\s,move(left,State,1)].

insert_buf(State, Binary) when is_binary(Binary) ->
    insert_buf(State, Binary, []).
insert_buf(State, Bin, Acc) ->
    case string:next_grapheme(Bin) of
        [] ->
            ReverseAcc = lists:reverse(Acc),
            NewBB = Acc ++ State#state.buffer_before,
            write(State#state.tty,
                  [unicode:characters_to_binary(
                     [ReverseAcc%,xnfix(State)
                     ])]),
            State#state{ buffer_before = NewBB };
        [$\t | Rest] ->
            insert_buf(State, Rest, [State#state.tab | Acc]);
        [$\e | Rest] ->
            case re:run(State#state.ansi_regexp, Bin, [unicode]) of
                {match, [{0, N}]} ->
                    <<Ansi:N/binary, AnsiRest/binary>> = Bin,
                    insert_buf(State, AnsiRest, [Ansi | Acc]);
                _ ->
                    insert_buf(State, Rest, [$\e | Acc])
            end;
        [NLCR | Rest] when NLCR =:= $\n; NLCR =:= $\r ->
            Tail =
                if NLCR =:= $\n ->
                        <<$\r,$\n>>;
                   true ->
                        <<$\r>>
                end,
            [] = write(State#state.tty,[unicode:characters_to_binary(lists:reverse(Acc)),Tail]),
            insert_buf(State#state{ buffer_before = [] }, Rest, []);
        [Cluster | Rest] when is_list(Cluster) ->
            insert_buf(State, Rest, [Cluster | Acc]);
        [Char | Rest] when Char >= 128, Acc =:= [], State#state.buffer_before =/= [] ->
            [PrevChar | BB] = State#state.buffer_before,
            case string:next_grapheme([PrevChar | Bin]) of
                [PrevChar | _] ->
                    insert_buf(State, Rest, [Char | Acc]);
                [Cluster | ClusterRest] ->
                    {_, ToWrite} = lists:split(length(lists:flatten([PrevChar])), Cluster),
                    write(State#state.tty, [unicode:characters_to_binary(ToWrite)]),
                    insert_buf(State#state{ buffer_before = [Cluster | BB] }, ClusterRest, Acc)
            end;
        [Char | Rest] when Char >= 128 ->
            insert_buf(State, Rest, [Char | Acc]);
        [Char | Rest] ->
            case {isprint(Char), Char} of
                {true,_} ->
                    insert_buf(State, Rest, [Char | Acc]);
                {false, 8#177} -> %% DEL
                    insert_buf(State, Rest, ["^?" | Acc]);
                {false, _} ->
                    insert_buf(State, Rest, ["^" ++ [Char bor 8#40] | Acc])
            end
    end.

call({Pid, _TTY}, Request) ->
    Mref = monitor(process, Pid, [{alias, demonitor}]),
    Pid ! {call, Mref, Request},
    receive
        {Mref, Reply} ->
            erlang:demonitor(Mref, [flush]),
            {ok, Reply};
        {'DOWN', Mref, _, _, Reason} ->
            {error, Reason}
    end.

cast({Pid, _TTY}, Request) ->
    Pid ! {cast, Request},
    ok.

dbg(_) ->
    ok.

%% Nif functions
isatty(_Fd) ->
    erlang:nif_error(undef).
tty_init(_Fd, _Options) ->
    erlang:nif_error(undef).
tty_set(_TTY) ->
    erlang:nif_error(undef).
tty_termcap(_TTY, _TERM) ->
    erlang:nif_error(undef).
setlocale() ->
    erlang:nif_error(undef).
tty_select(_TTY, _FD) ->
    erlang:nif_error(undef).
write(_TTY, _IOVec) ->
    erlang:nif_error(undef).
tty_window_size(_TTY) ->
    erlang:nif_error(undef).
isprint(_Char) ->
    erlang:nif_error(undef).
wcwidth(_Char) ->
    erlang:nif_error(undef).
sizeof_wchar() ->
    erlang:nif_error(undef).
wcswidth(_Char) ->
    erlang:nif_error(undef).
