%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
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

%% Test the garbage collector (or Memory Recycler)

-module(gc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([all/0, suite/0]).

-export([
    grow_heap/1,
    grow_stack/1,
    grow_stack_heap/1,
    binary_vheap_gc/1,
    binary_vheap_shrink/1,
    max_heap_size/1,
    max_heap_size_large_hfrag/1,
    minor_major_gc_option_async/1,
    minor_major_gc_option_self/1,
    gc_signal_order/1,
    gc_dirty_exec_proc/1,
    alias_signals_in_gc/1
]).

suite() ->
    [{ct_hooks,[ts_install_cth]}].

all() -> 
    [grow_heap, grow_stack, grow_stack_heap,
     binary_vheap_gc, binary_vheap_shrink, max_heap_size,
     max_heap_size_large_hfrag,
    minor_major_gc_option_self,
    minor_major_gc_option_async, gc_signal_order, gc_dirty_exec_proc,
    alias_signals_in_gc].


%% Produce a growing list of elements,
%% for X calls, then drop one item per call
%% until the list is empty.
grow_heap(Config) when is_list(Config) ->
    ct:timetrap({minutes, 40}),
    ok  = grow_heap1(256),
    ok  = grow_heap1(512),
    ok  = grow_heap1(1024),
    ok  = grow_heap1(2048),
    ok.

grow_heap1(Len) ->
    io:format("~ngrow_heap with ~p items.",[Len]),
    show_heap("before:"),
    grow_heap1([], Len, 0, up),
    show_heap("after:").

grow_heap1(List, MaxLen, MaxLen, up) ->
    show_heap("top:"),
    grow_heap1(List, MaxLen, MaxLen-1, down);
grow_heap1(List, MaxLen, CurLen, up) ->
    NewList=[make_arbit()|List],
    grow_heap1(NewList, MaxLen, CurLen+1, up);
grow_heap1([], _MaxLen, _, down) ->
    ok;
grow_heap1([_|List], MaxLen, CurLen, down) ->
    C=erlang:unique_integer([positive]),
    Num     = C rem (length(List))+1,
    Elem    = lists:nth(Num, List),
    NewList = lists:delete(Elem, List),
    grow_heap1(NewList, MaxLen, CurLen-1, down).



%% Increase and decrease stack size, and
%% drop off some garbage from time to time.
grow_stack(Config) when is_list(Config) ->
    ct:timetrap({minutes, 80}),
    show_heap("before:"),
    grow_stack1(200, 0),
    show_heap("after:"),
    ok.

grow_stack1(0, _) ->
    ok;
grow_stack1(Recs, 0) ->
%    show_heap("running:"),
    grow_stack1(Recs-1, Recs),
    grow_stack1(0,0);
grow_stack1(Recs, CurRecs) ->
    grow_stack1(Recs, CurRecs-1),
    make_arbit(),
    grow_stack1(1,0),
    ok.


%% Let's see how BEAM handles this one...
%% While growing the heap, bounces the size of the
%% stack, and while reducing the heap, bounces the stack usage.
grow_stack_heap(Config) when is_list(Config) ->
    ct:timetrap({minutes, 40}),
    grow_stack_heap1(16),
    grow_stack_heap1(32),
    ok.

grow_stack_heap1(MaxLen) ->
    io:format("~ngrow_stack_heap with ~p items.",[MaxLen]),
    show_heap("before:"),
    grow_stack_heap1([], MaxLen, 0, up),
    show_heap("after:").

grow_stack_heap1(List, MaxLen, MaxLen, up) ->
    show_heap("top:"),
    grow_stack_heap1(List, MaxLen, MaxLen-1, down);
grow_stack_heap1(List, MaxLen, CurLen, up) ->
    grow_stack1(CurLen*2,0),
    grow_stack_heap1([make_arbit()|List], MaxLen, CurLen+1, up),
    ok;

grow_stack_heap1([], _MaxLen, _, down) -> ok;
grow_stack_heap1([_|List], MaxLen, CurLen, down) ->
    grow_stack1(CurLen*2,0),
    C=erlang:unique_integer([positive]),
    Num=C rem (length(List))+1,
    Elem=lists:nth(Num, List),
    NewList=lists:delete(Elem, List),
    grow_stack_heap1(NewList, MaxLen, CurLen-1, down),
    ok.


%% Create an arbitrary element/term.
make_arbit() ->
    {AA,BB,CC}=erlang:timestamp(),
    A=AA+1, B=BB+1, C=(CC+erlang:unique_integer([positive])) rem 1000000 + 1,
    New =
	case C rem 9 of
	    0 -> make_string((B div C) +5);
	    1 -> C;
	    2 -> make_ref();
	    3 -> self();
	    4 -> list_to_binary(make_string((C div B) + 12));
	    5 -> (C*B)/(A+1);
	    6 -> list_to_tuple(make_string((B div C) +5));
	    7 -> list_to_atom(make_string(((C div B) rem 254) + 2));
	    8 -> fun(X) -> {X,AA,make_string((B div C)+10)} end
	end,
    New.

%% Create an arbitrary string of a certain length.
make_string(Length) ->
    Alph="abcdefghjiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"++
	"0123456789",
    make_string(Alph, Length, []).

make_string(_, 0, Acc) ->
    Acc;
make_string(Alph, Length, Acc) ->
    C=erlang:unique_integer([positive]),
    Pos=1+(Length*C rem length(Alph)),
    make_string(Alph, Length-1, 
		[lists:nth(Pos,Alph)|Acc]).

show_heap(String) ->
    garbage_collect(self()),
    receive after 1 -> ok end,
    {heap_size, HSize}=process_info(self(), heap_size),
    {stack_size, SSize}=process_info(self(), stack_size),
    io:format("Heap/Stack "++String++"~p/~p", [HSize, SSize]).

binary_vheap_gc(_Config) ->
    #{initial := Initial,
      grown := Grown,
      promoted := Promoted,
      major := Major,
      first_minor := FirstMinor,
      second_minor := SecondMinor} = run_binary_vheap_test(gc),

    %% Both generations should grow to accommodate live binaries, with room
    %% in the old generation for binaries promoted by a minor collection.
    ?assert(vheap_block_size(Grown) > vheap_block_size(Initial)),
    ?assert(old_vheap_block_size(Grown) > old_vheap_block_size(Initial)),
    ?assert(old_vheap_block_size(Grown) > vheap_block_size(Grown)),
    ?assert(vheap_size(Grown) > vheap_block_size(Initial)),

    %% The binary fits after promotion, so consecutive requested minor
    %% collections must not turn into a major collection.
    ?assert(old_vheap_size(Promoted) > 0),
    ?assert(old_vheap_size(Promoted) < old_vheap_block_size(Promoted)),
    ?assertEqual(1, maps:get(minor_gcs, Promoted)),

    ?assertEqual(0, old_vheap_size(Major)),
    ?assert(vheap_size(Major) > 0),
    ?assertEqual(0, maps:get(minor_gcs, Major)),

    ?assert(old_vheap_size(FirstMinor) > 0),
    ?assert(old_vheap_size(FirstMinor) < old_vheap_block_size(FirstMinor)),
    ?assertEqual(1, maps:get(minor_gcs, FirstMinor)),
    ?assertEqual(2, maps:get(minor_gcs, SecondMinor)),
    ok.

binary_vheap_shrink(_Config) ->
    #{grown := Grown, shrunk := Shrunk} = run_binary_vheap_test(shrink),
    [First | _] = Shrunk,
    Last = lists:last(Shrunk),

    %% Once the binary dies, both limits should only move downwards.
    ?assert(vheap_block_size(First) =< vheap_block_size(Grown)),
    ?assert(old_vheap_block_size(First) =< old_vheap_block_size(Grown)),
    ?assert(vheap_block_size(Last) < vheap_block_size(Grown)),
    ?assert(old_vheap_block_size(Last) < old_vheap_block_size(Grown)),
    ?assertEqual(0, vheap_size(Last)),
    ?assertEqual(0, old_vheap_size(Last)),
    assert_vheap_shrinks(Shrunk),
    ok.

run_binary_vheap_test(Test) ->
    Parent = self(),
    {Pid, Monitor} = spawn_opt(
                       fun() ->
                               Parent ! {self(), binary_vheap_test(Test)}
                       end,
                       [monitor, {min_heap_size, 100000}]),
    receive
        {Pid, Result} ->
            erlang:demonitor(Monitor, [flush]),
            Result;
        {'DOWN', Monitor, process, Pid, Reason} ->
            ct:fail({binary_vheap_test_failed, Reason})
    after 10000 ->
            ct:fail(binary_vheap_test_timeout)
    end.

binary_vheap_test(Test) ->
    Initial = garbage_collection_info(),
    WordSize = erlang:system_info(wordsize),
    BinarySize = (vheap_block_size(Initial) + 1) * WordSize,
    Binary = binary:copy(<<0>>, BinarySize),

    true = erlang:garbage_collect(self(), [{type, major}]),
    Grown = garbage_collection_info(),
    case Test of
        gc ->
            true = erlang:garbage_collect(self(), [{type, minor}]),
            Promoted = garbage_collection_info(),
            true = erlang:garbage_collect(self(), [{type, major}]),
            Major = garbage_collection_info(),
            true = erlang:garbage_collect(self(), [{type, minor}]),
            FirstMinor = garbage_collection_info(),
            true = erlang:garbage_collect(self(), [{type, minor}]),
            SecondMinor = garbage_collection_info(),
            0 = binary:last(Binary),
            #{initial => Initial,
              grown => Grown,
              promoted => Promoted,
              major => Major,
              first_minor => FirstMinor,
              second_minor => SecondMinor};
        shrink ->
            0 = binary:last(Binary),
            binary_vheap_shrink(Grown, 10, [])
    end.

binary_vheap_shrink(Grown, Count, Acc) ->
    true = erlang:garbage_collect(self(), [{type, major}]),
    Info = garbage_collection_info(),
    case Count =:= 1 orelse
         (vheap_block_size(Info) < vheap_block_size(Grown) andalso
          old_vheap_block_size(Info) < old_vheap_block_size(Grown)) of
        true ->
            #{grown => Grown, shrunk => lists:reverse([Info | Acc])};
        false ->
            binary_vheap_shrink(Grown, Count - 1, [Info | Acc])
    end.

garbage_collection_info() ->
    {garbage_collection_info, Info} =
        process_info(self(), garbage_collection_info),
    {garbage_collection, GC} = process_info(self(), garbage_collection),
    maps:from_list([{minor_gcs, proplists:get_value(minor_gcs, GC)} | Info]).

vheap_size(Info) -> maps:get(bin_vheap_size, Info).
vheap_block_size(Info) -> maps:get(bin_vheap_block_size, Info).
old_vheap_size(Info) -> maps:get(bin_old_vheap_size, Info).
old_vheap_block_size(Info) -> maps:get(bin_old_vheap_block_size, Info).

assert_vheap_shrinks([_]) ->
    ok;
assert_vheap_shrinks([First, Second | Rest]) ->
    ?assert(vheap_block_size(Second) =< vheap_block_size(First)),
    ?assert(old_vheap_block_size(Second) =< old_vheap_block_size(First)),
    assert_vheap_shrinks([Second | Rest]).
    
%% Test that doing a remote GC that triggers the max heap size
%% kills the process.
max_heap_size(_Config) ->

    Pid = spawn_opt(fun long_receive/0,[{max_heap_size, 1024},
                                        {message_queue_data, on_heap}]),
    [Pid ! lists:duplicate(I,I) || I <- lists:seq(1,100)],
    Ref = erlang:monitor(process, Pid),

    %% Force messages to be viewed as part of heap
    erlang:process_info(Pid, messages),

    %% Do the GC that triggers max heap
    erlang:garbage_collect(Pid),

    %% Verify that max heap was triggered
    receive
        {'DOWN', Ref, process, Pid, killed} -> ok
    after 5000 ->
            ct:fail({process_did_not_die, Pid, erlang:process_info(Pid)})
    end.

long_receive() ->
    receive
    after 10000 ->
            ok
    end.

%% Trigger gc-after-bif with a large heap fragment, which provoked some bugs.
max_heap_size_large_hfrag(Config) ->
    max_heap_size_large_hfrag_do(),
    %% Repeat with major GC
    process_flag(fullsweep_after, 0),
    max_heap_size_large_hfrag_do(),
    ok.

max_heap_size_large_hfrag_do() ->
    {Pid, Ref} =
        spawn_opt(
          fun Fun() ->
                  erlang:make_tuple(2000, []),
                  Fun()
          end,
          [monitor, {max_heap_size, 1000}]),
    %% Verify that max heap was triggered
    {'DOWN', Ref, process, Pid, killed} =
        receive M -> M
        after 5000 ->
                ct:fail({process_did_not_die, Pid, erlang:process_info(Pid)})
        end,
    ok.

minor_major_gc_option_self(_Config) ->
    %% Try as major, the test process will self-trigger GC
    check_gc_tracing_around(
        fun(Pid, Ref) ->
            Pid ! {gc, Ref, major}
        end, [gc_major_start, gc_major_end]),

    %% Try as major dirty, the test process will self-trigger GC
    check_gc_tracing_around(
        fun(Pid, Ref) ->
            Pid ! {gc, Ref, major}
        end, [gc_major_start, gc_major_end],
      lists:seq(1,128 * 1024)),

    %% Try as minor, the test process will self-trigger GC
    check_gc_tracing_around(
        fun(Pid, Ref) ->
            Pid ! {gc, Ref, minor}
        end, [gc_minor_start, gc_minor_end]).

minor_major_gc_option_async(_Config) ->
    %% Try with default option, must be major GC
    check_gc_tracing_around(
        fun(Pid, _Ref) ->
            erlang:garbage_collect(Pid, [])
        end, [gc_major_start, gc_major_end]),

    %% Try with the 'major' type
    check_gc_tracing_around(
        fun(Pid, _Ref) ->
            erlang:garbage_collect(Pid, [{type, major}])
        end, [gc_major_start, gc_major_end]),

    %% Try with 'minor' option, once
    check_gc_tracing_around(
        fun(Pid, _Ref) ->
            erlang:garbage_collect(Pid, [{type, minor}])
        end, [gc_minor_start, gc_minor_end]),

    %% Try with 'minor' option, once, async
    check_gc_tracing_around(
        fun(Pid, Ref) ->
            ?assertEqual(async,
                erlang:garbage_collect(Pid, [{type, minor}, {async, Ref}])),

            receive
                {garbage_collect, Ref, true} ->
                    ok
            after 10000 ->
                ct:fail("Did not receive a completion notification on async GC")
            end
        end, [gc_minor_start, gc_minor_end]).

gc_signal_order(Config) when is_list(Config) ->
    process_flag(scheduler, 1),
    process_flag(priority, high),
    Ref = make_ref(),
    Pid = spawn_opt(fun () -> receive after infinity -> ok end end,[{scheduler, 1}]),
    spam_signals(Pid, 10000),
    %% EXIT signal *should* arrive...
    exit(Pid, kill),
    %% ... before GC signal...
    async = garbage_collect(Pid, [{async, Ref}]),
    %% ... which means that the result of the gc *should* be 'false'...
    false = busy_wait_gc_res(Ref),
    ok.

busy_wait_gc_res(Ref) ->
    receive
	{garbage_collect, Ref, Res} ->
	    Res
    after 0 ->
	    busy_wait_gc_res(Ref)
    end.

spam_signals(P, N) when N =< 0 ->
    ok;
spam_signals(P, N) ->
    link(P),
    unlink(P),
    spam_signals(P, N-2).

gc_dirty_exec_proc(Config) when is_list(Config) ->
    check_gc_tracing_around(
      fun(Pid, _Ref) ->
	      Pid ! {dirty_exec, 1000},
	      receive after 100 -> ok end,
	      true = erlang:garbage_collect(Pid, [{type, major}])
      end, [gc_major_start, gc_major_end]).

%% Traces garbage collection around the given operation, and fails the test if
%% it results in any unexpected messages or if the expected trace tags are not
%% received.
check_gc_tracing_around(Fun, ExpectedTraceTags) ->
    check_gc_tracing_around(Fun, ExpectedTraceTags, []).
check_gc_tracing_around(Fun, ExpectedTraceTags, State) ->
    Ref = erlang:make_ref(),
    Pid = spawn(
            fun() ->
                    (fun Endless(S) ->
                             receive
                                 {gc, Ref, Type} ->
                                     erlang:garbage_collect(self(), [{type, Type}]);
                                 {dirty_exec, Time} ->
                                     erts_debug:dirty_io(wait, Time)
                             after 100 ->
                                     ok
                             end,
                             Endless(S)
                     end)(State)
            end),
    erlang:garbage_collect(Pid, []),
    erlang:trace(Pid, true, [garbage_collection]),
    Fun(Pid, Ref),
    expect_trace_messages(Pid, ExpectedTraceTags),
    erlang:trace(Pid, false, [garbage_collection]),
    erlang:exit(Pid, kill),
    check_no_unexpected_messages().

%% Ensures that trace messages with the provided tags have all been received
%% within a reasonable timeframe.
expect_trace_messages(_Pid, []) ->
    ok;
expect_trace_messages(Pid, [Tag | TraceTags]) ->
    receive
        {trace, Pid, Tag, _Data} ->
            expect_trace_messages(Pid, TraceTags)
    after 4000 ->
        ct:fail("Didn't receive tag ~p within 4000ms", [Tag])
    end.

check_no_unexpected_messages() ->
    receive
        Anything ->
            ct:fail("Unexpected message: ~p", [Anything])
    after 0 ->
        ok
    end.

alias_signals_in_gc(Config) when is_list(Config) ->
    %% Make sure alias signals in rootset won't cause
    %% crashes...
    process_flag(scheduler, 1),
    process_flag(priority, normal),
    process_flag(message_queue_data, on_heap),
    Alias = alias(),
    %% We deactivate the alias since it is no point converting
    %% the alias signals into messages for this test...
    unalias(Alias), 
    Pid = spawn_opt(fun () ->
			    alias_sig_spammer(Alias, 100000)
		    end, [{scheduler, 1}, {priority, high}, link]),
    erlang:yield(),
    do_gc(10),
    unlink(Pid),
    exit(Pid, bang),
    false = is_process_alive(Pid),
    ok.

alias_sig_spammer(Alias, N) ->
    alias_sig_spammer(Alias, N, N).
    
alias_sig_spammer(Alias, 0, NStart) ->
    Alias ! [hello],
    receive after 100 -> ok end,
    alias_sig_spammer(Alias, NStart, NStart);
alias_sig_spammer(Alias, N, NStart) ->
    Alias ! [hello],
    alias_sig_spammer(Alias, N-1, NStart).

do_gc(0) ->
    ok;
do_gc(N) ->
    garbage_collect(),
    receive after 100 -> ok end,
    do_gc(N-1).
