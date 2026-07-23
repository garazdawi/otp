%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 2026. All Rights Reserved.
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

%% Tests for the tail-modulo-cons transform (idea #68 technique A): the `+tmc'
%% compiler option, the set_cons_tail instruction and its force-fullsweep GC
%% mechanism. Exercises both front-ends (body-recursive `[H|self(...)]' and the
%% tail-recursive accumulator+reverse idiom) and, crucially, the runtime GC
%% edge-repair path -- which is what validates the JIT emitter (arm or x86) at
%% run time on whatever host the suite executes on.

-module(tmc_SUITE).

-include_lib("common_test/include/ct.hrl").

-export([all/0, suite/0]).
-export([fe1_results/1, fe2_results/1, eval_order/1, off_byte_identical/1,
         gc_edge_repair/1, o1_stack/1, exception_mid_loop/1]).

suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap, {minutes, 5}}].

all() ->
    [fe1_results, fe2_results, eval_order, off_byte_identical,
     gc_edge_repair, o1_stack, exception_mid_loop].

%%%========================================================================
%%% Test module source (compiled both with and without +tmc)
%%%========================================================================

src(Name) ->
    lists:flatten(
      ["-module(", atom_to_list(Name), ").\n"
       "-export([map/2, app/2, squares/1, sq2/1, sideff/1]).\n"
       %% FE1: body-recursive [H|self(...)]
       "map(F,[H|T]) -> [F(H) | map(F,T)];\n"
       "map(_,[]) -> [].\n"
       "app([H|T],L) -> [H | app(T,L)];\n"       %% non-nil base
       "app([],L) -> L.\n"
       %% FE2: accumulator + reverse/1 and reverse/2
       "squares(L) -> squares(L,[]).\n"
       "squares([H|T],A) -> squares(T,[H*H|A]);\n"
       "squares([],A) -> lists:reverse(A).\n"
       "sq2(L) -> sq2(L,[]).\n"
       "sq2([H|T],A) -> sq2(T,[H|A]);\n"
       "sq2([],A) -> lists:reverse(A,[done]).\n"
       %% side-effecting element, to check evaluation order
       "sideff(L) -> sideff(L,[]).\n"
       "sideff([H|T],A) -> sideff(T,[emit(H)|A]);\n"
       "sideff([],A) -> lists:reverse(A).\n"
       "emit(X) -> put(seq,[X|case get(seq) of undefined->[]; S->S end]), X.\n"]).

forms(Src) ->
    {ok, Toks, _} = erl_scan:string(Src),
    forms_1(Toks, [], []).

forms_1([{dot,_}=D | Ts], Cur, Acc) ->
    {ok, Form} = erl_parse:parse_form(lists:reverse([D | Cur])),
    forms_1(Ts, [], [Form | Acc]);
forms_1([T | Ts], Cur, Acc) ->
    forms_1(Ts, [T | Cur], Acc);
forms_1([], [], Acc) ->
    lists:reverse(Acc).

compile_load(Name, Opts) ->
    Forms = forms(src(Name)),
    {ok, Name, Bin} = compile:forms(Forms, [binary, report_errors | Opts]),
    _ = code:purge(Name),
    {module, Name} = code:load_binary(Name, atom_to_list(Name) ++ ".beam", Bin),
    Bin.

%% tmc-transformed module + an untransformed reference module.
setup() ->
    _ = compile_load(tmc_on, [tmc]),
    _ = compile_load(tmc_ref, []),
    ok.

%%%========================================================================
%%% Front-end 1 -- body-recursive builders
%%%========================================================================

fe1_results(_Config) ->
    setup(),
    Dbl = fun(X) -> X * 2 end,
    Id = fun(X) -> X end,
    NumInputs = [[], [7], lists:seq(1,5), lists:seq(1,1000)],
    [same = cmp(tmc_on:map(Dbl, L), tmc_ref:map(Dbl, L)) || L <- NumInputs],
    %% identity map preserves element order (also on non-numeric elements)
    OrderInputs = [[], [7], lists:seq(1,20), [a,b,c,d]],
    [same = cmp(tmc_on:map(Id, L), L) || L <- OrderInputs],
    %% append (non-nil base)
    same = cmp(tmc_on:app([1,2,3], [4,5]), tmc_ref:app([1,2,3], [4,5])),
    same = cmp(tmc_on:app([], [9]), [9]),
    same = cmp(tmc_on:app([1], []), [1]),
    same = cmp(tmc_on:app([a,b], [c,d,e]), [a,b,c,d,e]),
    ok.

%%%========================================================================
%%% Front-end 2 -- accumulator + reverse (reverse/1 and reverse/2)
%%%========================================================================

fe2_results(_Config) ->
    setup(),
    Inputs = [[], [7], lists:seq(1,5), lists:seq(1,1000)],
    [same = cmp(tmc_on:squares(L), tmc_ref:squares(L)) || L <- Inputs],
    [same = cmp(tmc_on:squares(L), [X*X || X <- L]) || L <- Inputs],
    %% reverse/2 (non-nil seal)
    [same = cmp(tmc_on:sq2(L), tmc_ref:sq2(L)) || L <- Inputs],
    same = cmp(tmc_on:sq2([1,2,3]), [1,2,3,done]),
    ok.

%%%========================================================================
%%% Evaluation order preserved (side-effecting element)
%%%========================================================================

eval_order(_Config) ->
    setup(),
    L = [a,b,c,d,e],
    erase(seq),
    R1 = tmc_on:sideff(L),
    S1 = get(seq),
    erase(seq),
    R2 = tmc_ref:sideff(L),
    S2 = get(seq),
    same = cmp(R1, R2),           %% same result
    same = cmp(R1, L),            %% forward order
    same = cmp(S1, S2),           %% same evaluation order
    same = cmp(S1, lists:reverse(L)),  %% emitted a..e in order
    ok.

%%%========================================================================
%%% +tmc off is byte-identical for a module with no eligible function
%%%========================================================================

off_byte_identical(_Config) ->
    Src = "-module(tmc_noel).\n-export([f/1, g/2]).\n"
          "f(X) -> X + 1.\ng(A, B) -> {A, B, A * B}.\n",
    Forms = forms(Src),
    {ok, _, On}  = compile:forms(Forms, [binary, deterministic, tmc]),
    {ok, _, Off} = compile:forms(Forms, [binary, deterministic]),
    OnCode  = code_chunk(On),
    OffCode = code_chunk(Off),
    same = cmp(OnCode, OffCode),
    ok.

code_chunk(Bin) ->
    {ok, _, Chunks} = beam_lib:all_chunks(Bin),
    {_, Code} = lists:keyfind("Code", 1, Chunks),
    Code.

%%%========================================================================
%%% GC edge-repair -- forces the old->young edge + force-fullsweep repair.
%%% Arch-agnostic; this is what validates the JIT emitter's guard at runtime.
%%%========================================================================

gc_edge_repair(_Config) ->
    setup(),
    N = 50000,
    Ref = lists:seq(1, N),
    Parent = self(),
    %% Small heap + high fullsweep_after so minor GCs tenure the builder and any
    %% major GC is one forced by the set_cons_tail guard (not fullsweep_after).
    {Pid, MRef} =
        spawn_opt(fun() ->
                          %% element fn allocates garbage to pressure the young heap
                          F = fun(X) -> _ = lists:duplicate(30, X), X end,
                          Built = tmc_on:map(F, lists:seq(1, N)),
                          Parent ! {self(), result, Built}
                  end,
                  [monitor, {min_heap_size, 233}, {fullsweep_after, 65535}]),
    1 = erlang:trace(Pid, true, [garbage_collection]),
    {Minor, Major} = collect_gc(Pid, MRef, 0, 0, undefined),
    receive
        {Pid, result, Built} ->
            same = cmp(Built, Ref),          %% intact + correctly ordered
            true = (Minor > 0),              %% the build did span GCs
            true = (Major > 0)               %% the force-fullsweep path fired
    after 60000 ->
            ct:fail(timeout)
    end,
    ok.

collect_gc(Pid, MRef, Mi, Ma, Res) ->
    receive
        {trace, Pid, gc_minor_start, _} -> collect_gc(Pid, MRef, Mi+1, Ma, Res);
        {trace, Pid, gc_major_start, _} -> collect_gc(Pid, MRef, Mi, Ma+1, Res);
        {trace, Pid, _, _}              -> collect_gc(Pid, MRef, Mi, Ma, Res);
        {'DOWN', MRef, process, Pid, normal} -> {Mi, Ma};
        {'DOWN', MRef, process, Pid, R} -> ct:fail({worker_crashed, R})
    after 60000 ->
            ct:fail(gc_collect_timeout)
    end.

%%%========================================================================
%%% O(1) stack -- a deep TMC build stays flat; body-recursion grows O(n).
%%%========================================================================

o1_stack(_Config) ->
    setup(),
    N = 200000,
    L = lists:seq(1, N),
    Probe = fun(X) ->
                    {stack_size, S} = process_info(self(), stack_size),
                    put(max_stack, max(get(max_stack), S)),
                    X
            end,
    put(max_stack, 0),
    _ = tmc_on:map(Probe, L),
    TmcMax = get(max_stack),
    put(max_stack, 0),
    _ = tmc_ref:map(Probe, L),
    RefMax = get(max_stack),
    ct:log("TMC max stack ~p words, body-recursive max stack ~p words",
           [TmcMax, RefMax]),
    true = (TmcMax < 1000),                 %% O(1)
    true = (RefMax > 100 * TmcMax),         %% reference grows O(n)
    ok.

%%%========================================================================
%%% Exception mid-loop -- propagates, partial builder abandoned, heap intact.
%%%========================================================================

exception_mid_loop(_Config) ->
    setup(),
    F = fun(5000) -> erlang:error(boom); (X) -> X end,
    boom = try tmc_on:map(F, lists:seq(1, 10000)), no_exception
           catch error:boom -> boom end,
    %% heap must be intact afterwards
    _ = [tmc_on:map(fun(X) -> X * 2 end, lists:seq(1, 2000)) || _ <- lists:seq(1, 50)],
    erlang:garbage_collect(),
    same = cmp(tmc_on:map(fun(X) -> X end, lists:seq(1, 500)), lists:seq(1, 500)),
    ok.

%%%========================================================================
%%% Helper
%%%========================================================================

cmp(X, X) -> same;
cmp(X, Y) -> ct:fail({mismatch, X, Y}).
