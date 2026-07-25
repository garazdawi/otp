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

%% Tests (and full-coverage driver) for the tail-modulo-cons transform,
%% beam_ssa_tmc, enabled by the `tmc' compile option (idea #68, technique A).
%%
%% Each testcase compiles source with `[tmc]' -- which exercises the whole
%% chain: the beam_ssa_tmc recognizer + destination-passing lowering, the
%% set_cons_tail lowering in beam_ssa_codegen, and its validation in
%% beam_validator -- and checks that the result is identical to a plain
%% (non-tmc) build. The rejection and coverage cases additionally drive every
%% recognizer near-miss / edge branch so the transform is fully covered.

-module(beam_ssa_tmc_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("compiler/src/beam_ssa.hrl").

-export([all/0, suite/0, groups/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         fe1_builders/1, fe2_accrev/1, rejections/1,
         report/1, api/1, disasm/1]).

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() ->
    [{group, p}].

groups() ->
    [{p, test_lib:parallel(),
      [fe1_builders, fe2_accrev, rejections, report, api, disasm]}].

init_per_suite(Config) ->
    test_lib:recompile(?MODULE),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_Name, Config) ->
    Config.

end_per_group(_Name, Config) ->
    Config.

%%%======================================================================
%%% Front-end 1: body-recursive builders  [H | f(T)]
%%%======================================================================
fe1_builders(_Config) ->
    L = [1, 2, 3],

    %% plain element (byte-identical to the non-tmc build)
    same([1,2,3], "f([H|T]) -> [H|f(T)];\nf([]) -> [].\n", f, [L]),
    %% arithmetic element
    same([2,4,6], "f([H|T]) -> [H*2|f(T)];\nf([]) -> [].\n", f, [L]),
    %% heap-allocated elements -- tuple / list-of-tuples / nested list / binary.
    %% These are the cases that regressed pre-fix (element sunk into the removed
    %% cons block).
    same([{1},{2},{3}], "f([H|T]) -> [{H}|f(T)];\nf([]) -> [].\n", f, [L]),
    same([{a,2},{b,3}],
         "f([{K,V}|T]) -> [{K,V+1}|f(T)];\nf([]) -> [].\n", f,
         [[{a,1},{b,2}]]),
    same([[1,1],[2,2],[3,3]], "f([H|T]) -> [[H,H]|f(T)];\nf([]) -> [].\n", f, [L]),
    same([<<1>>,<<2>>,<<3>>], "f([H|T]) -> [<<H>>|f(T)];\nf([]) -> [].\n", f, [L]),

    %% base clause returns a function argument (is_base_val var-in-args)
    same([1,2,3], "f([H|T],A) -> [H|f(T,A)];\nf([],A) -> A.\n", f, [L, []]),
    %% base clause returns [Arg] (put_list whose tail is [] -- exercises the
    %% non-cons cons_site path)
    same([1,2,3,x], "f([H|T],Y) -> [H|f(T,Y)];\nf([],Y) -> [Y].\n", f, [L, x]),
    %% a second, non-self local call in a returned cons (cons_site self-call
    %% test false) alongside a real TMC edge
    same([1,2,3], "f([H|T]) when H > 0 -> [H|f(T)];\n"
                  "f([H|_]) -> [H|g(H)];\nf([]) -> [].\ng(X) -> [X].\n",
         f, [[1,2,3]]),
    ok.

%%%======================================================================
%%% Front-end 2: accumulator + reverse  ->  forward TMC
%%%======================================================================
fe2_accrev(_Config) ->
    L = [1, 2, 3, 4],
    %% reverse/1 base
    same([1,2,3,4], "f([H|T],A) -> f(T,[H|A]);\nf([],A) -> lists:reverse(A).\n",
         f, [L, []]),
    %% reverse/1 with a non-empty initial accumulator: reverse([z]) is prepended
    same([z,1,2,3,4], "f([H|T],A) -> f(T,[H|A]);\nf([],A) -> lists:reverse(A).\n",
         f, [L, [z]]),
    %% arithmetic element
    same([2,4,6,8], "f([H|T],A) -> f(T,[H*2|A]);\nf([],A) -> lists:reverse(A).\n",
         f, [L, []]),
    %% heap element
    same([{1},{2},{3},{4}],
         "f([H|T],A) -> f(T,[{H}|A]);\nf([],A) -> lists:reverse(A).\n", f, [L, []]),
    %% reverse/2 with a real tail argument
    same([1,2,3,4,tail],
         "f([H|T],A,E) -> f(T,[H|A],E);\nf([],A,E) -> lists:reverse(A,E).\n",
         f, [L, [], [tail]]),
    %% reverse/2 with an empty (literal []) tail argument
    same([1,2,3,4], "f([H|T],A) -> f(T,[H|A]);\nf([],A) -> lists:reverse(A,[]).\n",
         f, [L, []]),
    ok.

%%%======================================================================
%%% Recognizer rejections / near-misses. These must compile cleanly with
%%% `tmc' and produce the same result as without it (i.e. left untransformed).
%%%======================================================================
rejections(_Config) ->
    P = [1, 2, 3],   %% all-positive: keeps the "when H > 0" clauses on the fast
                     %% path so the deliberately-broken fallback clauses of some
                     %% cases (tuple_size on a list, etc.) are never evaluated.

    %% ---- FE1 near-misses ----
    %% not a builder at all
    same_ref("f(X) -> X.\n", f, [P]),
    %% base returns a non-[]/non-arg literal -> no base site ({_,[]} -> no)
    same_ref("f([H|T]) -> [H|f(T)];\nf([]) -> [x].\n", f, [P]),
    %% two cons sites (two recursive clauses)
    same_ref("f([H|T]) when H > 0 -> [H|f(T)];\n"
             "f([H|T]) -> [H|f(T)];\nf([]) -> [].\n", f, [P]),
    %% a real TMC edge plus a self call consumed in a non-cons position
    %% (good_use rejects on the second self call)
    same_ref("f([H|T]) when H > 0 -> [H|f(T)];\n"
             "f([_|T]) -> X = f(T), tuple_size(X);\nf([]) -> [].\n", f, [P]),
    %% a real TMC edge plus a self call whose result is discarded (good_use: no
    %% real uses)
    same_ref("f([H|T]) when H > 0 -> [H|f(T)];\n"
             "f([_|T]) -> _ = f(T), [];\nf([]) -> [].\n", f, [P]),
    %% 20-deep nested cons of the self call -> good_use fuel exhausted
    same_ref("f([H|T]) when H > 0 -> [H|f(T)];\n"
             "f([_|T]) -> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20|f(T)];\n"
             "f([]) -> [].\n", f, [P]),

    %% ---- FE2 near-misses ----
    %% base returns the accumulator itself, not reverse(Acc)
    same_ref("f([H|T],A) -> f(T,[H|A]);\nf([],A) -> A.\n", f, [P, []]),
    %% self-call arg is not a prepend of a parameter
    same_ref("f([_|T],A) -> f(T,A);\nf([],A) -> lists:reverse(A).\n", f, [P, []]),
    %% reverse(Acc) is wrapped, not the sole terminal use (find_base_reverse no)
    same_ref("f([H|T],A) -> f(T,[H|A]);\n"
             "f([],A) -> {lists:reverse(A), length(A)}.\n", f, [P, []]),
    %% Acc observed mid-loop (acc_used_only false)
    same_ref("f([H|T],A) -> _ = erlang:phash2(A), f(T,[H|A]);\n"
             "f([],A) -> lists:reverse(A).\n", f, [P, []]),
    %% the prepend result also escapes (used_only_by false)
    same_ref("f([H|T],A) -> X = [H|A], _ = erlang:phash2(X), f(T,X);\n"
             "f([],A) -> lists:reverse(A).\n", f, [P, []]),
    %% self call not returned directly (extract_accrev outer no)
    same_ref("f([_|T]) -> X = f(T), {ok,X};\nf([]) -> {ok,done}.\n", f, [P]),
    %% two self calls
    same_ref("f([H|T],A) when H > 0 -> f(T,[H|A]);\n"
             "f([_|T],A) -> f(T,A);\nf([],A) -> lists:reverse(A).\n", f, [P, []]),
    %% more than one lists:reverse base (find_base_reverse no)
    same_ref("f([H|T],A) when H > 0 -> f(T,[H|A]);\n"
             "f([],A) when A =/= [] -> lists:reverse(A);\n"
             "f([],A) -> lists:reverse(A).\n", f, [P, []]),
    ok.

%%%======================================================================
%%% `tmc_report' option -- prints the rewritten functions.
%%%======================================================================
report(_Config) ->
    Src = "f([H|T]) -> [H|f(T)];\nf([]) -> [].\n"
          "g([H|T],A) -> g(T,[H|A]);\ng([],A) -> lists:reverse(A).\n",
    {Mod, Out} = compile_capture(Src, [tmc, tmc_report]),
    [1,2,3] = Mod:f([1,2,3]),
    [1,2,3] = Mod:g([1,2,3], []),
    %% the report mentions both rewritten functions
    true = string:find(Out, "rewrote") =/= nomatch,
    true = string:find(Out, "body-rec") =/= nomatch,
    true = string:find(Out, "acc+reverse") =/= nomatch,
    ok.

%%%======================================================================
%%% The exported eligible/2 and recognize/2 predicates.
%%%======================================================================
api(_Config) ->
    Builder = "f([H|T]) -> [H|f(T)];\nf([]) -> [].\n",
    Plain = "h(X) -> X.\n",
    [{{f,1}, FB}] = to_ssa(Builder),
    [{{h,1}, HB}] = to_ssa(Plain),

    true = beam_ssa_tmc:eligible({f,1}, FB),
    false = beam_ssa_tmc:eligible({h,1}, HB),

    {true, [_|_]} = beam_ssa_tmc:recognize({f,1}, FB),
    false = beam_ssa_tmc:recognize({h,1}, HB),
    ok.

%%%======================================================================
%%% A set_cons_tail-bearing module round-trips through beam_disasm.
%%%======================================================================
disasm(_Config) ->
    Src = "f([H|T]) -> [{H}|f(T)];\nf([]) -> [].\n",
    {Mod, Beam} = compile_bin(Src, [tmc]),
    {beam_file, Mod, _Exp, _Attr, _Ci, Fns} = beam_disasm:file(Beam),
    Code = lists:append([Is || {function,_,_,_,Is} <- Fns]),
    true = lists:any(fun({set_cons_tail,_,_}) -> true;
                        (_) -> false
                     end, Code),
    ok.

%%%======================================================================
%%% Helpers
%%%======================================================================

%% Compile Src both with and without `tmc', assert both give Expect for
%% Mod:Fun(Args).
same(Expect, Src, Fun, Args) ->
    Ref = compile_load(Src, []),
    Tmc = compile_load(Src, [tmc]),
    Expect = apply(Ref, Fun, Args),
    Expect = apply(Tmc, Fun, Args),
    ok.

%% As `same/4' but without pinning the value: assert only that `tmc' does not
%% change the result (used for the recognizer rejection cases).
same_ref(Src, Fun, Args) ->
    Ref = compile_load(Src, []),
    Tmc = compile_load(Src, [tmc]),
    Expect = apply(Ref, Fun, Args),
    Expect = apply(Tmc, Fun, Args),
    ok.

compile_load(Src, Opts) ->
    {Mod, Bin} = compile_bin(Src, Opts),
    _ = code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".beam", Bin),
    Mod.

compile_bin(Src, Opts) ->
    Mod = uniq_mod(),
    Wrapped = "-module(" ++ atom_to_list(Mod) ++ ").\n"
              "-compile([export_all,nowarn_export_all]).\n" ++ Src,
    Forms = to_forms(Wrapped),
    {ok, Mod, Bin} = compile:forms(Forms, [binary, return_errors | Opts]),
    {Mod, Bin}.

%% Compile with tmc_report and capture the printed output.
compile_capture(Src, Opts) ->
    Mod = uniq_mod(),
    Wrapped = "-module(" ++ atom_to_list(Mod) ++ ").\n"
              "-compile([export_all,nowarn_export_all]).\n" ++ Src,
    Forms = to_forms(Wrapped),
    {{ok, Mod, Bin}, Out} =
        with_captured_output(fun() ->
                                     compile:forms(Forms, [binary, return_errors | Opts])
                             end),
    _ = code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".beam", Bin),
    {Mod, Out}.

with_captured_output(Fun) ->
    Group = group_leader(),
    {ok, Dev} = ct_capture_start(),
    group_leader(Dev, self()),
    try
        R = Fun(),
        {R, ct_capture_stop(Dev)}
    after
        group_leader(Group, self())
    end.

%% Minimal capture IO server: collect everything written and return it.
ct_capture_start() ->
    Pid = spawn_link(fun() -> capture_loop([]) end),
    {ok, Pid}.

ct_capture_stop(Pid) ->
    Pid ! {get, self()},
    receive {captured, Data} -> Data after 5000 -> "" end.

capture_loop(Acc) ->
    receive
        {io_request, From, Ref, {put_chars, _Enc, Chars}} ->
            From ! {io_reply, Ref, ok},
            capture_loop([Chars | Acc]);
        {io_request, From, Ref, {put_chars, _Enc, M, F, A}} ->
            From ! {io_reply, Ref, ok},
            capture_loop([apply(M, F, A) | Acc]);
        {io_request, From, Ref, _Other} ->
            From ! {io_reply, Ref, ok},
            capture_loop(Acc);
        {get, From} ->
            From ! {captured, lists:flatten(lists:reverse(Acc))}
    end.

%% Optimized SSA blocks for each function of Src, as [{ {Name,Arity}, Blocks }].
to_ssa(Src) ->
    Mod = uniq_mod(),
    Wrapped = "-module(" ++ atom_to_list(Mod) ++ ").\n"
              "-compile([export_all,nowarn_export_all]).\n" ++ Src,
    Forms = to_forms(Wrapped),
    {ok, Mod, Core} = compile:forms(Forms, [to_core, binary]),
    {ok, M0, _} = beam_core_to_ssa:module(Core, []),
    {ok, #b_module{body=Opt}} = beam_ssa_opt:module(M0, []),
    [{fa(F), F#b_function.bs}
     || #b_function{anno=A}=F <- Opt,
        not is_autogen(maps:get(func_info, A))].

fa(#b_function{anno=Anno}) ->
    {_M, N, Ar} = maps:get(func_info, Anno),
    {N, Ar}.

is_autogen({_M, module_info, _}) -> true;
is_autogen(_) -> false.

to_forms(Str) ->
    {ok, Tokens, _} = erl_scan:string(Str),
    [begin
         {ok, Form} = erl_parse:parse_form(Ts),
         Form
     end || Ts <- split_dots(Tokens, [], [])].

split_dots([{dot,_}=D | Rest], Cur, Acc) ->
    split_dots(Rest, [], [lists:reverse([D | Cur]) | Acc]);
split_dots([T | Rest], Cur, Acc) ->
    split_dots(Rest, [T | Cur], Acc);
split_dots([], _, Acc) ->
    lists:reverse(Acc).

uniq_mod() ->
    list_to_atom("tmc_test" ++ test_lib:uniq()).
