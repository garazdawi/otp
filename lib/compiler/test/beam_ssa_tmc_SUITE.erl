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
         fe1_builders/1, fe2_accrev/1, filters/1, multi_cons/1, rejections/1,
         report/1, api/1, disasm/1, debug_info/1]).

suite() ->
    [{ct_hooks, [ts_install_cth]},
     {timetrap, {minutes, 2}}].

all() ->
    [{group, p}].

groups() ->
    [{p, test_lib:parallel(),
      [fe1_builders, fe2_accrev, filters, multi_cons, rejections,
       report, api, disasm, debug_info]}].

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
    %% element built from a case -> the transformed helper carries a phi/switch
    same([1,2,3,0],
         "f([X|Xs]) -> [case X of a->1; b->2; c->3; _->0 end | f(Xs)];\n"
         "f([]) -> [].\n", f, [[a,b,c,x]]),
    %% REGRESSION (epp:coalesce_strings/1 shape): a non-cons clause returns a
    %% computed value (a call result) that must SEAL the prefix already built,
    %% not replace it. The `stop' clause is hit mid-list, so the [1,2] prefix
    %% must survive. (Miscompiled before the seal-any-tail fix: returned [4,3].)
    same([1,2,4,3],
         "f([stop|T]) -> lists:reverse(T);\n"
         "f([H|T]) -> [H|f(T)];\nf([]) -> [].\n", f, [[1,2,stop,3,4]]),
    %% a computed base returning the tail argument via a helper call
    same([1,2,7,8],
         "f([stop|T]) -> id(T);\nf([H|T]) -> [H|f(T)];\nf([]) -> [].\n"
         "id(X) -> X.\n", f, [[1,2,stop,7,8]]),
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

    %% --- widening: tail-recursive filter (skip edge threads Acc unchanged) ---
    %% guarded filter: keep positives
    same([1,3,5], "f([H|T],A) when H > 0 -> f(T,[H|A]);\n"
                  "f([_|T],A) -> f(T,A);\nf([],A) -> lists:reverse(A).\n",
         f, [[1,-2,3,-4,5], []]),
    %% case-driven skip: keep evens
    same([2,4,6], "f([H|T],A) -> case H rem 2 of 0 -> f(T,[H|A]); _ -> f(T,A) end;\n"
                  "f([],A) -> lists:reverse(A).\n", f, [[1,2,3,4,5,6], []]),
    %% leading and trailing skips
    same([a,b], "f([x|T],A) -> f(T,A);\nf([H|T],A) -> f(T,[H|A]);\n"
                "f([],A) -> lists:reverse(A).\n", f, [[x,a,b,x], []]),
    %% filter with a non-empty initial accumulator (reverse([z]) prefix survives)
    same([z,1,3], "f([H|T],A) when H > 0 -> f(T,[H|A]);\n"
                  "f([_|T],A) -> f(T,A);\nf([],A) -> lists:reverse(A).\n",
         f, [[1,-2,3], [z]]),
    %% map+filter
    same([10,30], "f([H|T],A) when H > 0 -> f(T,[H*10|A]);\n"
                  "f([_|T],A) -> f(T,A);\nf([],A) -> lists:reverse(A).\n",
         f, [[1,-2,3,-4], []]),

    %% --- widening: multiple prepend clauses (shared single Dest) ---
    same([1,2,1,2], "f([a|T],A) -> f(T,[1|A]);\nf([b|T],A) -> f(T,[2|A]);\n"
                    "f([],A) -> lists:reverse(A).\n", f, [[a,b,a,b], []]),
    %% multi-clause + filter combined
    same([1,2,1], "f([a|T],A) -> f(T,[1|A]);\nf([b|T],A) -> f(T,[2|A]);\n"
                  "f([_|T],A) -> f(T,A);\nf([],A) -> lists:reverse(A).\n",
         f, [[a,x,b,y,a], []]),

    %% --- widening: multi-prepend per step (forward cell chain) ---
    same([1,2,2,4], "f([H|T],A) -> f(T,[H*2,H|A]);\nf([],A) -> lists:reverse(A).\n",
         f, [[1,2], []]),
    %% filter + multi-prepend
    same([1,1,3,3], "f([H|T],A) when H > 0 -> f(T,[H,H|A]);\n"
                    "f([_|T],A) -> f(T,A);\nf([],A) -> lists:reverse(A).\n",
         f, [[1,-2,3], []]),
    %% multi-prepend with a reverse/2 tail
    same([1,2,2,4,end0],
         "f([H|T],A) -> f(T,[H*2,H|A]);\nf([],A) -> lists:reverse(A,[end0]).\n",
         f, [[1,2], []]),
    ok.

%%%======================================================================
%%% Filters -- multiple self calls: a cons edge plus `continue' (skip) edges
%%% that must thread the loop, not seal. (Filtered list comprehensions compile
%%% to exactly this shape.)
%%%======================================================================
filters(_Config) ->
    In = [1, -2, 3, -4, 5, 0, 6, -7],
    %% guarded filter: keep positives
    same([1,3,5,6], "f([H|T]) when H > 0 -> [H|f(T)];\n"
                    "f([_|T]) -> f(T);\nf([]) -> [].\n", f, [In]),
    %% two distinct skip clauses
    same([1,2,3], "f([a|T]) -> f(T);\nf([b|T]) -> f(T);\n"
                  "f([H|T]) -> [H|f(T)];\nf([]) -> [].\n", f, [[a,1,b,2,a,3,b]]),
    %% filter with a computed (non-[]) base -- seal the base, thread the skips
    same([1,3,5,6,x], "f([H|T]) when H > 0 -> [H|f(T)];\n"
                      "f([_|T]) -> f(T);\nf([]) -> [x].\n", f, [In]),
    %% map+filter: transform the kept elements
    same([10,30,50,60], "f([H|T]) when H > 0 -> [H*10|f(T)];\n"
                        "f([_|T]) -> f(T);\nf([]) -> [].\n", f, [In]),
    %% filtered list comprehension (the compiler-generated skip shape)
    same([1,4,16,25], "f(L) -> [X*X || X <- L, X rem 3 =/= 0].\n", f,
         [[1,2,3,4,5,6]]),
    %% leading and trailing skips
    same([2,4], "f([H|T]) when H rem 2 =:= 0 -> [H|f(T)];\n"
                "f([_|T]) -> f(T);\nf([]) -> [].\n", f, [[1,2,3,4,5]]),
    ok.

%%%======================================================================
%%% Multiple cons clauses (several cons edges) and multi-cons per step
%%% (`[A, B | self()]'): more than one clause conses into the single Dest, and
%%% a clause may build a chain of cells per iteration.
%%%======================================================================
multi_cons(_Config) ->
    L = [1, -2, 3, -4, 5],
    %% multi-clause: two cons edges with different elements
    same([1,2,3,4,5], "f([H|T]) when H > 0 -> [H|f(T)];\n"
                      "f([H|T]) -> [-H|f(T)];\nf([]) -> [].\n", f, [L]),
    %% multi-clause with distinct guards/bases
    same([1,2,0,1,0], "f([a|T]) -> [1|f(T)];\nf([b|T]) -> [2|f(T)];\n"
                      "f([_|T]) -> [0|f(T)];\nf([]) -> [].\n", f, [[a,b,x,a,y]]),
    %% multi-cons: two elements per step
    same([1,2,2,4,3,6], "f([H|T]) -> [H, H*2 | f(T)];\nf([]) -> [].\n", f, [[1,2,3]]),
    %% multi-cons: three elements per step, heap element
    same([1,{1},2,2,{2},3], "f([H|T]) -> [H, {H}, H+1 | f(T)];\nf([]) -> [].\n",
         f, [[1,2]]),
    %% multi-cons with a non-nil base (append-like)
    same([1,1,2,2,x,y], "f([H|T],A) -> [H, H | f(T,A)];\nf([],A) -> A.\n",
         f, [[1,2], [x,y]]),
    %% combined: multi-clause + multi-cons + filter skip
    same([1,plus,3,plus,7,plus], "f([H|T]) when H > 0 -> [H, plus | f(T)];\n"
                                 "f([0|T]) -> f(T);\nf([H|T]) -> [H|f(T)];\nf([]) -> [].\n",
         f, [[1, 0, 3, 0, 7]]),
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
    %% element computed AFTER the self call, from calls -- the lowering would
    %% strand its definition, so the well-formedness guard rejects the transform
    same_ref("f([X|Xs]) -> R = f(Xs), Y = list_to_atom(integer_to_list(X)), [Y|R];\n"
             "f([]) -> [].\n", f, [P]),
    %% two cons returns sharing ONE self call (`R = f(...), ...[a|R]...[b|R]...')
    %% -> the cons edges are not distinct, so it is rejected
    same_ref("f(0) -> [];\n"
             "f(X) -> R = f(X-1), case X rem 2 of 0 -> [a|R]; _ -> [b|R] end.\n",
             f, [4]),
    %% ONE self call both consed AND skip-returned (the shape the inlined
    %% lists:filter expands to: `R = f(T), case P of true -> [H|R]; false -> R'):
    %% the cons edge's self call is also a direct return, so it is rejected --
    %% otherwise the DPS lowering would drop the skip and keep every element.
    same([2,4], "f([H|T]) -> R = f(T), case H rem 2 of 0 -> [H|R]; _ -> R end;\n"
                "f([]) -> [].\n", f, [[1,2,3,4]]),

    %% ---- FE2 near-misses ----
    %% base returns the accumulator itself, not reverse(Acc)
    same_ref("f([H|T],A) -> f(T,[H|A]);\nf([],A) -> A.\n", f, [P, []]),
    %% pure skip loop -- no prepend edge, so nothing is built (PrependSites empty)
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
    %% AccVar referenced at two argument positions (not just the accumulator
    %% position) -> classify_self bad (RefPositions =/= [P])
    same_ref("f([H|_],A) -> f(A, [H|A]);\nf([],A) -> lists:reverse(A).\n",
             f, [P, []]),
    %% a clause returns a raw value (neither self, reverse-base nor error) that
    %% would leave the Dest hole unsealed -> all_rets_accounted false
    same_ref("f([stop|_],_) -> other;\nf([H|T],A) -> f(T,[H|A]);\n"
             "f([],A) -> lists:reverse(A).\n", f, [[1,2,stop,3], []]),
    %% two DISTINCT lists:reverse bases of the same accumulator (arity 1 and
    %% arity 2, so they do not merge) -> find_base_reverse no
    same_ref("f([a|T],A) -> f(T,[a|A]);\nf([],A) -> lists:reverse(A);\n"
             "f([stop|T],A) -> lists:reverse(A, T).\n", f, [[a,a], []]),
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
%%% beam_debug_info: a DPS-generated helper is created after the abstract-code
%%% stage, so its `debug_line' instructions have no source-level identity and
%%% break the debug-line semantics beam_debug_info records (index<->function map,
%%% per-line reachability). A debugging build wants source-faithful code, so the
%%% `beam_debug_info' option disables the pass (compile:expand_opt adds `no_tmc',
%%% alongside no_copt/no_bsm_opt/...). Assert that FE1 and FE2 builders produce
%%% NO `-tmc-' helper under beam_debug_info, and that the emitted code is
%%% identical to a `no_tmc' build (the pass is fully gated off).
%%%======================================================================
debug_info(_Config) ->
    Src = "map([H|T]) -> [H*2|map(T)];\nmap([]) -> [].\n"
          "filt([H|T]) when H > 0 -> [H|filt(T)];\n"
          "filt([_|T]) -> filt(T);\nfilt([]) -> [].\n"
          "acc([H|T],A) -> acc(T,[H|A]);\nacc([],A) -> lists:reverse(A).\n"
          "accf([H|T],A) when H > 0 -> accf(T,[H|A]);\n"
          "accf([_|T],A) -> accf(T,A);\naccf([],A) -> lists:reverse(A).\n",
    {Mod, Bin} = compile_bin(Src, [beam_debug_info]),
    _ = code:purge(Mod),
    {module, Mod} = code:load_binary(Mod, atom_to_list(Mod) ++ ".beam", Bin),
    [2,4,6] = Mod:map([1,2,3]),
    [1,3] = Mod:filt([1,-2,3]),
    [1,2,3] = Mod:acc([1,2,3], []),
    [1,3] = Mod:accf([1,-2,3], []),
    %% the pass is gated off under beam_debug_info: no generated helper exists
    {beam_file, Mod, _, _, _, Fns} = beam_disasm:file(Bin),
    [] = [N || {function, N, _, _, _} <- Fns, lists:prefix("-tmc-", atom_to_list(N))],
    %% and the Code chunk is identical to a `no_tmc' build (fully gated)
    {_, BinRef} = compile_bin(Src, [beam_debug_info, no_tmc]),
    {ok, {Mod, [{"Code", C}]}} = beam_lib:chunks(Bin, ["Code"]),
    {ok, {_, [{"Code", C}]}} = beam_lib:chunks(BinRef, ["Code"]),
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
