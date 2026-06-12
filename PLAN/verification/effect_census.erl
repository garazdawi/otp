#!/usr/bin/env escript
%% -*- erlang -*-
%%! +S1
%%
%% effect_census.erl — the S2 scope-risk census from PLAN/T2/08 §6.
%%
%% Walks .beam files (no debug_info needed; disassembles the code
%% chunk), finds self-tail-recursive "loop functions", and buckets
%% them by what blocks the v1 loop tier from compiling them:
%%
%%   A  pure v1 op set only            -> compiles under the original
%%                                        effect-free-only rule
%%   B  + effectful BIF calls           -> compiles under the window
%%        (send, ets:*, put/2, ...)        model (the 08 §S2 amendment)
%%   I  + lists:* higher-order calls    -> compiles if the fun is
%%                                        literal (P3 intrinsics)
%%   C  + coverage-blocked ops          -> blocked on op coverage
%%        (bs_*/maps/floats/try/receive)   (G-bin / G-map / phases),
%%                                        NOT on the deopt model
%%   D  non-tail Erlang calls that      -> needs demote-on-return
%%      aren't leaf-inlineable             coverage loss or general
%%      (incl. call_fun/apply)             inlining (post-G3)
%%
%% Severity order D > C > I > B > A; a function lands in the worst
%% bucket it touches. Non-tail local calls to leaf functions
%% (<= 24 real ops, pure v1 body, no calls) do NOT demote a
%% function: v1 inlines those (08 §2 case "local leaf").
%% Tail calls to other functions are loop exits: neutral.
%%
%% Conservative bias: classification is whole-function, so effects on
%% non-loop clauses (e.g. a done-clause that sends a result) still
%% demote the function. Real loop-body-only numbers would be >= these.
%%
%% Usage: effect_census.erl LABEL:EBIN_DIR [LABEL:EBIN_DIR ...]
%%        (an EBIN_DIR is searched recursively for .beam files)

-mode(compile).

main(Args) when Args =/= [] ->
    Corpora = [begin
                   [Label, Dir] = string:split(A, ":"),
                   {Label, Dir}
               end || A <- Args],
    Results = [census_corpus(L, D) || {L, D} <- Corpora],
    print_summary(Results),
    dump_unknowns();
main(_) ->
    io:format("usage: effect_census.erl LABEL:EBIN_DIR ...~n"),
    halt(1).

census_corpus(Label, Dir) ->
    Beams = filelib:fold_files(Dir, "\\.beam$", true,
                               fun(F, Acc) -> [F | Acc] end, []),
    Stats = lists:foldl(fun census_beam/2, new_stats(), Beams),
    {Label, length(Beams), Stats}.

new_stats() ->
    #{funcs => 0, loops => 0,
      a => 0, b => 0, i => 0, c => 0, d => 0,
      %% orthogonal C sub-reasons over loop funcs (a func may have several)
      c_bin => 0, c_map => 0, c_float => 0, c_exc => 0, c_recv => 0,
      %% D sub-reasons
      d_local => 0, d_remote => 0, d_fun => 0, d_body_rec => 0,
      rows => []}.

census_beam(File, Stats) ->
    case catch beam_disasm:file(File) of
        {beam_file, Mod, _, _, _, Funs} ->
            ByName = maps:from_list(
                       [{{N, A}, Code} || {function, N, A, _, Code} <- Funs]),
            lists:foldl(fun(F, S) -> census_fun(Mod, F, ByName, S) end,
                        Stats, Funs);
        _ ->
            Stats
    end.

census_fun(Mod, {function, Name, Arity, _Entry, Code}, ByName, Stats0) ->
    Stats1 = maps:update_with(funcs, fun(N) -> N + 1 end, Stats0),
    Self = {Mod, Name, Arity},
    case is_loop_fun(Code, Self) of
        false -> Stats1;
        true ->
            {Bucket, Flags} = classify(Code, Self, ByName),
            Stats2 = maps:update_with(loops, fun(N) -> N + 1 end, Stats1),
            Stats3 = maps:update_with(Bucket, fun(N) -> N + 1 end, Stats2),
            Stats4 = lists:foldl(fun(Fl, S) ->
                                     maps:update_with(Fl, fun(N) -> N + 1 end, S)
                                 end, Stats3, Flags),
            maps:update_with(rows,
                             fun(Rows) ->
                                 [{Mod, Name, Arity, Bucket, Flags} | Rows]
                             end, Stats4)
    end.

is_loop_fun(Code, Self) ->
    lists:any(fun({call_only, _, T}) -> T =:= Self;
                 ({call_last, _, T, _}) -> T =:= Self;
                 (_) -> false
              end, Code).

%% Classify a whole function. Returns {Bucket, Flags}.
classify(Code, Self, ByName) ->
    Marks = lists:usort(lists:flatmap(fun(I) -> mark(I, Self, ByName) end,
                                      Code)),
    Flags = [M || M <- Marks,
                  lists:member(M, [c_bin, c_map, c_float, c_exc, c_recv,
                                   d_local, d_remote, d_fun, d_body_rec])],
    Bucket =
        case {lists:any(fun is_d/1, Marks),
              lists:any(fun is_c/1, Marks),
              lists:member(i, Marks),
              lists:member(b, Marks)} of
            {true, _, _, _} -> d;
            {false, true, _, _} -> c;
            {false, false, true, _} -> i;
            {false, false, false, true} -> b;
            _ -> a
        end,
    {Bucket, Flags}.

is_d(M) -> lists:member(M, [d_local, d_remote, d_fun, d_body_rec]).
is_c(M) -> lists:member(M, [c_bin, c_map, c_float, c_exc, c_recv, c_other]).

%% mark/3: per-instruction markers ([] = v1-neutral).
mark({call_only, _, _}, _, _) -> [];   % back-edge or tail call out = loop exit
mark({call_last, _, _, _}, _, _) -> [];
mark({call_ext_only, _, _}, _, _) -> [];
mark({call_ext_last, _, _, _}, _, _) -> [];
mark({call, _, {M, N, A}}, {M, SN, SA} = _Self, ByName) ->
    case {N, A} =:= {SN, SA} of
        true -> [d_body_rec];                          % body recursion
        false ->
            case is_leaf_inlineable(maps:get({N, A}, ByName, undefined)) of
                true -> [];                            % v1 leaf inline
                false -> [d_local]
            end
    end;
mark({call, _, _}, _, _) -> [d_local];                 % unresolved local target
mark({call_ext, _, {extfunc, M, F, A}}, _, _) ->
    case {erlang:is_builtin(M, F, A), is_intrinsic(M, F, A)} of
        {true, _} ->
            case is_pure_bif(M, F, A) of
                true -> [];
                false -> [b]                           % effectful BIF: window boundary
            end;
        {false, true} -> [i];                          % lists:* higher-order
        {false, false} -> [d_remote]
    end;
mark({call_fun, _}, _, _) -> [d_fun];
mark({call_fun2, _, _, _}, _, _) -> [d_fun];
mark({apply, _}, _, _) -> [d_fun];
mark({apply_last, _, _}, _, _) -> [];                  % tail apply = loop exit
mark(send, _, _) -> [b];
mark(I, _, _) when is_tuple(I) ->
    mark_op(element(1, I), I);
mark(I, _, _) when is_atom(I) ->
    mark_op(I, I).

mark_op(Op, I) ->
    case op_class(Op) of
        v1 -> [];
        bin -> [c_bin];
        map -> [c_map];
        float -> [c_float];
        exc -> [c_exc];
        recv -> [c_recv];
        test -> mark_test(I);
        unknown -> note_unknown(Op), [c_other]
    end.

mark_test({test, T, _, _}) -> mark_test_name(T);
mark_test({test, T, _, _, _}) -> mark_test_name(T);
mark_test({test, T, _, _, _, _}) -> mark_test_name(T);
mark_test(_) -> [].

mark_test_name(T) ->
    case atom_to_list(T) of
        "bs_" ++ _ -> [c_bin];
        _ -> []                                        % is_*, test_arity, ...
    end.

op_class(Op) ->
    case Op of
        %% v1-neutral
        label -> v1; line -> v1; executable_line -> v1; func_info -> v1;
        int_code_end -> v1; move -> v1; swap -> v1; init_yregs -> v1;
        allocate -> v1; allocate_heap -> v1; allocate_zero -> v1;
        allocate_heap_zero -> v1; deallocate -> v1; trim -> v1;
        test_heap -> v1; jump -> v1; return -> v1;
        get_list -> v1; get_hd -> v1; get_tl -> v1;
        get_tuple_element -> v1; put_list -> v1; put_tuple2 -> v1;
        set_tuple_element -> v1; update_record -> v1;
        select_val -> v1; select_tuple_arity -> v1;
        badmatch -> v1; if_end -> v1; case_end -> v1; badrecord -> v1;
        make_fun3 -> v1;
        bif -> v1; gc_bif -> v1;                       % guard-context, pure
        test -> test;
        %% receive machinery
        loop_rec -> recv; loop_rec_end -> recv; remove_message -> recv;
        timeout -> recv; wait -> recv; wait_timeout -> recv;
        recv_marker_bind -> recv; recv_marker_clear -> recv;
        recv_marker_reserve -> recv; recv_marker_use -> recv;
        %% exceptions
        'try' -> exc; try_end -> exc; try_case -> exc; try_case_end -> exc;
        'catch' -> exc; catch_end -> exc; raise -> exc; raw_raise -> exc;
        build_stacktrace -> exc;
        %% floats
        fclearerror -> float; fcheckerror -> float; fmove -> float;
        fconv -> float; fadd -> float; fsub -> float; fmul -> float;
        fdiv -> float; fnegate -> float;
        %% maps
        put_map_assoc -> map; put_map_exact -> map;
        get_map_elements -> map; has_map_fields -> map;
        _ ->
            case atom_to_list(Op) of
                "bs_" ++ _ -> bin;
                _ -> unknown
            end
    end.

is_intrinsic(lists, F, A) ->
    lists:member({F, A}, [{all, 2}, {any, 2}, {foreach, 2}, {map, 2},
                          {flatmap, 2}, {filter, 2}, {foldl, 3},
                          {foldr, 3}, {mapfoldl, 3}, {mapfoldr, 3}]);
is_intrinsic(_, _, _) -> false.

is_pure_bif(M, F, A) ->
    erl_bifs:is_pure(M, F, A).

%% Leaf-inlineable: <= 24 real ops, v1-class body, no calls/effects.
is_leaf_inlineable(undefined) -> false;
is_leaf_inlineable(Code) ->
    Real = [I || I <- Code,
                 not lists:member(op_name(I), [label, line, func_info,
                                               executable_line])],
    length(Real) =< 24 andalso
        lists:all(fun(I) ->
                      Op = op_name(I),
                      Calls = [call, call_last, call_only, call_ext_last,
                               call_ext_only, call_fun, call_fun2, apply,
                               apply_last, send],
                      case {Op, I} of
                          %% pure-BIF call inside a leaf is window-legal
                          {call_ext, {call_ext, _, {extfunc, M, F, A}}} ->
                              erlang:is_builtin(M, F, A)
                                  andalso is_pure_bif(M, F, A);
                          _ ->
                              case lists:member(Op, Calls) of
                                  true -> false;
                                  false ->
                                      case op_class(Op) of
                                          v1 -> true;
                                          test -> mark_test(I) =:= [];
                                          _ -> false
                                      end
                              end
                      end
                  end, Real).

op_name(I) when is_tuple(I) -> element(1, I);
op_name(I) when is_atom(I) -> I.

%% ------------------------------------------------------------------

print_summary(Results) ->
    io:format("~n~-12s ~6s ~6s | ~5s ~5s ~5s ~5s ~5s | ~s~n",
              ["corpus", "funcs", "loops", "A", "B", "I", "C", "D",
               "A+B+I share of loops"]),
    io:format("~s~n", [lists:duplicate(95, $-)]),
    lists:foreach(fun({Label, _NBeams, S}) ->
        #{funcs := F, loops := L, a := A, b := B, i := I, c := C, d := D} = S,
        Share = case L of 0 -> 0.0; _ -> 100 * (A + B + I) / L end,
        io:format("~-12s ~6b ~6b | ~5b ~5b ~5b ~5b ~5b | ~5.1f%~n",
                  [Label, F, L, A, B, I, C, D, Share])
    end, Results),
    io:format("~nC sub-reasons (loop funcs touching each; overlapping):~n"),
    io:format("~-12s ~6s ~6s ~6s ~6s ~6s | ~6s ~6s ~6s ~6s~n",
              ["corpus", "bin", "map", "float", "exc", "recv",
               "d_loc", "d_rem", "d_fun", "d_brec"]),
    lists:foreach(fun({Label, _, S}) ->
        io:format("~-12s ~6b ~6b ~6b ~6b ~6b | ~6b ~6b ~6b ~6b~n",
                  [Label, maps:get(c_bin, S), maps:get(c_map, S),
                   maps:get(c_float, S), maps:get(c_exc, S),
                   maps:get(c_recv, S), maps:get(d_local, S),
                   maps:get(d_remote, S), maps:get(d_fun, S),
                   maps:get(d_body_rec, S)])
    end, Results),
    %% per-function detail for later analysis
    Out = "/tmp/effect_census_detail.tsv",
    {ok, Fd} = file:open(Out, [write]),
    lists:foreach(fun({Label, _, S}) ->
        lists:foreach(fun({M, N, A, Bk, Fl}) ->
            io:format(Fd, "~s\t~s\t~s/~b\t~s\t~p~n",
                      [Label, M, N, A, Bk, Fl])
        end, maps:get(rows, S))
    end, Results),
    file:close(Fd),
    io:format("~ndetail written to ~s~n", [Out]).

note_unknown(Op) ->
    Tab = case get(unknown_ops) of undefined -> #{}; T -> T end,
    put(unknown_ops, maps:update_with(Op, fun(N) -> N + 1 end, 1, Tab)).

dump_unknowns() ->
    case get(unknown_ops) of
        undefined -> ok;
        T when map_size(T) =:= 0 -> ok;
        T ->
            io:format("~nUNKNOWN ops (classified C_other; refine table):~n~p~n",
                      [lists:reverse(lists:keysort(2, maps:to_list(T)))])
    end.
