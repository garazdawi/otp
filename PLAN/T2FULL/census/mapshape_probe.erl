%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% S1b.2 thesis validation (PLAN/T2FULL/census/map_monomorphic_design.md).
%% The "no de-dup" decision rests on one claim: maps produced by
%% shape-preserving update of a literal-derived base SHARE the base's
%% keys-tuple pointer, while independently-built same-shape maps do NOT.
%% If true, pointer-identity captures the struct pattern without interning.
%% Uses the {map_keys_ptr, Map} debug probe (keys-tuple pointer as integer).
-module(mapshape_probe).
-export([go/0]).

kp(M) -> erts_debug:get_internal_state({map_keys_ptr, M}).

%% "struct default" literal, as an Elixir %Struct{}'s __struct__ would be.
base() -> #{a => 0, b => 0, c => 0}.
another_literal() -> #{a => 0, b => 0, c => 0}.
constructed(A, B, C) -> (base())#{a := A, b := B, c := C}.

share(P, P) -> "SHARES base keys";
share(_, _) -> "distinct".

go() ->
    erts_debug:set_internal_state(available_internal_state, true),
    B = base(),
    KB = kp(B),
    io:format("base literal keys ptr: ~w~n", [KB]),

    Rows =
        [{"update a:=1", kp(B#{a := 1})},
         {"update b:=2", kp(B#{b := 2})},
         {"double update a,c", kp((B#{a := 7})#{c := 9})},
         {"maps:update(a,5)", kp(maps:update(a, 5, B))},
         {"maps:put existing b", kp(maps:put(b, 6, B))},
         {"constructor fn", kp(constructed(1, 2, 3))},
         {"constructor fn #2", kp(constructed(9, 8, 7))},
         {"2nd literal same fn", kp(#{a => 0, b => 0, c => 0})},
         {"literal in other fn", kp(another_literal())},
         {"from_list #1", kp(maps:from_list([{a, 1}, {b, 2}, {c, 3}]))},
         {"from_list #2", kp(maps:from_list([{a, 9}, {b, 8}, {c, 7}]))},
         {"put NEW key d (reshape)", kp(B#{d => 4})}],
    [io:format("  ~-26s ~-16w ~s~n", [N, P, share(P, KB)]) || {N, P} <- Rows],

    %% cross-check: do two independent from_list builds share each other?
    I1 = kp(maps:from_list([{a, 1}, {b, 2}, {c, 3}])),
    I2 = kp(maps:from_list([{a, 1}, {b, 2}, {c, 3}])),
    io:format("~ntwo independent from_list of identical content: ~s~n",
              [share(I1, I2)]),
    halt().
