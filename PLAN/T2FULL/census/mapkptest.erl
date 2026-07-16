%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%% S1b.3a pctab test: eligible loops with 1-, 2-, 3-pair map reads. Each
%% get_map_elements must produce exactly one EFFECT pctab entry with a
%% RESOLVED beam_idx (not 'unknown') and no T2_DEBUG count-mismatch.
-module(mapkptest).
-export([one/3, one_int/3, two/3, three/3]).

one(0, _M, A) -> A;
one(K, M, A) -> #{a := V} = M, one(K - 1, M, A bxor V).

one_int(0, _M, A) -> A;
one_int(K, M, A) -> #{1 := V} = M, one_int(K - 1, M, A bxor V).

two(0, _M, A) -> A;
two(K, M, A) -> #{a := X, b := Y} = M, two(K - 1, M, A bxor X bxor Y).

three(0, _M, A) -> A;
three(K, M, A) -> #{a := X, b := Y, c := Z} = M, three(K - 1, M, A bxor X bxor Y bxor Z).
