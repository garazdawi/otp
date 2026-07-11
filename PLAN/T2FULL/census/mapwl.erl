%% SPDX-License-Identifier: Apache-2.0
%% Copyright Ericsson AB 2026. All Rights Reserved.
%%
%% PLAN/T2FULL/19 §2 S0 corpus — the map/term-heavy workload memo 17 §4
%% calls "the one place I would not pre-judge." Three map-dominated
%% shapes: a term->map decoder, a map-lookup router (map-of-maps config),
%% and a record->map transform. Own-time concentrates in get_map_elements
%% (map matching in heads), maps:get/put/merge/fold, and is_map guards —
%% exactly the class with a work-removal story (fixed-offset loads under a
%% monomorphic shape). All inputs deterministic.
-module(mapwl).
-export([setup/0, work/1]).

-record(rec, {id, name, kind, weight, tags, meta}).

setup() ->
    Config = build_config(64),
    Reqs = [mk_request(N) || N <- lists:seq(1, 30000)],
    Recs = [mk_rec(N) || N <- lists:seq(1, 30000)],
    {Config, Reqs, Recs}.

work({Config, Reqs, Recs}) ->
    A = route_loop(Reqs, Config, 0),
    B = decode_loop(Reqs, 0),
    C = transform_loop(Recs, 0),
    A + B + C.

%% ---- map-lookup router: hot tail loop over requests -----------------

route_loop([], _Config, Acc) ->
    Acc;
route_loop([Req | T], Config, Acc) ->
    route_loop(T, Config, Acc + route(Req, Config)).

%% Match the request map in the head (get_map_elements), look the handler
%% up in the map-of-maps config (maps:get), score it (maps:fold).
route(#{service := S, method := M, weight := W} = Req, Config) ->
    Svc = maps:get(S, Config, #{}),
    case maps:get(M, Svc, undefined) of
        undefined ->
            W;
        Handler when is_map(Handler) ->
            #{cost := Cost, prio := Prio} = Handler,
            Extra = maps:fold(fun(_K, V, Sum) when is_integer(V) -> Sum + V;
                                 (_K, _V, Sum) -> Sum
                              end, 0, Req),
            (Cost * Prio) + W + Extra
    end.

%% ---- term->map decoder: proplist -> normalized map ------------------

decode_loop([], Acc) ->
    Acc;
decode_loop([Req | T], Acc) ->
    M = decode(maps:to_list(Req)),
    decode_loop(T, Acc + map_size(M)).

%% Fold a proplist into a map, then merge over a template (defaults).
decode(PL) ->
    Base = decode_pairs(PL, #{}),
    Tmpl = #{version => 1, service => none, method => none, weight => 0},
    Merged = maps:merge(Tmpl, Base),
    #{service := Svc} = Merged,
    Merged#{normalized => is_atom(Svc)}.

decode_pairs([], M) ->
    M;
decode_pairs([{K, V} | T], M) ->
    decode_pairs(T, maps:put(K, V, M)).

%% ---- record->map transform: hot tail loop over records --------------

transform_loop([], Acc) ->
    Acc;
transform_loop([R | T], Acc) ->
    M = rec_to_map(R),
    transform_loop(T, Acc + map_size(M)).

rec_to_map(#rec{id = Id, name = Nm, kind = K, weight = W, tags = Ts, meta = Meta}) ->
    Base = #{id => Id, name => Nm, kind => K, weight => W, tags => Ts},
    maps:merge(Base, Meta).

%% ---- deterministic corpus builders ----------------------------------

build_config(N) ->
    Svcs = [{svc_atom(I),
             maps:from_list([{meth_atom(J),
                              #{cost => (I * 3 + J) rem 17,
                                prio => (J rem 5) + 1}}
                             || J <- lists:seq(1, 8)])}
            || I <- lists:seq(1, N)],
    maps:from_list(Svcs).

mk_request(N) ->
    #{service => svc_atom((N rem 64) + 1),
      method => meth_atom((N rem 8) + 1),
      weight => N rem 100,
      user => N,
      region => (N rem 4)}.

mk_rec(N) ->
    #rec{id = N,
         name = N * 2,
         kind = (N rem 3),
         weight = N rem 50,
         tags = [N rem 7, N rem 11],
         meta = #{created => N, region => (N rem 4), score => (N * 7) rem 13}}.

svc_atom(I) -> list_to_atom("svc_" ++ integer_to_list(I)).
meth_atom(J) -> list_to_atom("m_" ++ integer_to_list(J)).
