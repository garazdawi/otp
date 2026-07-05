%% sinkdrv.erl — weighting + aggregation for M0.2 (sibling to elimdrv.erl).
%% Reads {[#asite{} tuples], DirOf} written by run_sink.escript and prints:
%%   * the whole-corpus + stdlib/kernel structural census (tier word% / site%);
%%   * per-module views for the stdlib leg (maps/json/lists);
%%   * the GALLOC-volume-weighted stdlib leg (named functions weighted by the
%%     per-function heap-word shares of GALLOC_RESULTS.md);
%%   * the P5 Amdahl reading.
%% Site tuple layout: {asite, MFA, Op, Cat, Words, Tier, Why}.
-module(sinkdrv).
-export([main/1, report/1, mfa_line/2, cat_line/2, load/1]).

%% aggregate accumulator
-record(agg, {n_t1=0, n_t2=0, n_esc=0,           % site counts by tier
              we_t1=0, we_t2=0, we_esc=0,        % EXACT words (tuple+cons+record)
              wa_t1=0, wa_t2=0, wa_esc=0,        % ALL approx words (+map+fun)
              %% category site counts (all tiers)
              c_tuple=0, c_cons=0, c_record=0, c_map=0, c_fun=0, c_bin=0,
              %% bin/fun/map by tier (site counts; words unknown/approx)
              bin_t1=0, bin_t2=0, bin_esc=0,
              fun_t1=0, fun_t2=0, fun_esc=0,
              map_t1=0, map_t2=0, map_esc=0}).

main([Path]) -> report(Path), halt(0);
main(_) -> io:format("usage: sinkdrv <dump.eterm>~n"), halt(1).

load(Path) ->
    {ok, B} = file:read_file(Path),
    binary_to_term(B).

report(Path) ->
    {Sites, DirOf} = load(Path),
    io:format("~n========================================================~n"),
    io:format(" M0.2 SINKABLE-ALLOCATION — results~n"),
    io:format("========================================================~n"),
    io:format("~ntotal allocation sites (post-beam_ssa_opt): ~p~n", [length(Sites)]),

    io:format("~n### WHOLE-CORPUS CENSUS (structural; all ~p modules)~n", [maps:size(DirOf)]),
    print_agg("whole corpus", agg(Sites)),

    io:format("~n### STDLIB / KERNEL CENSUS (structural)~n"),
    Std = [S || S <- Sites, maps:get(mod(S), DirOf, undefined) =:= stdlib],
    Ker = [S || S <- Sites, maps:get(mod(S), DirOf, undefined) =:= kernel],
    print_agg("stdlib/src", agg(Std)),
    print_agg("kernel/src", agg(Ker)),

    io:format("~n### STDLIB LEG — key modules (per-module tier word%)~n"),
    [cat_line(atom_to_list(M) ++ ".erl", [S || S <- Sites, mod(S) =:= M])
     || M <- [maps, json, lists, unicode, string, binary, base64, proplists]],

    io:format("~n### STDLIB LEG — named GALLOC functions (per-function)~n"),
    [mfa_line(Label, [S || S <- Sites, S == S, mfa(S) =:= MFA])
     || {Label, MFA} <- galloc_stdlib_mfas()],

    io:format("~n### GALLOC-VOLUME-WEIGHTED stdlib leg~n"),
    weighted_leg(Sites),

    io:format("~n(word% within a group is over EXACT words = tuple+cons+record;~n"
              " map/fun are approx and shown as a second 'all-approx' line; bin by site)~n"),
    ok.

%% ---- per-MFA one-liner ----
mfa_line(Label, Sites) ->
    A = agg(Sites),
    {We1, We2, WeE} = {A#agg.we_t1, A#agg.we_t2, A#agg.we_esc},
    WeT = We1 + We2 + WeE,
    io:format("  ~-34s sites t1/t2/esc=~p/~p/~p  exactWORD% t1=~s t2=~s esc=~s  (tup~p cons~p rec~p map~p fun~p bin~p)~n",
              [Label, A#agg.n_t1, A#agg.n_t2, A#agg.n_esc,
               pct(We1, WeT), pct(We2, WeT), pct(WeE, WeT),
               A#agg.c_tuple, A#agg.c_cons, A#agg.c_record, A#agg.c_map, A#agg.c_fun, A#agg.c_bin]).

%% ---- per-module tier word line ----
cat_line(Label, Sites) ->
    A = agg(Sites),
    WeT = A#agg.we_t1 + A#agg.we_t2 + A#agg.we_esc,
    WaT = A#agg.wa_t1 + A#agg.wa_t2 + A#agg.wa_esc,
    io:format("  ~-14s  n=~4w  exactWORD% t1/t2/esc=~s/~s/~s  allApproxWORD% t1/t2/esc=~s/~s/~s  bin(t1/t2/esc)=~p/~p/~p~n",
              [Label, A#agg.n_t1 + A#agg.n_t2 + A#agg.n_esc,
               pct(A#agg.we_t1, WeT), pct(A#agg.we_t2, WeT), pct(A#agg.we_esc, WeT),
               pct(A#agg.wa_t1, WaT), pct(A#agg.wa_t2, WaT), pct(A#agg.wa_esc, WaT),
               A#agg.bin_t1, A#agg.bin_t2, A#agg.bin_esc]).

%% ---- GALLOC-weighted stdlib leg ----
%% Weight each classifiable function's within-function sinkable share by its
%% GALLOC per-function heap-word share (thin-Bandit table). maps:from_list is
%% a BIF (no SSA site) => not addressable by allocation sinking; counted in a
%% dedicated 'BIF-internal' bucket. Jason/String are Elixir (own legs); here we
%% substitute the stdlib analog (json.erl) where noted.
weighted_leg(Sites) ->
    %% {Label, Weight%, kind}  kind = {mfa,MFA} | {mod,Mod} | bif
    Rows = [{"maps:from_list/1",           46.05, bif},
            {"json (Jason.Decoder analog)", 25.0, {mod, json}},   % obj+key+arr+val
            {"json (Jason.Encode analog)",   5.0, {mod_enc, json}},
            {"lists:reverse/1",             1.48, {mfa, {lists, reverse, 1}}}],
    {SW, ST1, ST2} =
        lists:foldl(
          fun({Label, W, Kind}, {AccW, AccT1, AccT2}) ->
                  {ShT1, ShT2} = kind_share(Kind, Sites),
                  io:format("  ~-30s w=~5.2f%%  within-fn sinkable: t1=~s t2=~s (t1+t2=~s)~n",
                            [Label, W, pct2(ShT1), pct2(ShT2), pct2(ShT1 + ShT2)]),
                  {AccW + W, AccT1 + W * ShT1, AccT2 + W * ShT2}
          end, {0, 0.0, 0.0}, Rows),
    io:format("  ------------------------------------------------------------~n"),
    io:format("  covered GALLOC weight: ~.1f%% of thin-Bandit volume~n", [SW]),
    io:format("  volume-weighted (over COVERED weight): t1=~s t2=~s t1+t2=~s~n",
              [pct2(safediv(ST1, SW)), pct2(safediv(ST2, SW)), pct2(safediv(ST1 + ST2, SW))]),
    io:format("  volume-weighted (over TOTAL 100%% heap): t1=~s t2=~s t1+t2=~s~n",
              [pct2(ST1 / 100), pct2(ST2 / 100), pct2((ST1 + ST2) / 100)]).

kind_share(bif, _Sites) -> {0.0, 0.0};   % BIF-internal: not sinkable by P5
kind_share({mfa, MFA}, Sites) ->
    exact_share([S || S <- Sites, mfa(S) =:= MFA]);
kind_share({mod, Mod}, Sites) ->
    %% decoder-analog: whole json module, exact-word tier share
    exact_share([S || S <- Sites, mod(S) =:= Mod]);
kind_share({mod_enc, Mod}, Sites) ->
    exact_share([S || S <- Sites, mod(S) =:= Mod]).

exact_share(Sites) ->
    A = agg(Sites),
    T = A#agg.we_t1 + A#agg.we_t2 + A#agg.we_esc,
    case T of
        0 -> {0.0, 0.0};
        _ -> {A#agg.we_t1 / T, A#agg.we_t2 / T}
    end.

galloc_stdlib_mfas() ->
    [{"lists:reverse/1", {lists, reverse, 1}},
     {"lists:map/2",     {lists, map, 2}},
     {"lists:keyfind/3", {lists, keyfind, 3}},
     {"maps:from_list/1",{maps, from_list, 1}}].

%% ---- aggregation ----
agg(Sites) ->
    lists:foldl(fun add_site/2, #agg{}, Sites).

add_site({asite, _MFA, _Op, Cat, Words, Tier, _Why}, A0) ->
    A1 = bump_cat(Cat, A0),
    A2 = bump_tier_site(Tier, A1),
    A3 = bump_words(Cat, Words, Tier, A2),
    bump_cattier(Cat, Tier, A3).

bump_cat(tuple, A) -> A#agg{c_tuple=A#agg.c_tuple+1};
bump_cat(cons, A) -> A#agg{c_cons=A#agg.c_cons+1};
bump_cat(record, A) -> A#agg{c_record=A#agg.c_record+1};
bump_cat(map, A) -> A#agg{c_map=A#agg.c_map+1};
bump_cat('fun', A) -> A#agg{c_fun=A#agg.c_fun+1};
bump_cat(bin, A) -> A#agg{c_bin=A#agg.c_bin+1}.

bump_tier_site(t1, A) -> A#agg{n_t1=A#agg.n_t1+1};
bump_tier_site(t2, A) -> A#agg{n_t2=A#agg.n_t2+1};
bump_tier_site(esc, A) -> A#agg{n_esc=A#agg.n_esc+1}.

%% EXACT words: tuple/cons/record with integer words.
%% ALL-approx words: additionally include map ({approx,N}) and fun (integer).
bump_words(Cat, Words, Tier, A) when Cat =:= tuple; Cat =:= cons; Cat =:= record ->
    case is_integer(Words) of
        true -> add_exact(Tier, Words, add_approx(Tier, Words, A));
        false -> A
    end;
bump_words(map, {approx, N}, Tier, A) -> add_approx(Tier, N, A);
bump_words('fun', N, Tier, A) when is_integer(N) -> add_approx(Tier, N, A);
bump_words(_, _, _, A) -> A.

add_exact(t1, W, A) -> A#agg{we_t1=A#agg.we_t1+W};
add_exact(t2, W, A) -> A#agg{we_t2=A#agg.we_t2+W};
add_exact(esc, W, A) -> A#agg{we_esc=A#agg.we_esc+W}.

add_approx(t1, W, A) -> A#agg{wa_t1=A#agg.wa_t1+W};
add_approx(t2, W, A) -> A#agg{wa_t2=A#agg.wa_t2+W};
add_approx(esc, W, A) -> A#agg{wa_esc=A#agg.wa_esc+W}.

bump_cattier(bin, T, A) -> case T of t1->A#agg{bin_t1=A#agg.bin_t1+1}; t2->A#agg{bin_t2=A#agg.bin_t2+1}; esc->A#agg{bin_esc=A#agg.bin_esc+1} end;
bump_cattier('fun', T, A) -> case T of t1->A#agg{fun_t1=A#agg.fun_t1+1}; t2->A#agg{fun_t2=A#agg.fun_t2+1}; esc->A#agg{fun_esc=A#agg.fun_esc+1} end;
bump_cattier(map, T, A) -> case T of t1->A#agg{map_t1=A#agg.map_t1+1}; t2->A#agg{map_t2=A#agg.map_t2+1}; esc->A#agg{map_esc=A#agg.map_esc+1} end;
bump_cattier(_, _, A) -> A.

print_agg(Label, A) ->
    N = A#agg.n_t1 + A#agg.n_t2 + A#agg.n_esc,
    WeT = A#agg.we_t1 + A#agg.we_t2 + A#agg.we_esc,
    WaT = A#agg.wa_t1 + A#agg.wa_t2 + A#agg.wa_esc,
    io:format("~n  ~s: ~p sites  (t1=~p t2=~p esc=~p)~n", [Label, N, A#agg.n_t1, A#agg.n_t2, A#agg.n_esc]),
    io:format("    site%%      t1=~s  t2=~s  esc=~s~n",
              [pct(A#agg.n_t1, N), pct(A#agg.n_t2, N), pct(A#agg.n_esc, N)]),
    io:format("    exactWORD%% t1=~s  t2=~s  esc=~s   (~p words: tuple+cons+record)~n",
              [pct(A#agg.we_t1, WeT), pct(A#agg.we_t2, WeT), pct(A#agg.we_esc, WeT), WeT]),
    io:format("    allApprox%% t1=~s  t2=~s  esc=~s   (~p words: +map+fun approx)~n",
              [pct(A#agg.wa_t1, WaT), pct(A#agg.wa_t2, WaT), pct(A#agg.wa_esc, WaT), WaT]),
    io:format("    categories: tuple=~p cons=~p record=~p map=~p fun=~p bin=~p~n",
              [A#agg.c_tuple, A#agg.c_cons, A#agg.c_record, A#agg.c_map, A#agg.c_fun, A#agg.c_bin]),
    io:format("    bin  tier: t1=~p t2=~p esc=~p~n", [A#agg.bin_t1, A#agg.bin_t2, A#agg.bin_esc]),
    io:format("    fun  tier: t1=~p t2=~p esc=~p~n", [A#agg.fun_t1, A#agg.fun_t2, A#agg.fun_esc]),
    io:format("    map  tier: t1=~p t2=~p esc=~p~n", [A#agg.map_t1, A#agg.map_t2, A#agg.map_esc]).

%% ---- misc ----
mod({asite, {M, _, _}, _, _, _, _, _}) -> M.
mfa({asite, MFA, _, _, _, _, _}) -> MFA.

pct(_, 0) -> "  n/a";
pct(N, D) -> lists:flatten(io_lib:format("~5.1f%", [100 * N / D])).
pct2(F) -> lists:flatten(io_lib:format("~4.1f%", [100 * F])).
safediv(_, 0) -> 0.0;
safediv(N, D) -> N / D.
