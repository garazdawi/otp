%% elimdrv.erl — weighting + aggregation for M0.1.
%% Reads fstats.eterm ({[#fstat{}], DirOf}) written by run_scan.escript
%% and prints the two weighted legs (dialyzer, json) plus the unweighted
%% stdlib/kernel census. Cycle weights come from PROFILE_RESULTS.md.
-module(elimdrv).
-export([main/1, report/0, report/1]).

-record(fstat, {mfa, nsites=0, opaque=0, guard=0, guardtests=0, const=0,
                consteff=0, litfun=0, cdcon=0, loop=0, coreelim=0, precise=0,
                none=0, elim=0,
                rsites=0, rguard=0, rconsteff=0, rlitfun=0, rcdcon=0,
                rloop=0, rcore=0, rprecise=0, rnone=0, relim=0}).

%% Aggregate accumulator (same fields, summed).
main([Path]) -> report(Path), halt(0);
main(_) -> report(), halt(0).

%% default dump path (session scratch); pass an explicit path to report/1.
report() ->
    report("/private/tmp/claude-502/-Users-lukas-code-otp-ideas/"
           "f1a0ad52-163c-48d3-84f7-612323edd05c/scratchpad/m01-elimscan/fstats.eterm").

report(Path) ->
    {Stats, DirOf} = read(Path),
    Map = maps:from_list([{S#fstat.mfa, S} || S <- Stats]),
    io:format("~n========================================================~n"),
    io:format(" M0.1 ELIMINABILITY — results~n"),
    io:format("========================================================~n"),

    %% ---------- DIALYZER LEG (cycle-weighted) ----------
    io:format("~n### DIALYZER LEG (cycle-weighted; weights from PROFILE_RESULTS.md)~n"),
    FamA = [{erl_types,is_limited,2},{erl_types,are_all_limited,2},
            {erl_types,t_has_var,1},{erl_types,t_has_var_list,1}],
    FamB = [{erl_types,oc_mark,3} | lc_helpers_of(oc_mark, Stats)],
    AggA = agg([maps:get(M,Map,#fstat{mfa=M}) || M <- FamA]),
    AggB = agg([maps:get(M,Map,#fstat{mfa=M}) || M <- FamB]),
    print_group("Family A: is_limited+are_all_limited+t_has_var* (12% busy)", AggA),
    print_group("Family B: oc_mark + its comprehensions      ( 9% busy)", AggB),
    %% cycle-weighted combination over the named core (weights 12 and 9)
    WCoreElim = wmean([{12, frac(AggA#fstat.coreelim, AggA#fstat.nsites)},
                       {9,  frac(AggB#fstat.coreelim, AggB#fstat.nsites)}]),
    WElim     = wmean([{12, frac(AggA#fstat.elim, AggA#fstat.nsites)},
                       {9,  frac(AggB#fstat.elim, AggB#fstat.nsites)}]),
    io:format("  >> cycle-weighted over the 21% named hot core:~n"),
    io:format("     Pillar-2 core-eliminable sites = ~.1f%% ; incl. loop-unlock = ~.1f%%~n",
              [WCoreElim*100, WElim*100]),

    %% supporting: whole erl_types module (proxy, structural)
    ETmod = agg([S || S <- Stats, mod(S) =:= erl_types]),
    print_group("erl_types WHOLE MODULE (structural proxy, NOT cycle-weighted)", ETmod),
    %% supporting: comprehension-heavy structural view across erl_types
    AllLC = agg([S || S <- Stats, mod(S)=:=erl_types, is_lc(S#fstat.mfa)]),
    print_group("erl_types comprehension helpers (structural)", AllLC),
    %% supporting: dialyzer_* modules aggregate (the <=2.5% tail — structural)
    DiaTail = agg([S || S <- Stats, maps:get(mod(S), DirOf, undefined)=:=dialyzer]),
    print_group("dialyzer app whole (all dialyzer/src modules; <=2.5%/fn TAIL, structural)", DiaTail),

    %% ---------- JSON LEG ----------
    io:format("~n### STDLIB JSON LEG (json.erl, uniform)~n"),
    JsonMod = agg([S || S <- Stats, mod(S) =:= json]),
    print_group("json.erl whole module", JsonMod),
    JsonEqual = equal_weighted([S || S <- Stats, mod(S) =:= json]),
    io:format("  >> per-function-equal weighting (alt): core-elim=~.1f%% elim=~.1f%%~n",
              [element(1,JsonEqual)*100, element(2,JsonEqual)*100]),

    %% ---------- WHOLE-CORPUS CENSUS ----------
    io:format("~n### UNWEIGHTED CENSUS (structural shape)~n"),
    Std = agg([S || S <- Stats, maps:get(mod(S), DirOf, undefined) =:= stdlib]),
    Ker = agg([S || S <- Stats, maps:get(mod(S), DirOf, undefined) =:= kernel]),
    print_group("stdlib/src (all modules)", Std),
    print_group("kernel/src (all modules)", Ker),
    io:format("~n(fields are % of RESOLVABLE sites unless noted; opaque shown separately)~n"),
    ok.

%% ---- helpers ----
read(Path) ->
    {ok, B} = file:read_file(Path),
    binary_to_term(B).

mod(#fstat{mfa={M,_,_}}) -> M.

is_lc({_M,F,_A}) when is_atom(F) ->
    L = atom_to_list(F),
    string:find(L,"-lc$") =/= nomatch orelse string:find(L,"lc^") =/= nomatch
        orelse string:find(L,"-lbc$") =/= nomatch;
is_lc(_) -> false.

lc_helpers_of(Fun, Stats) ->
    Pat = "-" ++ atom_to_list(Fun) ++ "/",
    [S#fstat.mfa || S <- Stats, mod(S)=:=erl_types, is_lc(S#fstat.mfa),
                    string:find(atom_to_list(element(2,S#fstat.mfa)), Pat) =/= nomatch].

agg(Ss) ->
    lists:foldl(
      fun(#fstat{}=S, A) ->
              A#fstat{nsites=A#fstat.nsites+S#fstat.nsites,
                      opaque=A#fstat.opaque+S#fstat.opaque,
                      guard=A#fstat.guard+S#fstat.guard,
                      guardtests=A#fstat.guardtests+S#fstat.guardtests,
                      const=A#fstat.const+S#fstat.const,
                      consteff=A#fstat.consteff+S#fstat.consteff,
                      litfun=A#fstat.litfun+S#fstat.litfun,
                      cdcon=A#fstat.cdcon+S#fstat.cdcon,
                      loop=A#fstat.loop+S#fstat.loop,
                      coreelim=A#fstat.coreelim+S#fstat.coreelim,
                      precise=A#fstat.precise+S#fstat.precise,
                      none=A#fstat.none+S#fstat.none,
                      elim=A#fstat.elim+S#fstat.elim,
                      rsites=A#fstat.rsites+S#fstat.rsites,
                      rguard=A#fstat.rguard+S#fstat.rguard,
                      rconsteff=A#fstat.rconsteff+S#fstat.rconsteff,
                      rlitfun=A#fstat.rlitfun+S#fstat.rlitfun,
                      rcdcon=A#fstat.rcdcon+S#fstat.rcdcon,
                      rloop=A#fstat.rloop+S#fstat.rloop,
                      rcore=A#fstat.rcore+S#fstat.rcore,
                      rprecise=A#fstat.rprecise+S#fstat.rprecise,
                      rnone=A#fstat.rnone+S#fstat.rnone,
                      relim=A#fstat.relim+S#fstat.relim}
      end, #fstat{mfa=agg}, Ss).

frac(_, 0) -> 0.0;
frac(N, D) -> N / D.

wmean(Pairs) ->
    W = lists:sum([Wt || {Wt,_} <- Pairs]),
    case W of 0 -> 0.0; _ -> lists:sum([Wt*V || {Wt,V} <- Pairs]) / W end.

%% per-function-equal weighting: mean over functions of (coreelim/nsites)
equal_weighted(Ss) ->
    Fs = [S || S <- Ss, S#fstat.nsites > 0],
    case Fs of
        [] -> {0.0,0.0};
        _ ->
            N = length(Fs),
            C = lists:sum([frac(S#fstat.coreelim,S#fstat.nsites) || S <- Fs])/N,
            E = lists:sum([frac(S#fstat.elim,S#fstat.nsites) || S <- Fs])/N,
            {C,E}
    end.

print_group(Title, #fstat{}=A) ->
    Tot = A#fstat.nsites + A#fstat.opaque,
    N = A#fstat.nsites,
    io:format("~n  ~s~n", [Title]),
    io:format("    total sites=~p  resolvable=~p  opaque=~p  (resolution ~.1f%%)~n",
              [Tot, N, A#fstat.opaque, 100*frac(Tot-A#fstat.opaque,Tot)]),
    io:format("    -- as %% of resolvable sites --~n"),
    io:format("      guard-subsumable   ~5.1f%%   (~p sites, ~p subsumed tests)~n",
              [100*frac(A#fstat.guard,N), A#fstat.guard, A#fstat.guardtests]),
    io:format("      constant-args LIB  ~5.1f%%   (~p sites; any literal, upper bound)~n",
              [100*frac(A#fstat.const,N), A#fstat.const]),
    io:format("      constant-args EFF  ~5.1f%%   (~p sites; literal folds a callee branch)~n",
              [100*frac(A#fstat.consteff,N), A#fstat.consteff]),
    io:format("      literal-FUN        ~5.1f%%   (~p sites; collapses call_fun)~n",
              [100*frac(A#fstat.litfun,N), A#fstat.litfun]),
    io:format("      construct/deconstr ~5.1f%%   (~p sites)~n",
              [100*frac(A#fstat.cdcon,N), A#fstat.cdcon]),
    io:format("      loop-unlock        ~5.1f%%   (~p sites)~n",
              [100*frac(A#fstat.loop,N), A#fstat.loop]),
    io:format("    ------------------------------------------~n"),
    io:format("      PRECISE-elim       ~5.1f%%   (guard|litfun|cdcon; high-confidence floor)~n",
              [100*frac(A#fstat.precise,N)]),
    io:format("      CORE-eliminable    ~5.1f%%   (guard|constEFF|litfun|cdcon; Pillar-2 pool)~n",
              [100*frac(A#fstat.coreelim,N)]),
    io:format("      ANY-eliminable     ~5.1f%%   (core|loop-unlock)~n",
              [100*frac(A#fstat.elim,N)]),
    io:format("      none (G3-2 shape)  ~5.1f%%~n", [100*frac(A#fstat.none,N)]),
    Dom = dominant(A),
    io:format("    dominant Pillar-2 category: ~p  (loop-unlock reported separately)~n", [Dom]),
    %% cross-module (remote) sub-view — the actual P3 pool
    R = A#fstat.rsites,
    io:format("    -- REMOTE (cross-module) sites only: ~p of ~p resolvable --~n", [R, N]),
    io:format("       r-guard=~.1f%% r-constEFF=~.1f%% r-litfun=~.1f%% r-cdcon=~.1f%% r-loop=~.1f%%~n",
              [100*frac(A#fstat.rguard,R),100*frac(A#fstat.rconsteff,R),
               100*frac(A#fstat.rlitfun,R),100*frac(A#fstat.rcdcon,R),
               100*frac(A#fstat.rloop,R)]),
    io:format("       r-PRECISE=~.1f%%  r-CORE=~.1f%%  r-ANY=~.1f%%  r-none=~.1f%%~n",
              [100*frac(A#fstat.rprecise,R),100*frac(A#fstat.rcore,R),
               100*frac(A#fstat.relim,R),100*frac(A#fstat.rnone,R)]).

%% Dominant among the Pillar-2 CORE categories (loop-unlock excluded — it
%% is a Pillar-1 enabler and empirically null on the G3-2 family).
dominant(#fstat{guard=G,consteff=C,litfun=L,cdcon=D}) ->
    case lists:reverse(lists:sort(
           [{G,'guard-subsumable'},{C,'constant-args(eff)'},
            {L,'literal-fun'},{D,'construct/deconstruct'}])) of
        [{0,_}|_] -> none;
        [{_,Top}|_] -> Top
    end.
