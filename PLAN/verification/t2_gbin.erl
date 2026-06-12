%% T2 G-bin experiment — benchmark + correctness harness for the
%% stdlib json:decode/1 scan-loop specialization.
%%
%%   t2_gbin:bench(Dir, Reps) -> decode timing per corpus file
%%   t2_gbin:check(Dir)       -> decode-result hash (must match across modes)
%%   t2_gbin:hot(Dir)         -> cprof: hottest json.erl functions per file
-module(t2_gbin).
-export([bench/2, check/1, hot/1]).

files() -> ["twitter.json", "citm_catalog.json", "canada.json"].

small() ->
    %% Typical API response shape, ~1.5 KB.
    iolist_to_binary(
      ["{\"items\":[",
       lists:join($,,
                  [io_lib:format(
                     "{\"id\":~b,\"name\":\"user~b\",\"score\":~b.5,"
                     "\"tags\":[\"a\",\"b\"],\"active\":true}",
                     [I, I, I * 7]) || I <- lists:seq(1, 10)]),
       "],\"next\":null}"]).

corpus(Dir) ->
    [{filename:basename(F), element(2, file:read_file(filename:join(Dir, F)))}
     || F <- files()] ++ [{"small.json", small()}].

bench(Dir, Reps) ->
    [begin
         N = iters(Name),
         Times = [begin
                      T0 = erlang:monotonic_time(microsecond),
                      decode_n(Bin, N),
                      erlang:monotonic_time(microsecond) - T0
                  end || _ <- lists:seq(1, Reps)],
         Sorted = lists:sort(Times),
         io:format("~-18s x~-4b median ~8b us   min ~8b us~n",
                   [Name, N, lists:nth(max(1, Reps div 2), Sorted),
                    hd(Sorted)])
     end || {Name, Bin} <- corpus(Dir)],
    ok.

iters("small.json") -> 2000;
iters(_) -> 10.

decode_n(_Bin, 0) -> ok;
decode_n(Bin, N) -> _ = json:decode(Bin), decode_n(Bin, N - 1).

check(Dir) ->
    Decoded = [json:decode(Bin) || {_, Bin} <- corpus(Dir)],
    Weird = [<<"[1,2,3]">>, <<"{}">>, <<"[]">>, <<"\"\"">>,
             <<"123456789012345678901234567890">>,        % bignum
             <<"-0.0e-10">>, <<"[1.5e300]">>,
             <<"\"\\u00e5\\n\\t\\\\\"">>,                  % escapes
             <<"\"", 229/utf8, 228/utf8, 246/utf8, "\"">>, % direct utf8
             <<"[", (binary:copy(<<"0,">>, 9999))/binary, "0]">>,
             <<"{\"a\":{\"b\":{\"c\":[null,true,false]}}}">>],
    WeirdDecoded = [json:decode(B) || B <- Weird],
    Errors = [catch json:decode(B)
              || B <- [<<"[1,">>, <<"01">>, <<"\"abc">>, <<"nul">>, <<"1e">>]],
    Hash = erlang:phash2({Decoded, WeirdDecoded, Errors}, 1 bsl 32),
    io:format("decode hash ~b~n", [Hash]),
    Hash.

hot(Dir) ->
    [begin
         cprof:stop(),
         cprof:start(),
         decode_n(Bin, 3),
         cprof:pause(),
         {_, _, Funs} = cprof:analyse(json),
         Top = lists:sublist(lists:reverse(lists:keysort(2, Funs)), 8),
         io:format("~n~s:~n", [Name]),
         [io:format("  ~10b  ~p~n", [N, MFA]) || {MFA, N} <- Top]
     end || {Name, Bin} <- corpus(Dir)],
    cprof:stop(),
    ok.
