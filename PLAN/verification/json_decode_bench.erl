-module(bench2).
-export([go/0]).

%% twitter-like: string-heavy (text fields, nested user objects)
twitter(N) ->
    #{<<"statuses">> =>
          [#{<<"text">> => txt(80 + rand:uniform(120)),
             <<"id">> => rand:uniform(1 bsl 50),
             <<"retweet_count">> => rand:uniform(10000),
             <<"user">> => #{<<"name">> => txt(10+rand:uniform(20)),
                             <<"screen_name">> => txt(8+rand:uniform(12)),
                             <<"description">> => txt(40+rand:uniform(100)),
                             <<"followers_count">> => rand:uniform(1000000)},
             <<"lang">> => <<"en">>,
             <<"truncated">> => false}
           || _ <- lists:seq(1, N)]}.

%% canada-like: number-heavy (coordinate pairs)
canada(N) ->
    #{<<"type">> => <<"FeatureCollection">>,
      <<"features">> =>
          [#{<<"type">> => <<"Feature">>,
             <<"geometry">> =>
                 #{<<"type">> => <<"Polygon">>,
                   <<"coordinates">> =>
                       [[ [ (rand:uniform(360000)-180000)/1000,
                            (rand:uniform(180000)-90000)/1000 ]
                          || _ <- lists:seq(1, 40) ]]}}
           || _ <- lists:seq(1, N)]}.

%% citm-like: structure-heavy (deep nesting, ids + short strings)
citm(N) ->
    #{<<"events">> =>
          maps:from_list(
            [{integer_to_binary(rand:uniform(1 bsl 40)),
              #{<<"id">> => rand:uniform(1 bsl 40),
                <<"name">> => txt(20+rand:uniform(30)),
                <<"subTopicIds">> => [rand:uniform(1000) || _ <- lists:seq(1,8)],
                <<"performances">> =>
                    [#{<<"eventId">> => rand:uniform(1 bsl 40),
                       <<"start">> => rand:uniform(1 bsl 50),
                       <<"prices">> => [#{<<"amount">> => rand:uniform(50000),
                                          <<"audienceSubCategoryId">> => rand:uniform(100)}
                                        || _ <- lists:seq(1,5)]}
                     || _ <- lists:seq(1,3)]}}
             || _ <- lists:seq(1, N)])}.

txt(L) ->
    unicode:characters_to_binary(
      [ case rand:uniform(20) of
            1 -> 32; 2 -> $!; 3 -> $,; 4 -> $.;
            _ -> $a + rand:uniform(25) end || _ <- lists:seq(1, L) ]).

go() ->
    rand:seed(exsss, {7,8,9}),
    Docs = [{"twitter(str)", iolist_to_binary(json:encode(twitter(3000)))},
            {"canada(num) ", iolist_to_binary(json:encode(canada(1500)))},
            {"citm(struct)", iolist_to_binary(json:encode(citm(2500)))}],
    [io:format("~s: ~.2f MB~n",[T, byte_size(B)/1.0e6]) || {T,B} <- Docs],
    Iters = fun(B) -> max(20, 80_000_000 div byte_size(B)) end,
    Time = fun(B) ->
        N = Iters(B),
        T0 = erlang:monotonic_time(microsecond),
        lists:foreach(fun(_) -> json:decode(B) end, lists:seq(1,N)),
        T1 = erlang:monotonic_time(microsecond),
        (T1-T0)/N
    end,
    %% baseline = stock json (currently loaded from release)
    Stock = [{T, Time(B), B} || {T,B} <- Docs],
    code:unstick_mod(json), code:purge(json),
    {module,json} = code:load_abs("/tmp/scan/json"),
    io:format("~-14s ~10s ~10s ~8s~n",["doc","stock us","scan us","speedup"]),
    lists:foreach(
      fun({T, Su, B}) ->
              Sc = Time(B),
              io:format("~-14s ~9.1f ~9.1f ~7.3fx  (~w vs ~w MB/s)~n",
                        [T, Su, Sc, Su/Sc,
                         round(byte_size(B)/Su), round(byte_size(B)/Sc)])
      end, Stock),
    halt().
