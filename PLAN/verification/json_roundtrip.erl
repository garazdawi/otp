-module(rnd).
-export([go/0]).
gen(0) -> elem();
gen(D) ->
    case rand:uniform(7) of
        1 -> [gen(D-1) || _ <- lists:seq(1, rand:uniform(5)-1)];
        2 -> maps:from_list([{key(), gen(D-1)} || _ <- lists:seq(1,rand:uniform(4)-1)]);
        _ -> elem()
    end.
key() -> list_to_binary([$k | integer_to_list(rand:uniform(1000))]).
elem() ->
    case rand:uniform(6) of
        1 -> rand:uniform(1000000) - 500000;
        2 -> (rand:uniform(1000)-500) / 7;
        3 -> str();
        4 -> true; 5 -> false; _ -> null
    end.
str() ->
    L = rand:uniform(40),
    unicode:characters_to_binary(
      [ case rand:uniform(10) of
          1 -> 34; 2 -> 92; 3 -> 10; 4 -> 9; 5 -> 16#00e9;
          _ -> 32 + rand:uniform(94) end || _ <- lists:seq(1,L) ]).
go() ->
    rand:seed(exsss, {1,2,3}),
    Terms = [gen(rand:uniform(4)) || _ <- lists:seq(1, 5000)],
    Jsons = [iolist_to_binary(json:encode(T)) || T <- Terms],
    Exp = [json:decode(J) || J <- Jsons],
    code:unstick_mod(json), code:purge(json), {module,json}=code:load_abs("/tmp/scan/json"),
    Got = [json:decode(J) || J <- Jsons],
    Bad = [{J,E,G} || {J,E,G} <- lists:zip3(Jsons,Exp,Got), E =/= G],
    io:format("docs: ~p  mismatches: ~p~n", [length(Jsons), length(Bad)]),
    [io:format("DIFF ~p~n exp ~p~n got ~p~n",[J,E,G]) || {J,E,G} <- lists:sublist(Bad,5)],
    halt().
