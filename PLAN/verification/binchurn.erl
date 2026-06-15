-module(binchurn).
-export([spawn_worker/1]).
%% Mimics the Jason decode pattern that drives bin_vheap pressure:
%% repeatedly slice a refc binary into many sub-binaries + build a map,
%% then drop them. Spawn opts let us vary min_bin_vheap_size /
%% fullsweep_after per-process to isolate the major-GC trigger.
spawn_worker(Opts) ->
    Big = binary:copy(<<"abcdefgh">>, 4096),   % 32KB refc binary
    spawn_opt(fun() -> loop(Big) end, Opts).
loop(Big) ->
    Subs = [binary:part(Big, I * 16, 96) || I <- lists:seq(0, 40)],  % 41 sub-bins
    M = maps:from_list(lists:zip(lists:seq(1, 41), Subs)),
    _ = maps:size(M),
    Tup = list_to_tuple(Subs),
    _ = element(1, Tup),
    receive stop -> ok after 0 -> loop(Big) end.
