Node = NODEATOM,
Fun = fun() ->
  P0 = erlang:memory(processes),
  T0 = erlang:memory(total),
  Procs = erlang:processes(),
  Live = lists:sum([ case erlang:process_info(Pid, total_heap_size) of {_, V} -> V; _ -> 0 end || Pid <- Procs ]),
  WS = erlang:system_info(wordsize),
  EtsSave = lists:foldl(fun(Tab, {U, C, Cnt, Sk}) ->
      try
        case ets:info(Tab, compressed) of
          true -> {U, C, Cnt, Sk + 1};
          _ ->
            Ty = ets:info(Tab, type),
            Kp = ets:info(Tab, keypos),
            Os = ets:tab2list(Tab),
            Cl = ets:new(probetmp, [Ty, {keypos, Kp}, compressed, public]),
            ets:insert(Cl, Os),
            Um = ets:info(Tab, memory),
            Cm = ets:info(Cl, memory),
            ets:delete(Cl),
            {U + Um, C + Cm, Cnt + 1, Sk}
        end
      catch _:_ -> {U, C, Cnt, Sk + 1} end
    end, {0, 0, 0, 0}, ets:all()),
  [ (catch erlang:garbage_collect(Pid, [{type, major}])) || Pid <- Procs ],
  timer:sleep(300),
  P1 = erlang:memory(processes),
  {EtsU, EtsC, EtsCnt, _} = EtsSave,
  [{otp, erlang:system_info(otp_release)}, {procs, length(Procs)},
   {processes_mb, P0 / 1048576}, {live_heap_mb, Live * WS / 1048576},
   {ets_mb, erlang:memory(ets) / 1048576},
   {ets_unc_mb, EtsU * WS / 1048576}, {ets_comp_mb, EtsC * WS / 1048576},
   {ets_tables, EtsCnt}, {ets_saved_mb, (EtsU - EtsC) * WS / 1048576},
   {shrink_gcall_saved_mb, (P0 - P1) / 1048576}, {total_mb, T0 / 1048576}]
end,
R = rpc:call(Node, erlang, apply, [Fun, []]),
erlang:display(R),
halt().
