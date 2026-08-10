begin
  GetState = fun(P) ->
    S = self(),
    W = spawn(fun() -> R = (catch sys:get_state(P, 100)), S ! {st, self(), R} end),
    MRef = erlang:monitor(process, W),
    receive
      {st, W, R0} ->
        erlang:demonitor(MRef, [flush]),
        case R0 of {'EXIT', _} -> undefined; _ -> R0 end;
      {'DOWN', MRef, process, W, _} -> undefined
    after 500 -> (catch exit(W, kill)), erlang:demonitor(MRef, [flush]), undefined
    end
  end,
  Snap = fun(P) ->
    case erlang:process_info(P, [heap_size, total_heap_size, message_queue_len, dictionary, messages]) of
      undefined -> undefined;
      Info ->
        Hsz = proplists:get_value(heap_size, Info),
        THsz = proplists:get_value(total_heap_size, Info),
        MqLen = proplists:get_value(message_queue_len, Info),
        Dict = proplists:get_value(dictionary, Info),
        Msgs = proplists:get_value(messages, Info),
        St = GetState(P),
        Payload = try term_to_binary({Dict, Msgs, St})
                  catch _:_ -> try term_to_binary({Dict, [], undefined})
                               catch _:_ -> term_to_binary({[], [], undefined}) end
                  end,
        {Hsz, THsz, MqLen, Payload}
    end
  end,
  Data0 = [Snap(P) || P <- erlang:processes()],
  Data = [X || X <- Data0, X =/= undefined],
  EtsSave = lists:foldl(fun(T, {U, C, Cnt, Sk}) ->
    try
      case ets:info(T, compressed) of
        true -> {U, C, Cnt, Sk + 1};
        _ ->
          Type = ets:info(T, type),
          Kp = ets:info(T, keypos),
          Objs = ets:tab2list(T),
          Cl = ets:new(captmp, [Type, {keypos, Kp}, compressed, public]),
          ets:insert(Cl, Objs),
          Um = ets:info(T, memory),
          Cm = ets:info(Cl, memory),
          ets:delete(Cl),
          {U + Um, C + Cm, Cnt + 1, Sk}
      end
    catch _:_ -> {U, C, Cnt, Sk + 1} end
  end, {0, 0, 0, 0}, ets:all()),
  Result = {Data, EtsSave, erlang:memory(), erlang:system_info(process_count),
            erlang:system_info(wordsize)},
  ok = file:write_file("/tmp/cap.bin", term_to_binary(Result, [compressed])),
  {captured_processes, length(Data), ets_tables_measured, element(3, EtsSave)}
end.
