%% This module is an attempt at creating a better erlang term format. It does these changes to the original:
%% 
%% 1. All sizes and identifiers (creation, serial,  are changed to be [varint] encoded.
%% 2. unsigned byte we leave as is, but signed small is encoded as 64-bit [zigzag] [varint], and bignums are encoded as 64-bit [varint] chunks.
%% 3. We move all "text" data to be after the structure of the terms. That means that binary, string and atoms are appended (and de-duplicated) at the end. The 


%[varint]: https://en.wikipedia.org/wiki/Variable-length_quantity
%[zigzag]: https://gist.github.com/mfuerstenau/ba870a29e16536fdbaba

-module(transcode).

-export([go/1, term_to_binary/1, term_to_binary/2]).

-compile({no_auto_import,[term_to_binary/2]}).

term_to_binary(Term) ->
    term_to_binary(Term, #{ method => false }).
term_to_binary(Term, Compress) ->
    {Bin, <<>>} = term(erlang:term_to_binary(Term), Compress),
    iolist_to_binary(Bin).

go(Filename) ->
    {ok, Bin} = file:read_file(Filename),

    TCs =
        [
         {"Orig Size", fun(B) -> transcode(B, {orig, fun(T) -> erlang:term_to_binary(T, []) end}) end},
         {"Orig Zip Size", fun(B) -> transcode(B, {orig, fun(T) -> erlang:term_to_binary(T, [compressed]) end}) end},
         {"New Size", fun(B) -> transcode(B, #{ method => false }) end},
         {"ZStd Block Size", fun(B) -> transcode(B, #{ method => zstd }) end},
         {"ZStd Block Dict Size",
          fun(B) ->
                  DictDir = string:trim(os:cmd("mktemp -d")),
                  transcode(B, #{ method => {zstd, train, DictDir} }),
                  Dict = string:trim(os:cmd("mktemp")),
                  dbg("zstd --train " ++ DictDir ++ "/* -o " ++ Dict),
                  dbg("~ts",[os:cmd("zstd --train " ++ DictDir ++ "/* -o " ++ Dict)]),
                  Ret = transcode(B, #{ method => {zstd, dict, Dict} }),
                  file:delete(Dict),
                  file:del_dir_r(DictDir),
                  Ret
          end},
         {"Zstd All", fun(B) -> transcode(B, #{ scope => all, method => zstd }) end},
         {"Zip Block", fun(B) -> transcode(B, #{ method => deflate }) end},
         {"Zip All", fun(B) -> transcode(B, #{ scope => all, method => deflate }) end},
         {"Tenc", fun(B) -> transcode(B, {orig, fun(T) -> tenc:encode(T) end}) end},
         {"Tenc Zstd", fun(B) -> transcode(B, {orig, fun(T) -> compress(tenc:encode(T), zstd) end}) end}
        ],

    run_tcs(Bin, TCs),
    ok.

run_tcs(Bin, [First | Rest] = All) ->
    MaxStrSize = lists:foldl(fun({Slogan, _}, Max) -> max(length(Slogan), Max) end, 0, All),
    OrigSize = run_test(Bin, First, MaxStrSize, 0),
    [run_test(Bin, TC, MaxStrSize, OrigSize) || TC <- Rest].

run_test(Bin, {Slogan, Fun}, Padding, OrigSize) ->
    Sz = iolist_size(Fun(Bin)),
    io:format("~"++integer_to_list(Padding+2)++"ts: ~10B (~p%)~n",
              [Slogan, Sz, (100 * Sz) div if OrigSize =:= 0 -> Sz; true -> OrigSize end]),
    Sz.

dbg(Fmt) ->
    dbg(Fmt, []).
dbg(_Fmt, _Args) ->
    %% io:format(_Fmt, _Args),
    ok.

transcode(<<0, Size:32, Bin:Size/binary, Rest/binary>>, Compress) ->
    case get_payload(Bin) of
        skip -> transcode(Rest, Compress);
        Term when element(1, Compress) =:= orig ->
            {orig, Fun} = Compress,
            case dec(iolist_to_binary(Term), #{}) of
                [131, DistHdr, Ctl] ->
                    [131, DistHdr, Fun(Ctl), transcode(Rest, Compress)];
                [131, DistHdr, Ctl, Msg] ->
                    [131, DistHdr, Fun(Ctl), Fun(Msg), transcode(Rest, Compress)]
            end;
        Term ->
            dbg("Decoding: ~p~n",[Term]),

            NewTerm = term(iolist_to_binary(Term), Compress),
            [NewTerm, transcode(Rest, Compress)]
    end;
transcode(<<>>, _) ->
    [].

get_payload(Bin) when is_binary(Bin) ->
    get_payload(binary_to_term(Bin));
get_payload({trace_ts,_Pid,return_from,{erlang, dist_ctrl_get_data, 1}, {_Size, Data}, _TS}) ->
    Data;
get_payload({trace_ts,_Pid,return_from,{erlang, dist_ctrl_get_data, 1}, none, _TS}) ->
    skip;
get_payload({trace_ts,_Pid,call,{erlang, dist_ctrl_put_data, [_, Data]}, _TS}) ->
    Data;
get_payload({trace_ts,_Pid,call,{erlang, dist_ctrl_get_data, _}, _TS}) ->
    skip.

dec(<<131, 68, Term/binary>>, Compress) ->
    {DistHdr, Rest, DistHdrState} = dec(<<68, Term/binary>>, #{ atoms => [], text => []}),
    <<>> = flush_state(DistHdrState),
    case dec(<<131, Rest/binary>>, Compress) of
        {Ctl, <<>>} ->
            [131, DistHdr, Ctl];
        {Ctl, MsgRest} ->
            {Msg, <<>>} = dec(<<131, MsgRest/binary>>, Compress),
            [131, DistHdr, Ctl, Msg]
    end;
dec(<<131, Term/binary>>, _Compress) ->
    {FinalTerm, Rest, _} = dec(Term, #{ index => 1 }),
    {FinalTerm, Rest};
%% Dist header
dec(<<68, 0>> = Bin, State) ->
    dbg("68: dist hdr~n"),
    {Bin, <<>>, State};
dec(<<68, N, Flags:((N bsr 1) + 1)/binary, Atoms/binary>>, State) ->
    dbg("68: dist hdr~n"),
    dbg("Atoms: ~p~n",[Atoms]),
    LongAtoms =
        if N rem 2 =:= 0 ->
                <<_:(byte_size(Flags)-1)/binary,_:3,LA:1, _:4>> = Flags,
                LA =:= 1;
           N rem 2 =:= 1 ->
                <<_:(byte_size(Flags)-1)/binary,_:4, _:3,LA:1>> = Flags,
                LA =:= 1
        end,
    dbg("Long atoms: ~p~n",[LongAtoms]),
    {Rest, AtomRefs, _} =
        lists:foldl(fun
                        (_, {_, _, Cnt} = Acc) when Cnt > N - 1 ->
                           Acc;
                        (<<0:1, Seg:3>>, {<<F, B/binary>>, Acc, Cnt}) ->
                           dbg("~p Old Seg: ~.2B Idx: ~.2B~n",[Cnt, Seg, F]),
                           {B, [F | Acc], Cnt + 1};
                        (<<1:1, Seg:3>>, {<<F, Sz, AtomText:Sz/binary, B/binary>>, Acc, Cnt}) ->
                           dbg("~p New Seg: ~.2B Idx: ~.2B Sz: ~p Txt: ~p~n",[Cnt, Seg, F, Sz, AtomText]),
                           {B, [F, Sz, AtomText | Acc], Cnt + 1}
                   end, {Atoms, [], 0}, lists:flatten([[F2, F1] || <<F1:4/bits, F2:4/bits>> <= Flags])),
    dbg("AtomRefs: ~p~n",[lists:reverse(AtomRefs)]),
    dbg("Rest: ~p~n",[Rest]),
    {[68, N, Flags, lists:reverse(AtomRefs)], Rest, State};
%% Bits
%% 77   Len Bits    Data
dec(<<77, Sz:32, Bits, Str:Sz/binary, Rest/binary>>, State) ->
    dbg("77: BitString~n"),
    {erlang:binary_to_term(<<131, 77, Sz:32, Bits, Str/binary>>), Rest, State};
%% Atom cache ref
dec(<<82, Index, Rest/binary>>, State) ->
    dbg("82: Atom cache~n"),
    {list_to_atom("$cached" ++ integer_to_list(Index)), Rest, State};
                                                %{list_to_atom("$cached"), Rest, State};
%% Pid
dec(<<88, Pid/binary>>, State) ->
    dbg("88: Pid~n"),
    {Node, <<ID:32, Serial:32, Creation:32, NodeRest/binary>>, NodeState} = dec(Pid, State),
    <<131, NodeBin/binary>> = erlang:term_to_binary(Node),
    {erlang:binary_to_term(<<131, 88, NodeBin/binary, ID:32, Serial:32, Creation:32>>), NodeRest, NodeState};
%% Reference
%% 90   Len Node    Creation    ID ...
dec(<<90, Len:16, Ref/binary>>, State) ->
    dbg("90: Ref~n"),
    {Node, <<Creation:32, RefData:(Len*4)/binary, NodeRest/binary>>, NodeState} = dec(Ref, State),
    <<131, NodeBin/binary>> = erlang:term_to_binary(Node),
    {erlang:binary_to_term(<<131, 90, Len:16, NodeBin/binary, Creation:32, RefData/binary>>), NodeRest, NodeState};
%% Small int
%% 97   Int
dec(<<97, Int, Rest/binary>>, State) ->
    dbg("97: Very Small~n"),
    {Int, Rest, State};
%% Small int
%% 98   Int
dec(<<98, Int:32, Rest/binary>>, State) ->
    dbg("98: Small~n"),
    {Int, Rest, State};
dec(<<101, Ref/binary>>, State) ->
    dbg("101: Old Ref~n"),
    {Node, <<RefData:32, Creation, NodeRest/binary>>, NodeState} = dec(Ref, State),
    <<131, NodeBin/binary>> = erlang:term_to_binary(Node),
    {erlang:binary_to_term(<<131, 101, NodeBin/binary, RefData:32, Creation>>), NodeRest, NodeState};
%% Small tuple
dec(<<104, Sz, Tuple/binary>>, State) ->
    dbg("104: Tuple~n"),
    {Term, Rest, NewState} =
        lists:foldl(
          fun(0, Acc) ->
                  Acc;
             (_N, {Acc, Bin, S}) ->
                  {T, R, NewS} = dec(Bin, S),
                  {[T | Acc], R, NewS}
          end, {[], Tuple, State}, lists:reverse(lists:seq(0,Sz))),
    {list_to_tuple(lists:reverse(Term)), Rest, NewState};
dec(<<106, Rest/binary>>, State) ->
    dbg("106: NIL~n"),
    {[], Rest, State};
%% String
dec(<<107, Sz:16, Str:Sz/binary, Rest/binary>>, State) ->
    dbg("107: String~n"),
    {binary_to_list(Str), Rest, State};
%% List
%% 108  Length  Elements    Tail
dec(<<108, Sz:32, List/binary>>, State) ->
    dbg("108: list~n"),
    {Term, Rest, NewState} =
        lists:foldl(
          fun(0, Acc) ->
                  Acc;
             (_N, {Acc, Bin, S}) ->
                  {T, R, NewS} = dec(Bin, S),
                  {[T | Acc], R, NewS}
          end, {[], List, State}, lists:reverse(lists:seq(0,Sz+1))),
    {lists:reverse(Term), Rest, NewState};
%% Binary
%% 109  Len Data
dec(<<109, Sz:32, Str:Sz/binary, Rest/binary>>, State) ->
    dbg("109: binary~n"),
    {Str, Rest, State};
%% Small big
%% 110  n   Sign    d(0) ... d(n-1)
dec(<<110, Sz, Sign, Num:Sz/binary, Rest/binary>>, State) ->
    dbg("110: big num~n"),
    BigNum = erlang:binary_to_term(<<131, 110, Sz, Sign, Num/binary>>),
    {BigNum, Rest, State};
%% Map
dec(<<116, Sz:32, Map/binary>>, State) ->
    dbg("116: Map~n"),
    {Term, Rest, NewState} =
        lists:foldl(
          fun(0, Acc) ->
                  Acc;
             (_N, {Acc, Bin, S}) ->
                  {Key, R, NewS} = dec(Bin, S),
                  {Value, R2, NewS2} = dec(R, NewS),
                  {[{Key, Value} | Acc], R2, NewS2}
          end, {[], Map, State}, lists:reverse(lists:seq(0,Sz))),
    {maps:from_list(Term), Rest, NewState};
%% Small atom utf8
dec(<<119, Sz, Str:Sz/binary, Rest/binary>>, State) ->
    dbg("119: atom~n"),
    {binary_to_atom(Str, utf8), Rest, State};
dec(<<A, _/binary>> = Term, State) ->
    io:format("Unhandled tag: ~p (~w)~n",[A, Term]),
    ok = nok,
    {Term, <<>>, State}.

term(<<131, 68, Term/binary>>, Compress) ->
    {DistHdr, Rest, DistHdrState} = term(<<68, Term/binary>>, #{ atoms => [], text => []}),
    <<>> = flush_state(DistHdrState),
    case term(<<131, Rest/binary>>, Compress) of
        {[_ | Ctl], <<>>} ->
            [131, DistHdr, Ctl];
        {[_ | Ctl], MsgRest} ->
            {[_ | Msg], <<>>} = term(<<131, MsgRest/binary>>, Compress),
            [131, DistHdr, Ctl, Msg]
    end;
term(<<131, Term/binary>>, Compress) ->
    {FinalTerm, Rest, FinalState} = term(Term, #{ index => 1 }),
    %% dbg("FinalTerm: ~p~nFinalState: ~p~n",[FinalTerm, FinalState]),
    case flush_state(FinalState) of
        <<>> ->
            {[131, FinalTerm], Rest};
        BlockData ->
            case maps:get(scope, Compress, block) of
                block ->
                    {[132, sizeof(FinalTerm), FinalTerm, compress(BlockData, maps:get(method, Compress))], Rest};
                all ->
                    {[132, compress([sizeof(FinalTerm), FinalTerm, BlockData], maps:get(method, Compress))], Rest}
            end
    end;
%% Dist header
term(<<68, 0>> = Bin, State) ->
    dbg("68: dist hdr~n"),
    {Bin, <<>>, State};
term(<<68, N, Flags:((N bsr 1) + 1)/binary, Atoms/binary>>, State) ->
    dbg("68: dist hdr~n"),
    dbg("Atoms: ~p~n",[Atoms]),
    LongAtoms =
        if N rem 2 =:= 0 ->
                <<_:(byte_size(Flags)-1)/binary,_:3,LA:1,_:4>> = Flags,
                LA =:= 1;
           N rem 2 =:= 1 ->
                <<_:(byte_size(Flags)-1)/binary,_:4,_:3,LA:1>> = Flags,
                LA =:= 1
        end,
    dbg("Long atoms: ~p~n",[LongAtoms]),
    {Rest, AtomRefs, _} =
        lists:foldl(fun
                        (_, {_, _, Cnt} = Acc) when Cnt > N - 1 ->
                           Acc;
                        (<<0:1, Seg:3>>, {<<F, B/binary>>, Acc, Cnt}) ->
                           dbg("~p Old Seg: ~.2B Idx: ~.2B~n",[Cnt, Seg, F]),
                           {B, [F | Acc], Cnt + 1};
                        (<<1:1, Seg:3>>, {<<F, Sz, AtomText:Sz/binary, B/binary>>, Acc, Cnt}) ->
                           dbg("~p New Seg: ~.2B Idx: ~.2B Sz: ~p Txt: ~p~n",[Cnt, Seg, F, Sz, AtomText]),
                           {B, [F, Sz, AtomText | Acc], Cnt + 1}
                   end, {Atoms, [], 0}, lists:flatten([[F2, F1] || <<F1:4/bits, F2:4/bits>> <= Flags])),
    dbg("AtomRefs: ~p~n",[lists:reverse(AtomRefs)]),
    dbg("Rest: ~p~n",[Rest]),
    {[68, N, Flags, lists:reverse(AtomRefs)], Rest, State};
%% Bits
%% 77   Len Bits    Data
term(<<77, Sz:32, Bits, Str:Sz/binary, Rest/binary>>, State) ->
    dbg("77: BitString~n"),
    {SzOrKey, NewState} = update_block(State, Str),
    {[77, SzOrKey, Bits], Rest, NewState};
%% Atom cache ref
term(<<82, Index, Rest/binary>>, State) ->
    dbg("82: Atom cache~n"),
    AtomStr = "$cached" ++ integer_to_list(Index),
    term(<<119, (length(AtomStr)), (list_to_binary(AtomStr))/binary, Rest/binary>>, State);
%% {[82, Index], Rest, State};
%% Pid
term(<<88, Pid/binary>>, State) ->
    dbg("88: Pid~n"),
    {Node, <<ID:32, Serial:32, Creation:32, NodeRest/binary>>, NodeState} = term(Pid, State),
    {[88, Node, encode_varint(ID), encode_varint(Serial), encode_varint(Creation)], NodeRest, NodeState};
%% Reference
%% 90   Len Node    Creation    ID ...
term(<<90, Len:16, Ref/binary>>, State) ->
    dbg("90: Ref~n"),
    {Node, <<Creation:32, RefData:(Len*4)/binary, NodeRest/binary>>, NodeState} = term(Ref, State),
    % {SzOrKey, NewState} = update_block(NodeState, RefData),
    % {[90, SzOrKey, Node, encode_varint(Creation)], NodeRest, NewState};
    {[90, encode_varint(Len), Node, encode_varint(Creation), RefData], NodeRest, NodeState};
%% Small int
%% 97   Int
term(<<97, Int, Rest/binary>>, State) ->
    dbg("97: Very Small~n"),
    {[97, Int], Rest, State};
%% Small int
%% 98   Int
term(<<98, Int:32/signed, Rest/binary>>, State) ->
    dbg("98: Small~n"),
    {[98, encode_zigzag_varint(Int)], Rest, State};
term(<<101, Ref/binary>>, State) ->
    dbg("101: Old Ref~n"),
    {Node, <<RefData:32, Creation, NodeRest/binary>>, NodeState} = term(Ref, State),
    {[101, Node, encode_varint(RefData), Creation], NodeRest, NodeState};
%% Small tuple
term(<<104, Sz, Tuple/binary>>, State) ->
    dbg("104: Tuple~n"),
    {Term, Rest, NewState} =
        lists:foldl(
          fun(0, Acc) ->
                  Acc;
             (_N, {Term, Bin, S}) ->
                  {T, R, NewS} = term(Bin, S),
                  {[Term, T], R, NewS}
          end, {[], Tuple, State}, lists:reverse(lists:seq(0,Sz))),
    {[104, Sz, Term], Rest, NewState};
term(<<106, Rest/binary>>, State) ->
    dbg("106: NIL~n"),
    {[<<106>>], Rest, State};
%% String
term(<<107, Sz:16, Str:Sz/binary, Rest/binary>>, State) ->
    dbg("107: String~n"),
    {SzOrKey, NewState} = update_block(State, Str),
    {[107, SzOrKey], Rest, NewState};
%% List
%% 108  Length  Elements    Tail
term(<<108, Sz:32, List/binary>>, State) ->
    dbg("108: list~n"),
    {Term, Rest, NewState} =
        lists:foldl(
          fun(0, Acc) ->
                  Acc;
             (_N, {Term, Bin, S}) ->
                  {T, R, NewS} = term(Bin, S),
                  {[Term, T], R, NewS}
          end, {[], List, State}, lists:reverse(lists:seq(0,Sz+1))),
    {[104, encode_varint(Sz), Term], Rest, NewState};
%% Binary
%% 109  Len Data
term(<<109, Sz:32, Str:Sz/binary, Rest/binary>>, State) ->
    dbg("109: binary~n"),
    {SzOrKey, NewState} = update_block(State, Str),
    {[109, SzOrKey], Rest, NewState};
%% Small big
%% 110  n   Sign    d(0) ... d(n-1)
term(<<110, Sz, Sign, Num:Sz/binary, Rest/binary>>, State) ->
    dbg("110: big num~n"),
    {[<<110, Sz, Sign, Num/binary>>], Rest, State};
%% Map
term(<<116, Sz:32, Map/binary>>, State) ->
    dbg("116: Map~n"),
    {Term, Rest, NewState} =
        lists:foldl(
          fun(0, Acc) ->
                  Acc;
             (_N, {Term, Bin, S}) ->
                  {T, R, NewS} = term(Bin, S),
                  {[Term, T], R, NewS}
          end, {[], Map, State}, lists:reverse(lists:seq(0,Sz*2))),
    {[116, encode_varint(Sz), Term], Rest, NewState};
%% Small atom utf8
term(<<119, Sz, Str:Sz/binary, Rest/binary>>, State) ->
    dbg("119: atom~n"),
    {SzOrKey, NewState} = update_block(State, Str),
    {[119, SzOrKey], Rest, NewState};
term(<<A, _/binary>> = Term, State) ->
    io:format("Unhandled tag: ~p (~w)~n",[A, Term]),
    ok = nok,
    {Term, <<>>, State}.

update_block(State, Bin) ->
    dbg("Update block: ~p ~p~n",[Bin, State]),
    {IndexOrSz, NewState} =
        case maps:find(iolist_to_binary(Bin), State) of
            {ok, Index} -> {-Index, State};
            _error ->
                NewIndex = maps:get(index, State) + 1,
                {iolist_size(Bin), State#{ iolist_to_binary(Bin) =>  NewIndex,
                                           index := NewIndex }}
        end,
    dbg("ZigZag: ~p -> ~p~n",[IndexOrSz, encode_zigzag_varint(IndexOrSz)]),
    {encode_zigzag_varint(IndexOrSz), NewState}.

flush_state(State) ->
    iolist_to_binary([Bin || {_Key, Bin} <- lists:sort([{Key, Bin} || Bin := Key <- State, not is_atom(Bin)])]).

encode_zigzag_varint(N) when N >= -1 bsl 31, N < 1 bsl 31 ->
    encode_varint((N bsr (32-1)) bxor (N bsl 1)).

encode_varint(0) ->
    <<0>>;
encode_varint(N) ->
    do_encode_varint(N).
do_encode_varint(0) ->
    <<>>;
do_encode_varint(N) ->
    HasMore = if N > 127 -> 1; true -> 0 end,
    <<(do_encode_varint(N bsr 7))/binary, HasMore:1, N:7>>.

sizeof(Bin) ->
    encode_varint(iolist_size(Bin)).

compress(Bin, false) ->
    Bin;
compress(Bin, zstd) ->
    TmpFile = string:trim(os:cmd("mktemp")),
    ok = file:write_file(TmpFile, Bin),
    os:cmd("zstd --ultra -22 " ++ TmpFile),
    {ok, Compressed} = file:read_file(TmpFile++".zst"),
    ok = file:delete(TmpFile++".zst"),
    ok = file:delete(TmpFile),
    case iolist_size(Bin) < iolist_size(Compressed) of
        true ->
            Bin;
        false ->
            Compressed
    end;
compress(Bin, {zstd, train, DictDir}) ->
    TmpFile = string:trim(os:cmd("mktemp -p  " ++ DictDir)),
    ok = file:write_file(TmpFile, Bin),
    os:cmd("zstd " ++ TmpFile),
    {ok, Compressed} = file:read_file(TmpFile++".zst"),
    ok = file:delete(TmpFile++".zst"),
    case iolist_size(Bin) < iolist_size(Compressed) of
        true ->
            Bin;
        false ->
            Compressed
    end;
compress(Bin, {zstd, dict, Dict}) ->
    TmpFile = string:trim(os:cmd("mktemp")),
    ok = file:write_file(TmpFile, Bin),
    os:cmd("zstd -D "++ Dict ++" " ++ TmpFile),
    {ok, Compressed} = file:read_file(TmpFile++".zst"),
    ok = file:delete(TmpFile),
    ok = file:delete(TmpFile++".zst"),
    case iolist_size(Bin) < iolist_size(Compressed) of
        true ->
            Bin;
        false ->
            Compressed
    end;
compress(A, deflate) ->
    ALen = iolist_size(A),
    ZStream = zlib:open(),
    try
        ok = zlib:deflateInit(ZStream, 9),
        B = erlang:iolist_to_binary(zlib:deflate(ZStream, A, finish)),
        _ = zlib:deflateEnd(ZStream),
        BLen = byte_size(B),
        case ALen =< BLen of
            false ->
                B;
            true ->
                A
        end
    after
        zlib:close(ZStream)
    end.
