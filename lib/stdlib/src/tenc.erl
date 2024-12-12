-module(tenc).

-include_lib("argo/include/argo_common.hrl").

% -define(is_i64(X), (is_integer((X)) andalso (X) >= -16#8000000000000000 andalso (X) =< 16#7FFFFFFFFFFFFFFF)).
% -define(is_u64(X), (is_integer((X)) andalso (X) >= 0 andalso (X) =< 16#FFFFFFFFFFFFFFFF)).

-export([
    compare/1,
    compare/2,
    display/1,
    display_ext/1,
    internal/0,
    internal/1,
    encode/1,
    encode_with_cache/2,
    encode_a/1,
    encode_b/1,
    transcode/1,
    xxd/1
]).

% -record(atom_block, {
%     atoms = argo_index_set:new() :: argo_index_set:t(atom()),
%     values =
% }).

-record(block, {
    buffer = <<>> :: binary(),
    values = argo_index_set:new() :: argo_index_set:t(dynamic())
}).

-record(enc, {
    atoms = #{} :: #{atom() => 0..2048},
    blocks = argo_index_map:new() :: argo_index_map:t(char(), #block{}),
    core = <<>> :: binary()
}).

-define(ATOM_EXT, $a).
-define(ATOM_CACHE_REF, $A).
-define(FLOAT64_EXT, $f).
-define(BIGNUM_NEG_EXT, $i).
-define(BIGNUM_POS_EXT, $I).
-define(BINARY_EXT, $b).
-define(BITSTRING_EXT, $B).
-define(EXPORT_EXT, $e).
-define(FUNCTION_EXT, $f).
-define(LIST_EXT, $l).
-define(MAP_EXT, $m).
-define(NIL_EXT, $n).
-define(PID_EXT, $p).
-define(PORT_EXT, $P).
-define(REFERENCE_EXT, $r).
-define(STRING_EXT, $s).
-define(TUPLE_EXT, $t).
-define(VARINT_EXT, $v).

internal() ->
    fun(X) -> X + 1 end.

internal(X) ->
    fun(Y) -> Y + X end.

compare(Term) ->
    compare(Term, #{}).

display(Term) ->
    compare(Term, #{display => true}).

display_ext(Term) ->
    compare(Term, #{display => ext}).

compare(Term, Options) ->
    Cache = maps:get(cache, Options, #{}),
    Compress = maps:get(compress, Options, deflate),
    Display = maps:get(display, Options, false),
    X = erlang:term_to_binary(Term, [{minor_version, 2}]),
    Y = encode_with_cache(Term, Cache),
    Z = transcode:term_to_binary(Term),
    XLen = iolist_size(X),
    YLen = iolist_size(Y),
    ZLen = iolist_size(Z),
    XYRatio = ratio(XLen, YLen),
    XZRatio = ratio(XLen, ZLen), 
    XC = compress(X, Compress),
    YC = compress(Y, Compress),
    ZC = transcode:term_to_binary(Term, #{ method => Compress }),
    XCLen = iolist_size(XC),
    YCLen = iolist_size(YC),
    ZCLen = iolist_size(ZC),
    XCYCRatio = ratio(XCLen, YCLen),
    XCZCRatio = ratio(XCLen, ZCLen),
    io:format("T2B vs tenc [U] ~w -> ~w (~w%)~n", [XLen, YLen, XYRatio]),
    io:format("T2B vs tenc [C] ~w -> ~w (~w%)~n", [XCLen, YCLen, XCYCRatio]),
    io:format("T2B vs tran [U] ~w -> ~w (~w%)~n", [XLen, ZLen, XZRatio]),
    io:format("T2B vs tran [C] ~w -> ~w (~w%)~n", [XCLen, ZCLen, XCZCRatio]),
    Barrier = binary:copy(<<"=">>, 47),
    case Display of
        ext ->
            io:format("~ts~nT2B  [U] (~w-bytes):~n~ts~n~n", [Barrier, XLen, format_hex(X)]),
            io:format("~ts~nTENC [U] (~w-bytes):~n~ts~n~n", [Barrier, YLen, format_hex(Y)]),
            io:format("~ts~nTRAN [U] (~w-bytes):~n~ts~n~n", [Barrier, ZLen, format_hex(Z)]),
            io:format("~ts~nT2B  [C] (~w-bytes):~n~ts~n~n", [Barrier, XCLen, format_hex(XC)]),
            io:format("~ts~nTENC [C] (~w-bytes):~n~ts~n~n", [Barrier, YCLen, format_hex(YC)]),
            io:format("~ts~nTRAN [C] (~w-bytes):~n~ts~n~n", [Barrier, ZCLen, format_hex(ZC)]),
            ok;
        true ->
            io:format("~ts~nXC (~w-bytes):~n~ts~n~n", [Barrier, XCLen, format_hex(XC)]),
            io:format("~ts~nYC (~w-bytes):~n~ts~n~n", [Barrier, YCLen, format_hex(YC)]),
            ok;
        _ ->
            ok
    end.

transcode(Bin) ->
    Bin.

compress(A, deflate) ->
    deflate(A);
compress(A, zstd) ->
    zstd(A).

zstd(A) ->
    ALen = iolist_size(A),
    Filename = "/tmp/uncompressed.bin",
    ok = file:write_file(Filename, A),
    % Port = erlang:open_port({spawn_executable, "/opt/homebrew/bin/zstd"}, [{args, ["--ultra", "-22", "--stdout", Filename]}, stream, binary, use_stdio, in, exit_status, stderr_to_stdout]),
    Port = erlang:open_port({spawn_executable, os:find_executable("zstd")}, [{args, ["--stdout", Filename]}, stream, binary, use_stdio, in, exit_status, stderr_to_stdout]),
    % true = erlang:port_command(Port, A),
    % _ = erlang:port_control(Port, 16#0112c000, []),
    % true = erlang:port_close(Port),
    zstd(Port, A, ALen, <<>>, 0).

zstd(Port, A, ALen, B, BLen) ->
    receive
        Msg ->
            case Msg of
                {Port, {data, Data}} ->
                    zstd(Port, A, ALen, <<B/bytes, Data/bytes>>, BLen + byte_size(Data));
                {Port, {exit_status, 0}} ->
                    case ALen =< BLen of
                        false ->
                            B;
                        true ->
                            A
                    end
            end
    after
        1000 ->
            timeout
    end.

deflate(A) ->
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

xxd(Bin) ->
    io:format("Bin (~w-bytes):~n~ts~n~n", [byte_size(Bin), format_hex(Bin)]),
    ok.

format_hex(Bin) ->
    format_hex_bytes(Bin, []).

format_hex_bytes(<<>>, Lines) ->
    lists:join(<<"\n">>, lists:reverse(Lines));
format_hex_bytes(Bin, Lines) ->
    {Bytes, Padding, Rest} =
        case Bin of
            <<Bs:16/bytes, Rs/bytes>> ->
                {Bs, <<>>, Rs};
            <<Bs/bytes>> ->
                case (16 - byte_size(Bs)) of
                    0 ->
                        {Bs, <<>>, <<>>};
                    Pad ->
                        {Bs, binary:copy(<<"  ">>, Pad), <<>>}
                end
        end,
    Chars = <<(binary:encode_hex(Bytes, lowercase))/bytes, Padding/bytes>>,
    Line = lists:join(<<" ">>, [<<Hi:8, Lo:8>> || <<Hi:8, Lo:8>> <= Chars]),
    LineExt = Line ++ format_printable_bytes(Bytes),
    format_hex_bytes(Rest, [LineExt | Lines]).

format_printable_bytes(Bytes) ->
    format_printable_bytes(Bytes, []).

format_printable_bytes(<<>>, Chars) ->
    [$\s | lists:reverse(Chars)];
format_printable_bytes(<<Char:8, Rest/bytes>>, Chars) ->
    case (Char > 31 andalso Char < 127) of
        false ->
            format_printable_bytes(Rest, ["." | Chars]);
        true ->
            format_printable_bytes(Rest, [Char | Chars])
    end.
    % case io_lib:printable_unicode_list([Char]) of
    %     false ->
    %         format_printable_bytes(Rest, ["." | Chars]);
    %     true when (Char >= $A andalso Char =< $Z) orelse (Char >= $a andalso Char =< $z) orelse (Char >= $0 andalso Char =< $9) orelse (Char =:= $@) ->
    %         format_printable_bytes(Rest, [Char | Chars]);
    %     true ->
    %         format_printable_bytes(Rest, [" " | Chars])
    %         % case Char of
    %         %     $\n ->
    %         %         format_printable_bytes(Rest, ["\\n" | Chars]);
    %         %     $\r ->
    %         %         format_printable_bytes(Rest, ["\\r" | Chars]);
    %         %     $\s ->
    %         %         format_printable_bytes(Rest, ["\\s" | Chars]);
    %         %     $\t ->
    %         %         format_printable_bytes(Rest, ["\\t" | Chars]);
    %         %     _ ->
    %         %         format_printable_bytes(Rest, [[$\s, Char] | Chars])
    %         % end
    % end.
% format_printable_bytes(<<_:8, Rest/bytes>>, Chars) ->
%     format_printable_bytes(Rest, [$. | Chars]).

ratio(XLen, YLen) ->
    trunc(((YLen - XLen) / YLen) * 100).

encode(Term) ->
    encode_b(Term).

encode_with_cache(Term, Cache) ->
    encode_b(Term, Cache).

encode_a(Term) ->
    encode(#enc{}, Term).

encode_b(Term) ->
    encode_b(Term, #{}).

encode_b(Term, Cache) ->
    % Enc1 = #enc{blocks = argo_index_map:from_list([{?ATOM_EXT, #block{}}])},
    Enc1 = #enc{atoms = Cache},
    _Enc2 = #enc{blocks = Blocks, core = Core} = encode(Enc1, Term),
    BlocksChunk = argo_index_map:foldl(fun(_Index, _BlockKey, #block{buffer = Buffer}, BlocksAcc) ->
        Length = byte_size(Buffer),
        <<
            BlocksAcc/bytes,
            (argo_varint:write_zigzag_u64(Length))/bytes,
            Buffer/bytes
        >>
    end, <<>>, Blocks),
    CoreChunk = <<(argo_varint:write_zigzag_u64(byte_size(Core)))/bytes, Core/bytes>>,
    <<133, BlocksChunk/bytes, CoreChunk/bytes>>.

encode_atom_cache_ref(Enc1 = #enc{}, Block1 = #block{}, Key, CacheIndex) ->
    Data = argo_varint:write_zigzag_u64(CacheIndex),
    Length = byte_size(Data),
    Block2 = write_block(Block1, Data),
    Enc2 = write_core(Enc1, <<Key:8, (argo_varint:write_zigzag_i64(Length))/bytes>>),
    {Enc2, Block2}.

encode_atom_ext(Enc1 = #enc{}, Block1 = #block{}, Key, Value) ->
    Data = erlang:atom_to_binary(Value, utf8),
    Length = byte_size(Data),
    Block2 = write_block(Block1, Data),
    Enc2 = write_core(Enc1, <<Key:8, (argo_varint:write_zigzag_i64(Length))/bytes>>),
    {Enc2, Block2}.

encode_binary_ext(Enc1 = #enc{}, Block1 = #block{}, Key, Value) ->
    Data = Value,
    Length = byte_size(Data),
    Block2 = write_block(Block1, Data),
    Enc2 = write_core(Enc1, <<Key:8, (argo_varint:write_zigzag_i64(Length))/bytes>>),
    {Enc2, Block2}.

encode_bits_ext(Enc1 = #enc{}, Block1 = #block{}, Key, Value) ->
    Data = Value,
    Length = bit_size(Data),
    PaddedData = <<Data/bits, 0:(8 - (Length rem 8))>>,
    true = is_binary(PaddedData),
    Block2 = write_block(Block1, PaddedData),
    Enc2 = write_core(Enc1, <<Key:8, (Length rem 8), (argo_varint:write_zigzag_i64(Length+1))/bytes>>),
    {Enc2, Block2}.

encode_export_ext(Enc1 = #enc{core = Core1}, Block1 = #block{}, Key, Export) ->
    case maps:from_list(erlang:fun_info(Export)) of
        #{type := external, module := Module, name := Name, arity := Arity, env := []} ->
            Enc2 = Enc1#enc{core = <<>>},
            % Enc3 = #enc{core = <<?ATOM_EXT:8, EncModule/bytes>>} = encode(Enc2, Module),
            Enc3 = #enc{core = EncModule} = encode(Enc2, Module),
            Enc4 = Enc3#enc{core = <<>>},
            % Enc5 = #enc{core = <<?ATOM_EXT:8, EncName/bytes>>} = encode(Enc4, Name),
            Enc5 = #enc{core = EncName} = encode(Enc4, Name),
            Data = <<
                EncModule/bytes,
                EncName/bytes,
                (argo_varint:write_zigzag_u64(argo_types:dynamic_cast(Arity)))/bytes
            >>,
            Length = byte_size(Data),
            Enc6 = Enc5#enc{core = Core1},
            Block2 = write_block(Block1, Data),
            Enc7 = write_core(Enc6, <<Key:8, (argo_varint:write_zigzag_i64(Length))/bytes>>),
            {Enc7, Block2}
    end.

encode_function_ext(Enc1 = #enc{core = Core1}, Block1 = #block{}, Key, Function) ->
    case maps:from_list(erlang:fun_info(Function)) of
        #{type := local, module := Module, arity := Arity, env := Env, pid := Pid, new_index := NewIndex, new_uniq := <<NewUniq:16/bytes>>, index := OldIndex, uniq := OldUniq} ->
            Enc2 = Enc1#enc{core = <<>>},
            % Enc3 = #enc{core = <<?ATOM_EXT:8, EncModule/bytes>>} = encode(Enc2, Module),
            Enc3 = #enc{core = EncModule} = encode(Enc2, Module),
            Enc4 = Enc3#enc{core = <<>>},
            % Enc5 = #enc{core = <<?ATOM_EXT:8, EncName/bytes>>} = encode(Enc4, Name),
            % Enc6 = Enc5#enc{core = <<>>},
            Enc5 = #enc{core = <<?PID_EXT:8, EncPid/bytes>>} = encode(Enc4, Pid),
            Enc6 = Enc5#enc{core = <<>>},
            Enc7 = #enc{core = EncEnv} = encode_list(Enc6, Env),
            Data = <<
                EncModule/bytes,
                % EncName/bytes,
                (argo_varint:write_zigzag_u64(argo_types:dynamic_cast(Arity)))/bytes,
                NewUniq:16/bytes,
                (argo_varint:write_zigzag_u64(argo_types:dynamic_cast(NewIndex)))/bytes,
                (argo_varint:write_zigzag_u64(argo_types:dynamic_cast(OldUniq)))/bytes,
                (argo_varint:write_zigzag_u64(argo_types:dynamic_cast(OldIndex)))/bytes,
                EncPid/bytes,
                EncEnv/bytes
            >>,
            Length = byte_size(Data),
            Enc8 = Enc7#enc{core = Core1},
            Block2 = write_block(Block1, Data),
            Enc9 = write_core(Enc8, <<Key:8, (argo_varint:write_zigzag_i64(Length))/bytes>>),
            {Enc9, Block2}
    end.

encode_pid_ext(Enc1 = #enc{core = Core1}, Block1 = #block{}, Key, Pid) ->
    VERSION_MAGIC = 131,
    NEW_PID_EXT = $X,
    SMALL_ATOM_UTF8_EXT = $w,
    case erlang:term_to_binary(Pid, [{minor_version, 2}]) of
        <<VERSION_MAGIC:8, NEW_PID_EXT:8, SMALL_ATOM_UTF8_EXT:8, NodeLen:8, NodeBin:NodeLen/bytes, Id:32, Serial:32, Creation:32>> ->
            Node = erlang:binary_to_existing_atom(NodeBin, utf8),
            Enc2 = Enc1#enc{core = <<>>},
            Enc3 = #enc{core = <<_:8, EncNode/bytes>>} = encode(Enc2, Node),
            % Enc3 = #enc{core = <<?ATOM_EXT:8, EncNode/bytes>>} = encode(Enc2, Node),
            Data = <<
                EncNode/bytes,
                (argo_varint:write_zigzag_u64(Id))/bytes,
                (argo_varint:write_zigzag_u64(Serial))/bytes,
                (argo_varint:write_zigzag_u64(Creation))/bytes
            >>,
            Length = byte_size(Data),
            Enc4 = Enc3#enc{core = Core1},
            Block2 = write_block(Block1, Data),
            Enc5 = write_core(Enc4, <<Key:8, (argo_varint:write_zigzag_i64(Length))/bytes>>),
            {Enc5, Block2}
    end.

encode_port_ext(Enc1 = #enc{core = Core1}, Block1 = #block{}, Key, Port) ->
    VERSION_MAGIC = 131,
    V4_PORT_EXT = $x,
    SMALL_ATOM_UTF8_EXT = $w,
    case erlang:term_to_binary(Port, [{minor_version, 2}]) of
        <<VERSION_MAGIC:8, V4_PORT_EXT:8, SMALL_ATOM_UTF8_EXT:8, NodeLen:8, NodeBin:NodeLen/bytes, Id:64, Creation:32>> ->
            Node = erlang:binary_to_existing_atom(NodeBin, utf8),
            Enc2 = Enc1#enc{core = <<>>},
            % Enc3 = #enc{core = <<?ATOM_EXT:8, EncNode/bytes>>} = encode(Enc2, Node),
            Enc3 = #enc{core = EncNode} = encode(Enc2, Node),
            Data = <<
                EncNode/bytes,
                (argo_varint:write_zigzag_u64(Id))/bytes,
                (argo_varint:write_zigzag_u64(Creation))/bytes
            >>,
            Length = byte_size(Data),
            Enc4 = Enc3#enc{core = Core1},
            Block2 = write_block(Block1, Data),
            Enc5 = write_core(Enc4, <<Key:8, (argo_varint:write_zigzag_i64(Length))/bytes>>),
            {Enc5, Block2}
    end.

encode_reference_ext(Enc1 = #enc{core = Core1}, Block1 = #block{}, Key, Reference) ->
    VERSION_MAGIC = 131,
    NEWER_REFERENCE_EXT = $Z,
    SMALL_ATOM_UTF8_EXT = $w,
    case erlang:term_to_binary(Reference, [{minor_version, 2}]) of
        <<VERSION_MAGIC:8, NEWER_REFERENCE_EXT:8, Len:16, SMALL_ATOM_UTF8_EXT:8, NodeLen:8, NodeBin:NodeLen/bytes, Creation:32, EncodedIds:(Len * 32)/bits>> ->
            Node = erlang:binary_to_existing_atom(NodeBin, utf8),
            Enc2 = Enc1#enc{core = <<>>},
            % Enc3 = #enc{core = <<_:8, EncNode/bytes>>} = encode(Enc2, Node),
            Enc3 = #enc{core = EncNode} = encode(Enc2, Node),
            % Enc3 = #enc{core = <<?ATOM_EXT:8, EncNode/bytes>>} = encode(Enc2, Node),
            Ids = <<<<(argo_varint:write_zigzag_u64(Id))/bytes>> || <<Id:32>> <= EncodedIds>>,
            Data = <<
                EncNode/bytes,
                (argo_varint:write_zigzag_u64(Creation))/bytes,
                (argo_varint:write_zigzag_u64(Len))/bytes,
                Ids/bytes
            >>,
            Length = byte_size(Data),
            Enc4 = Enc3#enc{core = Core1},
            Block2 = write_block(Block1, Data),
            Enc5 = write_core(Enc4, <<Key:8, (argo_varint:write_zigzag_i64(Length))/bytes>>),
            {Enc5, Block2}
    end.

encode_string_ext(Enc1 = #enc{}, Block1 = #block{}, Key, Value) ->
    Data = Value,
    Length = byte_size(Data),
    Block2 = write_block(Block1, Data),
    Enc2 = write_core(Enc1, <<Key:8, (argo_varint:write_zigzag_i64(Length))/bytes>>),
    {Enc2, Block2}.

encode_varint_ext(Enc1 = #enc{}, Block1 = #block{}, Key, Value) ->
    Data = argo_varint:write_zigzag_i64(Value),
    % Length = byte_size(Data),
    Block2 = write_block(Block1, Data),
    Enc2 = write_core(Enc1, <<Key:8>>),
    {Enc2, Block2}.

encode(Enc1 = #enc{atoms = Cache}, Atom) when is_atom(Atom) ->
    case maps:find(Atom, Cache) of
        {ok, CacheIndex} ->
            update_block_with(Enc1, ?ATOM_CACHE_REF, CacheIndex, fun encode_atom_cache_ref/4);
        error ->
            update_block_with(Enc1, ?ATOM_EXT, Atom, fun encode_atom_ext/4)
    end;
encode(Enc1 = #enc{}, Binary) when is_binary(Binary) ->
    update_block_with(Enc1, ?BINARY_EXT, Binary, fun encode_binary_ext/4);
encode(Enc1 = #enc{}, Binary) when is_bitstring(Binary) ->
    update_block_with(Enc1, ?BITSTRING_EXT, Binary, fun encode_bits_ext/4);
encode(Enc1 = #enc{}, Float64) when is_float(Float64) ->
    write_core(Enc1, <<?FLOAT64_EXT:8, Float64:1/float-little-unit:64>>);
encode(Enc1 = #enc{}, Function) when is_function(Function) ->
    case erlang:fun_info(Function, type) of
        {type, external} ->
            update_block_with(Enc1, ?EXPORT_EXT, Function, fun encode_export_ext/4);
        {type, local} ->
            update_block_with(Enc1, ?FUNCTION_EXT, Function, fun encode_function_ext/4)
    end;
encode(Enc1 = #enc{}, Varint) when ?is_i64(Varint) ->
    update_block_with(Enc1, ?VARINT_EXT, fun(Enc2, Block1) -> encode_varint_ext(Enc2, Block1, ?VARINT_EXT, Varint) end);
    % write_core(Enc1, <<?VARINT_EXT:8, (argo_varint:write_zigzag_i64(Varint))/bytes>>);
encode(Enc1 = #enc{}, Integer) when is_integer(Integer) ->
    VERSION_MAGIC = 131,
    SMALL_BIG_EXT = $n,
    LARGE_BIG_EXT = $o,
    case erlang:term_to_binary(Integer, [{minor_version, 2}]) of
        <<VERSION_MAGIC:8, SMALL_BIG_EXT:8, N:8, 0:8, D:N/bytes>> ->
            write_core(Enc1, <<?BIGNUM_POS_EXT:8, (argo_varint:write_zigzag_u64(N))/bytes, D:N/bytes>>);
        <<VERSION_MAGIC:8, SMALL_BIG_EXT:8, N:8, 1:8, D:N/bytes>> ->
            write_core(Enc1, <<?BIGNUM_NEG_EXT:8, (argo_varint:write_zigzag_u64(N))/bytes, D:N/bytes>>);
        <<VERSION_MAGIC:8, LARGE_BIG_EXT:8, N:32, 0:8, D:N/bytes>> ->
            write_core(Enc1, <<?BIGNUM_POS_EXT:8, (argo_varint:write_zigzag_u64(N))/bytes, D:N/bytes>>);
        <<VERSION_MAGIC:8, LARGE_BIG_EXT:8, N:32, 1:8, D:N/bytes>> ->
            write_core(Enc1, <<?BIGNUM_NEG_EXT:8, (argo_varint:write_zigzag_u64(N))/bytes, D:N/bytes>>)
    end;
encode(Enc1 = #enc{}, Nil) when Nil =:= [] ->
    write_core(Enc1, <<?NIL_EXT:8>>);
encode(Enc1 = #enc{}, ListOrString) when is_list(ListOrString) ->
    case io_lib:printable_latin1_list(ListOrString) of
        false ->
            case io_lib:printable_unicode_list(ListOrString) of
                false ->
                    encode_list(Enc1, ListOrString);
                true ->
                    String = argo_types:unicode_binary(ListOrString),
                    update_block_with(Enc1, ?STRING_EXT, String, fun encode_string_ext/4)
            end;
        true ->
            String = erlang:list_to_binary(ListOrString),
            update_block_with(Enc1, ?STRING_EXT, String, fun encode_string_ext/4)
    end;
encode(Enc1 = #enc{}, Map) when is_map(Map) ->
    MapSize = maps:size(Map),
    Enc2 = write_core(Enc1, <<?MAP_EXT:8, (argo_varint:write_zigzag_u64(MapSize))/bytes>>),
    Enc3 = maps:fold(fun(Key, Value, Enc2_Acc1) ->
        Enc2_Acc2 = encode(Enc2_Acc1, Key),
        Enc2_Acc3 = encode(Enc2_Acc2, Value),
        Enc2_Acc3
    end, Enc2, Map),
    Enc3;
encode(Enc1 = #enc{}, Pid) when is_pid(Pid) ->
    update_block_with(Enc1, ?PID_EXT, Pid, fun encode_pid_ext/4);
encode(Enc1 = #enc{}, Port) when is_port(Port) ->
    update_block_with(Enc1, ?PORT_EXT, Port, fun encode_port_ext/4);
encode(Enc1 = #enc{}, Reference) when is_reference(Reference) ->
    update_block_with(Enc1, ?REFERENCE_EXT, Reference, fun encode_reference_ext/4);
encode(Enc1 = #enc{}, Tuple) when is_tuple(Tuple) ->
    TupleSize = tuple_size(Tuple),
    Enc2 = write_core(Enc1, <<?TUPLE_EXT:8, (argo_varint:write_zigzag_u64(TupleSize))/bytes>>),
    Enc3 = lists:foldl(fun(Element, Enc2_Acc1) ->
        Enc2_Acc2 = encode(Enc2_Acc1, Element),
        Enc2_Acc2
    end, Enc2, tuple_to_list(Tuple)),
    Enc3.

% encode_block_value(Enc1 = #enc{blocks = Blocks1}, BlockKey, BlockValue) ->
%     case argo_index_map:find(BlockKey, Blocks1) of
%         {ok, Block1} ->
%             encode_block_value(Enc1, BlockKey, Block1, BlockValue);
%         error ->
%             Block1 = #block{},
%             Blocks2 = argo_index_map:put(BlockKey, Block1, Blocks1),
%             Enc2 = Enc1#enc{blocks = Blocks2},
%             encode_block_value(Enc2, BlockKey, BlockValue)
%     end.

% encode_block_value(Enc1 = #enc{blocks = Blocks1, core = Core1}, BlockKey = ?ATOM_EXT, Block1 = #block{seen = Seen1, values = Values1}, Term) when is_atom(Term) ->
%     case argo_index_set:find_index_of(Term, Seen1) of
%         {ok, Index} ->
%             Core2 = <<Core1/bytes, BlockKey:8, (argo_varint:write_zigzag_i64(-1 - Index))/bytes>>,
%             Enc2 = Enc1#enc{core = Core2},
%             Enc2;
%         error ->
%             Seen2 = argo_index_set:add_element(Term, Seen1),
%             Value = erlang:atom_to_binary(Term, utf8),
%             Length = byte_size(Value),
%             Values2 = <<Values1/bytes, Value/bytes>>,
%             Block2 = Block1#block{seen = Seen2, values = Values2},
%             Blocks2 = argo_index_map:put(BlockKey, Block2, Blocks1),
%             Core2 = <<Core1/bytes, BlockKey:8, (argo_varint:write_zigzag_i64(Length))/bytes>>,
%             Enc2 = Enc1#enc{blocks = Blocks2, core = Core2},
%             Enc2
%     end;
% encode_block_value(Enc1 = #enc{blocks = Blocks1, core = Core1}, BlockKey = ?STRING_EXT, Block1 = #block{seen = Seen1, values = Values1}, Term) when is_list(Term) ->
%     case argo_index_set:find_index_of(Term, Seen1) of
%         {ok, Index} ->
%             Core2 = <<Core1/bytes, BlockKey:8, (argo_varint:write_zigzag_i64(-1 - Index))/bytes>>,
%             Enc2 = Enc1#enc{core = Core2},
%             Enc2;
%         error ->
%             Seen2 = argo_index_set:add_element(Term, Seen1),
%             Value = << <<(argo_varint:write_zigzag_u64(Char))/bytes>> || Char <- Term >>,
%             Length = byte_size(Value),
%             Values2 = <<Values1/bytes, Value/bytes>>,
%             Block2 = Block1#block{seen = Seen2, values = Values2},
%             Blocks2 = argo_index_map:put(BlockKey, Block2, Blocks1),
%             Core2 = <<Core1/bytes, BlockKey:8, (argo_varint:write_zigzag_i64(Length))/bytes>>,
%             Enc2 = Enc1#enc{blocks = Blocks2, core = Core2},
%             Enc2
%     end.

encode_list(Enc1 = #enc{}, List) ->
    {Heads, Tail, Size} = list_size(List),
    Enc2 = write_core(Enc1, <<?LIST_EXT:8, (argo_varint:write_zigzag_u64(Size))/bytes>>),
    Enc3 = lists:foldl(fun(Element, Enc2_Acc1) ->
        Enc2_Acc2 = encode(Enc2_Acc1, Element),
        Enc2_Acc2
    end, Enc2, Heads),
    Enc4 = encode(Enc3, Tail),
    Enc4.

list_size(List) when is_list(List) ->
    list_size(List, [], 0).

list_size([H | T], Heads, Size) ->
    list_size(T, [H | Heads], Size + 1);
list_size(Tail, Heads, Size) ->
    {lists:reverse(Heads), Tail, Size}.

get_block(Enc1 = #enc{blocks = Blocks1}, Key) ->
    case argo_index_map:find(Key, Blocks1) of
        {ok, Block1} ->
            {Enc1, Block1};
        error ->
            Block1 = #block{},
            Blocks2 = argo_index_map:put(Key, Block1, Blocks1),
            Enc2 = Enc1#enc{blocks = Blocks2},
            {Enc2, Block1}
    end.

update_block_normalize(Enc1 = #enc{blocks = Blocks1}, Key, Block1 = #block{}) ->
    Blocks2 = argo_index_map:put(Key, Block1, Blocks1),
    Enc2 = Enc1#enc{blocks = Blocks2},
    Enc2;
update_block_normalize(_Enc1 = #enc{}, _Key, Enc2 = #enc{}) ->
    Enc2;
update_block_normalize(_Enc1 = #enc{}, Key, {Enc2 = #enc{blocks = Blocks2}, Block1 = #block{}}) ->
    Blocks3 = argo_index_map:put(Key, Block1, Blocks2),
    Enc3 = Enc2#enc{blocks = Blocks3},
    Enc3.

update_block_with(Enc1 = #enc{}, Key, UpdateFun) ->
    {Enc2 = #enc{}, Block1 = #block{}} = get_block(Enc1, Key),
    update_block_normalize(Enc2, Key, UpdateFun(Enc2, Block1)).

update_block_with(Enc1 = #enc{}, Key, Value, UpdateFun) ->
    update_block_with(Enc1, Key, fun(Enc2, Block1 = #block{values = Values1}) ->
        case argo_index_set:find_index_of(Value, Values1) of
            {ok, Index} ->
                write_core(Enc1, <<Key:8, (argo_varint:write_zigzag_i64(-4 - Index))/bytes>>);
            error ->
                Values2 = argo_index_set:add_element(Value, Values1),
                Block2 = Block1#block{values = Values2},
                update_block_normalize(Enc2, Key, UpdateFun(Enc2, Block2, Key, Value))
        end
    end).

write_block(Block1 = #block{buffer = Buffer1}, Data) ->
    Buffer2 = <<Buffer1/bytes, Data/bytes>>,
    Block2 = Block1#block{buffer = Buffer2},
    Block2.

% write_block_and_core_length(Block1 = #block{buffer = Buffer1}, Data) ->
%     Length =

write_core(Enc1 = #enc{core = Core1}, Data) ->
    Core2 = <<Core1/bytes, Data/bytes>>,
    Enc2 = Enc1#enc{core = Core2},
    Enc2.