%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1997-2025. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
-module(disk_log_SUITE).

%%-define(debug, true).

-ifdef(debug).
-define(format(S, A), io:format(S, A)).
-define(line, put(line, ?LINE), ).
-define(privdir(_), "./disk_log_SUITE_priv").
-define(datadir(_), "./disk_log_SUITE_data").
-define(config(X,Y), foo).
-else.
-include_lib("common_test/include/ct.hrl").
-define(format(S, A), ok).
-define(privdir(Conf), proplists:get_value(priv_dir, Conf)).
-define(datadir(Conf), proplists:get_value(data_dir, Conf)).
-endif.

-compile(export_all).

-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1, 
	 init_per_group/2,end_per_group/2, 

	 halt_int_inf/1, 
	 halt_int_sz_1/1, halt_int_sz_2/1,

	 halt_int_ro/1, halt_ext_ro/1, wrap_int_ro/1, 
	 wrap_ext_ro/1, halt_trunc/1, halt_misc/1, halt_ro_alog/1, 
	 halt_ro_balog/1, halt_ro_crash/1,

	 wrap_int_1/1, wrap_int_2/1, inc_wrap_file/1,

	 halt_ext_inf/1,

	 halt_ext_sz_1/1, halt_ext_sz_2/1,

	 wrap_ext_1/1, wrap_ext_2/1,

	 rotate_1/1, rotate_truncate/1, rotate_reopen/1,
         rotate_breopen/1, next_rotate_file/1,

	 head_func/1, plain_head/1, one_header/1,

	 wrap_notif/1, full_notif/1, trunc_notif/1, blocked_notif/1,

	 new_idx_vsn/1, 

	 reopen/1, 

	 block_blocked/1, block_queue/1, block_queue2/1,

	 unblock/1,

	 open_overwrite/1, open_size/1, open_change_size/1,
         open_truncate/1, open_error/1,

	 close_race/1, close_block/1, close_deadlock/1,

	 error_repair/1, error_log/1, error_index/1,

	 chunk/1, 

	 truncate/1,

	 many_users/1,

	 info_current/1, 

	 change_size_before/1, change_size_during/1, 
	 change_size_after/1, default_size/1, change_size2/1,
	 change_size_truncate/1,

	 change_attribute/1,

         otp_6278/1, otp_10131/1, otp_16768/1, otp_16809/1,
        
         decrease_size_with_chunk_step/1, decrease_size_twice/1]).

-export([head_fun/1, hf/0, hf_bin/0, lserv/1, 
	 measure/0, init_m/1, xx/0]).

-export([init_per_testcase/2, end_per_testcase/2]).

-export([try_unblock/1]).

-export([client/4]).

%% error_logger
-export([init/1,
	 handle_event/2, handle_call/2, handle_info/2,
	 terminate/2]).

-include_lib("kernel/include/file.hrl").
-include_lib("kernel/src/disk_log.hrl").

%% TODO (old):
%%   - global logs
%%   - badarg
%%   - force file:write fail (how?)
%%   - kill logging proc while he is logging
%%   - kill logging node while he is logging
%%   - test chunk_step

%% These are all tests, the list to be returned by all().
-define(ALL_TESTS,
	[halt_int, wrap_int, halt_ext, wrap_ext, read_mode, head,
	 notif, new_idx_vsn, reopen, block, unblock, open, close,
	 error, chunk, truncate, many_users, info, change_size,
	 open_change_size, change_attribute, otp_6278, otp_10131,
         otp_16768, otp_16809, rotate]).


suite() ->
    [{ct_hooks,[ts_install_cth]},
     {timetrap,{minutes,2}}].

all() -> 
    [{group, halt_int}, {group, wrap_int},
     {group, halt_ext}, {group, wrap_ext}, {group, rotate},
     {group, read_mode}, {group, head}, {group, notif},
     new_idx_vsn, reopen, {group, block}, unblock,
     {group, open}, {group, close}, {group, error}, chunk,
     truncate, many_users, {group, info},
     {group, change_size}, change_attribute,
     otp_6278, otp_10131, otp_16768, otp_16809].

groups() -> 
    [{halt_int, [], [halt_int_inf, {group, halt_int_sz}]},
     {halt_int_sz, [], [halt_int_sz_1, halt_int_sz_2]},
     {read_mode, [],
      [halt_int_ro, halt_ext_ro, wrap_int_ro, wrap_ext_ro,
       halt_trunc, halt_misc, halt_ro_alog, halt_ro_balog,
       halt_ro_crash]},
     {wrap_int, [], [wrap_int_1, wrap_int_2, inc_wrap_file]},
     {halt_ext, [], [halt_ext_inf, {group, halt_ext_sz}]},
     {halt_ext_sz, [], [halt_ext_sz_1, halt_ext_sz_2]},
     {wrap_ext, [], [wrap_ext_1, wrap_ext_2]},
     {rotate, [],
      [rotate_1, rotate_truncate, rotate_reopen,
       rotate_breopen, next_rotate_file]},
     {head, [], [head_func, plain_head, one_header]},
     {notif, [],
      [wrap_notif, full_notif, trunc_notif, blocked_notif]},
     {block, [], [block_blocked, block_queue, block_queue2]},
     {open, [],
      [open_overwrite, open_size, open_change_size, open_truncate,
       open_error]},
     {close, [], [close_race, close_block, close_deadlock]},
     {error, [], [error_repair, error_log, error_index]},
     {info, [], [info_current]},
     {change_size, [],
      [change_size_before, change_size_during,
       change_size_after, default_size, change_size2,
       change_size_truncate, decrease_size_with_chunk_step,
       decrease_size_twice]}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.



init_per_testcase(_Case, Config) ->
    Config.

end_per_testcase(_Case, _Config) ->
    ok.


%% Test simple halt disk log, size infinity.
halt_int_inf(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    ok = disk_log:start(),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal},
			     {file, File}]),
    simple_log(a),
    ok = disk_log:close(a),
    ok = file:delete(File).


%% Test simple halt disk log, size defined.
halt_int_sz_1(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,18000},
			     {format,internal},
			     {file, File}]),
    simple_log(a),
    ok = disk_log:truncate(a),
    [] = get_all_terms(a),
    T1 = mk_bytes(10000),
    T2 = mk_bytes(5000),
    ok = disk_log:log(a, T1),
    case get_all_terms(a) of
	[T1] ->
	    ok;
	E1 ->
	    test_server_fail({bad_terms, E1, [T1]})
    end,
    ok = disk_log:log(a, T2),
    {error, {full, a}} = disk_log:log(a, T1),
    ok = disk_log:alog(a, T1),
    case get_all_terms(a) of
	[T1, T2] ->
	    ok;
	E2 ->
	    test_server_fail({bad_terms, E2, [T1, T2]})
    end,
    ok = disk_log:close(a),
    ok = file:delete(File).

%% Test simple halt disk log, size ~8192.
halt_int_sz_2(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "b.LOG"),
    File3 = filename:join(Dir, "c.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,8191},
			     {format,internal},
			     {file, File1}]),
    {ok, b} = disk_log:open([{name,b}, {type,halt}, {size,8192},
			     {format,internal},
			     {file, File2}]),
    {ok, c} = disk_log:open([{name,c}, {type,halt}, {size,8193},
			     {format,internal},
			     {file, File3}]),
    T1 = mk_bytes(8191-16), % 16 is size of header + magics for 1 item
    T2 = mk_bytes(8192-16),
    T3 = mk_bytes(8193-16),
    ok = disk_log:log(a, T1),
    ok = disk_log:log(b, T2),
    ok = disk_log:log(c, T3),
    case get_all_terms(a) of
	[T1] ->
	    ok;
	E1 ->
	    test_server_fail({bad_terms, E1, [T1]})
    end,
    case get_all_terms(b) of
	[T2] ->
	    ok;
	E2 ->
	    test_server_fail({bad_terms, E2, [T2]})
    end,
    case get_all_terms(c) of
	[T3] ->
	    ok;
	E3 ->
	    test_server_fail({bad_terms, E3, [T3]})
    end,
    ok = disk_log:truncate(a),
    ok = disk_log:truncate(b),
    {error, {full, a}} = disk_log:log(a, T2),
    {error, {full, b}} = disk_log:log(b, T3),
    [] = get_all_terms(a),
    [] = get_all_terms(b),
    ok = disk_log:close(a),
    ok = disk_log:close(b),
    ok = disk_log:close(c),
    ok = file:delete(File1),
    ok = file:delete(File2),
    ok = file:delete(File3),
    ok.


%% Test simple halt disk log, read only, internal.
halt_int_ro(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),

    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal}, {file, File}]),
    simple_log(a),
    ok = disk_log:close(a),

    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal}, {file, File},
			     {mode,read_only}]),
    T1 = "not allowed to write",
    {error, {read_only_mode, a}} = disk_log:log(a, T1),
    ok = disk_log:close(a),
    ok = file:delete(File).

%% Test simple halt disk log, read only, external.
halt_ext_ro(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,external}, {file, File}]),
    xsimple_log(File, a),
    ok = disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,external}, {file, File},
			     {mode,read_only}]),
    T1 = "not allowed to write",
    {error, {read_only_mode, a}}  = disk_log:blog(a, T1),
    ok = disk_log:close(a),
    ok = file:delete(File).

%% Test simple wrap disk log, read only, internal.
wrap_int_ro(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
			     {format,internal}, {file, File}]),
    simple_log(a),
    ok = disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
			     {format,internal}, {file, File}, {mode,read_only}]),
    T1 = "not allowed to write",
    {error, {read_only_mode, a}} = disk_log:log(a, T1),
    ok = disk_log:close(a),
    del(File, 4).

%% Test simple wrap disk log, read only, external.
wrap_ext_ro(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
			     {format,external}, {file, File}]),
    x2simple_log(File ++ ".1", a),
    ok = disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
			     {format,external}, {file, File},
			     {mode,read_only}]),
    T1 = "not allowed to write",
    {error, {read_only_mode, a}}  = disk_log:blog(a, T1),
    {error, {read_only_mode, a}}  = disk_log:inc_wrap_file(a),
    ok = disk_log:close(a),
    del(File, 4).

%% Test truncation of halt disk log.
halt_trunc(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal}, {file, File}]),
    simple_log(a),
    ok = disk_log:close(a),
    {error,{badarg,repair_read_only}} =
	disk_log:open([{name,a}, {type,halt}, {size,infinity},
		       {repair, truncate}, {format,internal},
		       {file, File}, {mode,read_only}]),
    ok = file:delete(File).

%% Test truncation of halt disk log.
halt_misc(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal}, {file, File}]),
    simple_log(a),
    ok = disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal}, {file, File},
			     {mode,read_only}]),
    T1 = "not allowed to write",
    {error, {read_only_mode, a}} = disk_log:log(a, T1),
    {error, {read_only_mode, a}} = disk_log:sync(a),
    {error, {read_only_mode, a}} = disk_log:reopen(a, "b.LOG"),
    {error, {read_only_mode, a}} =
        disk_log:change_header(a, {head,header}),
    {error, {read_only_mode, a}} =
        disk_log:change_size(a, infinity),
    ok = disk_log:close(a),
    ok = file:delete(File).

%% Test truncation of halt disk log, read only.
halt_ro_alog(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal}, {file, File}]),
    simple_log(a),
    ok = disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {notify,true}, {format,internal},
			     {file, File}, {mode,read_only}]),
    T1 = "not allowed to write",
    ok = disk_log:alog(a, T1),
    ok = halt_ro_alog_wait_notify(a, T1),
    ok = disk_log:close(a),
    ok = file:delete(File).

halt_ro_alog_wait_notify(Log, T) ->
    Term = term_to_binary(T),
    receive
	{disk_log, _, Log,{read_only, [Term]}} ->
	    ok;
	Other ->
	    Other
    after 5000 ->
	    failed
    end.

%% Test truncation of halt disk log, read only.
halt_ro_balog(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal}, {file, File}]),
    simple_log(a),
    ok = disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {notify,true}, {format,external},
			     {file, File}, {mode,read_only}]),
    T1 = "not allowed to write",
    ok = disk_log:balog(a, T1),
    ok = halt_ro_balog_wait_notify(a, T1),
    ok = disk_log:close(a),
    ok = file:delete(File).

halt_ro_balog_wait_notify(Log, T) ->
    Term = list_to_binary(T),
    receive
	{disk_log, _, Log,{read_only, [Term]}} ->
	    ok;
	Other ->
	    Other
    after 5000 ->
	    failed
    end.

%% Test truncation of halt disk log, read only, repair.
halt_ro_crash(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),

    file:delete(File),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal},{file, File}]),
    simple_log(a),
    ok = disk_log:close(a),
    crash(File, 10),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {notify,true}, {format,internal},
			     {file, File}, {mode,read_only}]),

    Error1 = {error, {read_only_mode, a}} = disk_log:truncate(a),
    "The disk log" ++ _ = format_error(Error1),

    %% crash/1 sets the length of the first item to something big (2.5 kb).
    %% In R6B, binary_to_term accepts garbage at the end of the binary,
    %% which means that the first item is recognized!
    %% This is how it was before R6B:
    %% {C1,T1,15} = disk_log:chunk(a,start),
    %% {C2,T2} = disk_log:chunk(a,C1),
    {C1,_OneItem,7476} = disk_log:chunk(a,start),
    {C2, [], 7} = disk_log:chunk(a,C1),
    eof = disk_log:chunk(a,C2),
    ok = disk_log:close(a),
    ok = file:delete(File).





%% Test wrap disk log, internal.
wrap_int_1(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
			     {format,internal},
			     {file, File}]),
    [_] =
        lists:filter(fun(P) -> disk_log:pid2name(P) =/= undefined end, 
                     erlang:processes()),
    simple_log(a),
    ok = disk_log:close(a),
    del(File, 4),
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
			     {format,internal},
			     {file, File}]),
    [] = get_all_terms(a),
    T1 = mk_bytes(10000), % file 2
    T2 = mk_bytes(5000),  % file 3
    T3 = mk_bytes(4000),  % file 4
    T4 = mk_bytes(2000),  % file 4
    T5 = mk_bytes(5000),  % file 1
    T6 = mk_bytes(5000),  % file 2
    ok = disk_log:log(a, T1),
    ok = disk_log:log(a, T2),
    ok = disk_log:log(a, T3),
    ok = disk_log:log_terms(a, [T4, T5, T6]),
    case get_all_terms(a) of
	[T2,T3,T4,T5,T6] ->
	    ok;
	E1 ->
	    test_server_fail({bad_terms, E1, [T2,T3,T4,T5,T6]})
    end,
    ok = disk_log:close(a),
    del(File, 4).

%% Test wrap disk log, internal.
wrap_int_2(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "b.LOG"),
    File3 = filename:join(Dir, "c.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8191,3}},
			     {format,internal},
			     {file, File1}]),
    {ok, b} = disk_log:open([{name,b}, {type,wrap}, {size,{8192,3}},
			     {format,internal},
			     {file, File2}]),
    {ok, c} = disk_log:open([{name,c}, {type,wrap}, {size,{8193,3}},
			     {format,internal},
			     {file, File3}]),
    T1 = mk_bytes(8191-16), % 16 is size of header + magics for 1 item
    T2 = mk_bytes(8192-16),
    T3 = mk_bytes(8193-16),
    ok = disk_log:log(a, T1),
    ok = disk_log:log(b, T2),
    ok = disk_log:log(c, T3),
    case get_all_terms(a) of
	[T1] ->
	    ok;
	E1 ->
	    test_server_fail({bad_terms, E1, [T1]})
    end,
    case get_all_terms(b) of
	[T2] ->
	    ok;
	E2 ->
	    test_server_fail({bad_terms, E2, [T2]})
    end,
    case get_all_terms(c) of
	[T3] ->
	    ok;
	E3 ->
	    test_server_fail({bad_terms, E3, [T3]})
    end,
    ok = disk_log:close(a),
    ok = disk_log:close(b),
    ok = disk_log:close(c),
    del(File1, 3),
    del(File2, 3),
    del(File3, 3).

%% Test disk log, force a change to next file.
inc_wrap_file(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "b.LOG"),
    File3 = filename:join(Dir, "c.LOG"),

    %% Test that halt logs gets an error message
    {ok, a} = disk_log:open([{name, a}, {type, halt},
			     {format, internal},
			     {file, File1}]),
    ok = disk_log:log(a, "message one"),
    {error, {halt_log, a}} = disk_log:inc_wrap_file(a),

    %% test an internally formatted wrap log file
    {ok, b} = disk_log:open([{name, b}, {type, wrap}, {size, {100,3}},
			     {format, internal}, {head, 'thisisahead'},
			     {file, File2}]),
    ok = disk_log:log(b, "message one"),
    ok = disk_log:inc_wrap_file(b),
    ok = disk_log:log(b, "message two"),
    ok = disk_log:inc_wrap_file(b),
    ok = disk_log:log(b, "message three"),
    ok = disk_log:inc_wrap_file(b),
    ok = disk_log:log(b, "message four"),
    T1 = get_all_terms(b),
    ['thisisahead', "message two",
     'thisisahead', "message three",
     'thisisahead', "message four"] = T1,

    %% test an externally formatted wrap log file
    {ok, c} = disk_log:open([{name, c}, {type, wrap}, {size, {100,3}},
			     {format,external}, {head,"this is a head "},
			     {file, File3}]),
    ok = disk_log:blog(c, "message one"),
    ok = disk_log:inc_wrap_file(c),
    ok = disk_log:blog(c, "message two"),
    ok = disk_log:inc_wrap_file(c),
    ok = disk_log:blog(c, "message three"),
    ok = disk_log:inc_wrap_file(c),
    ok = disk_log:blog(c, "message four"),
    ok = disk_log:sync(c),
    {ok, Fd31} = file:open(File3 ++ ".1", [read]),
    {ok,"this is a head message four"} = file:read(Fd31, 200),
    {ok, Fd32} = file:open(File3 ++ ".2", [read]),
    {ok,"this is a head message two"} = file:read(Fd32, 200),
    {ok, Fd33} = file:open(File3 ++ ".3", [read]),
    {ok,"this is a head message three"} = file:read(Fd33, 200),
    ok = file:close(Fd31),
    ok = file:close(Fd32),
    ok = file:close(Fd33),

    ok = disk_log:close(a),
    ok = disk_log:close(b),
    ok = disk_log:close(c),
    ok = file:delete(File1),
    del(File2, 3),
    del(File3, 3).




%% Test halt disk log, external, infinity.
halt_ext_inf(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,external},
			     {file, File}]),
    xsimple_log(File, a),
    ok = disk_log:close(a),
    ok = file:delete(File).


%% Test halt disk log, external, size defined.
halt_ext_sz_1(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,18000},
			     {format,external},
			     {file, File}]),
    xsimple_log(File, a),
    ok = disk_log:truncate(a),
    [] = get_list(File, a),
    {B1, T1} = x_mk_bytes(10000),
    {B2, T2} = x_mk_bytes(5000),
    {B3, T3} = x_mk_bytes(1000),
    ok = disk_log:blog(a, B1),
    case get_list(File, a) of
	T1 ->
	    ok;
	E1 ->
	    test_server_fail({bad_terms, E1, T1})
    end,
    ok = disk_log:blog(a, B2),
    {error, {full, a}} = disk_log:blog_terms(a, [B3,B3,B1]),
    ok = disk_log:balog(a, B1),
    Tmp = T1 ++ T2 ++ T3 ++ T3,
    case get_list(File, a) of
	Tmp ->
	    ok;
	E2 ->
	    test_server_fail({bad_terms, E2, Tmp})
    end,
    ok = disk_log:close(a),
    ok = file:delete(File).

%% Test halt disk log, external, size defined.
halt_ext_sz_2(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "b.LOG"),
    File3 = filename:join(Dir, "c.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,8191},
			     {format,external},
			     {file, File1}]),
    {ok, b} = disk_log:open([{name,b}, {type,halt}, {size,8192},
			     {format,external},
			     {file, File2}]),
    {ok, c} = disk_log:open([{name,c}, {type,halt}, {size,8193},
			     {format,external},
			     {file, File3}]),
    {B1, T1} = x_mk_bytes(8191),
    {B2, T2} = x_mk_bytes(8192),
    {B3, T3} = x_mk_bytes(8193),
    ok = disk_log:blog(a, B1),
    ok = disk_log:blog(b, B2),
    ok = disk_log:blog(c, B3),
    case get_list(File1, a) of
	T1 ->
	    ok;
	E1 ->
	    test_server_fail({bad_terms, E1, T1})
    end,
    case get_list(File2, b) of
	T2 ->
	    ok;
	E2 ->
	    test_server_fail({bad_terms, E2, T2})
    end,
    case get_list(File3, c) of
	T3 ->
	    ok;
	E3 ->
	    test_server_fail({bad_terms, E3, T3})
    end,
    ok = disk_log:truncate(a),
    ok = disk_log:truncate(b),
    {error, {full, a}} = disk_log:blog(a, B2),
    Error1 = {error, {full, b}} = disk_log:blog(b, B3),
    "The halt log" ++ _ = format_error(Error1),
    true = info(b, full, false),
    [] = get_list(File1, a),
    [] = get_list(File2, b),
    ok = disk_log:close(a),
    ok = disk_log:close(b),
    ok = disk_log:close(c),
    ok = file:delete(File1),
    ok = file:delete(File2),
    ok = file:delete(File3),
    ok.


%% Test wrap disk log, external, size defined.
wrap_ext_1(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
			     {format,external},
			     {file, File}]),
    x2simple_log(File ++ ".1", a),
    ok = disk_log:close(a),
    %%    del(File, 4),
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8000, 4}},
			     {format,external},
			     {file, File}]),
    {B1, _T1} = x_mk_bytes(10000), % file 2
    {B2, T2} = x_mk_bytes(5000),  % file 3
    {B3, T3} = x_mk_bytes(4000),  % file 4
    {B4, T4} = x_mk_bytes(2000),  % file 4
    {B5, T5} = x_mk_bytes(5000),  % file 1
    {B6, T6} = x_mk_bytes(5000),  % file 2
    ok = disk_log:blog(a, B1),
    ok = disk_log:blog(a, B2),
    ok = disk_log:blog(a, B3),
    ok = disk_log:blog_terms(a, [B4, B5, B6]),
    case get_list(File ++ ".3", a) of
	T2 ->
	    ok;
	E2 ->
	    test_server_fail({bad_terms, E2, T2})
    end,
    T34 = T3 ++ T4,
    case get_list(File ++ ".4", a) of
	T34 ->
	    ok;
	E34 ->
	    test_server_fail({bad_terms, E34, T34})
    end,
    case get_list(File ++ ".1", a) of
	T5 ->
	    ok;
	E5 ->
	    test_server_fail({bad_terms, E5, T5})
    end,
    case get_list(File ++ ".2", a) of
	T6 ->
	    ok;
	E6 ->
	    test_server_fail({bad_terms, E6, T6})
    end,
    ok = disk_log:close(a),
    del(File, 4).

%% Test wrap disk log, external, size defined.
wrap_ext_2(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "b.LOG"),
    File3 = filename:join(Dir, "c.LOG"),
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{8191,3}},
			     {format,external},
			     {file, File1}]),
    {ok, b} = disk_log:open([{name,b}, {type,wrap}, {size,{8192,3}},
			     {format,external},
			     {file, File2}]),
    {ok, c} = disk_log:open([{name,c}, {type,wrap}, {size,{8193,3}},
			     {format,external},
			     {file, File3}]),
    {B1, T1} = x_mk_bytes(8191),
    {B2, T2} = x_mk_bytes(8192),
    {B3, T3} = x_mk_bytes(8193),
    ok = disk_log:blog(a, B1),
    ok = disk_log:blog(b, B2),
    ok = disk_log:blog(c, B3),
    case get_list(File1 ++ ".1", a) of
	T1 ->
	    ok;
	E1 ->
	    test_server_fail({bad_terms, E1, T1})
    end,
    case get_list(File2 ++ ".1", b) of
	T2 ->
	    ok;
	E2 ->
	    test_server_fail({bad_terms, E2, T2})
    end,
    case get_list(File3 ++ ".1", c) of
	T3 ->
	    ok;
	E3 ->
	    test_server_fail({bad_terms, E3, T3})
    end,
    ok = disk_log:close(a),
    ok = disk_log:close(b),
    ok = disk_log:close(c),
    del(File1, 3),
    del(File2, 3),
    del(File3, 3),
    ok.

%% Test rotate disk log, external, size defined.
rotate_1(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    Name = a,
    {ok, Name} = disk_log:open([{name,Name}, {type,rotate}, {size,{8000, 3}},
			     {format,external},
			     {file, File}]),
    x2simple_log(File, Name),
    ok = disk_log:close(Name),
    del_rot_files(File, 4),
    {ok, Name} = disk_log:open([{name,Name}, {type,rotate}, {size,{8000, 3}},
                                {format,external},
                                {head_func, {?MODULE, hf_bin, []}},
                                {file, File}]),
    {B1, _T1} = x_mk_bytes(10000), % lost due to rotation 
    {B2, T2} = x_mk_bytes(5000),  % file a.LOG.2.gx 
    {B3, T3} = x_mk_bytes(4000),  % file a.LOG.1.gz 
    {B4, T4} = x_mk_bytes(2000),  % file a.LOG.1.gz
    {B5, T5} = x_mk_bytes(5000),  % file a.LOG.0.gz 
    {B6, T6} = x_mk_bytes(5000),  % in the active file 
    ok = disk_log:blog(Name, B1),
    ok = disk_log:blog(Name, B2),
    ok = disk_log:blog(Name, B3),
    ok = disk_log:blog_terms(a, [B4, B5, B6]),
    {ok, BinHeader} = hf_bin(),
    Header = binary_to_list(BinHeader),
    T20 = Header ++ T2,
    case get_list(File ++ ".2.gz", Name, rotate) of
        T20 ->
            ok;
        E2 ->
            test_server_fail({bad_terms, E2, T2})
    end,
    T34 = Header ++ T3 ++ T4,
    case get_list(File ++ ".1.gz", Name, rotate) of
        T34 ->
            ok;
        E34 ->
            test_server_fail({bad_terms, E34, T34})
    end,
    T50 = Header ++ T5,
    case get_list(File ++ ".0.gz", Name, rotate) of
        T50 ->
            ok;
        E5 ->
            test_server_fail({bad_terms, E5, T5})
    end,
    T60 = Header ++ T6,
    case get_list(File, Name) of
        T60 ->
            ok;
        E6 ->
            test_server_fail({bad_terms, E6, T6})
    end,
    ok = disk_log:close(Name),
    del_rot_files(File, 3).

%% test truncate/1 for rotate logs
rotate_truncate(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    Name = a,
    {ok, Name} = disk_log:open([{name,Name}, {type,rotate}, {size,{100, 3}},
			     {format,external},
			     {file, File}]),
    B = mk_bytes(60),
    ok = disk_log:blog_terms(Name, [B, B, B]),
    B = get_list(File, Name),
    B = get_list(File ++ ".0.gz", Name, rotate),
    B = get_list(File ++ ".1.gz", Name, rotate),
    ok = disk_log:truncate(Name),
    [] = get_list(File, Name),
    {error, enoent} = file:read_file_info(File ++ ".0.gz"),
    {error, enoent} = file:read_file_info(File ++ ".1.gz"),
    ok = disk_log:close(Name),
    file:delete(File).

%% test reopen/2 for rotate logs
rotate_reopen(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    Name = a,
    {ok, Name} = disk_log:open([{name,Name}, {type,rotate}, {size,{100, 3}},
			     {format,external},
			     {file, File}]),
    B = mk_bytes(60),
    ok = disk_log:blog_terms(Name, [B, B, B]),
    B = get_list(File, Name),
    B = get_list(File ++ ".0.gz", Name, rotate),
    B = get_list(File ++ ".1.gz", Name, rotate),
    File1 = filename:join(Dir, "b.LOG"),
    ok = disk_log:reopen(Name, File1),
    [] = get_list(File, Name),
    {error, enoent} = file:read_file_info(File ++ ".0.gz"),
    {error, enoent} = file:read_file_info(File ++ ".1.gz"),
    B = get_list(File1 ++ ".0.gz", Name, rotate),
    B = get_list(File1 ++ ".1.gz", Name, rotate),
    B = get_list(File1 ++ ".2.gz", Name, rotate),
    ok = disk_log:close(Name),
    file:delete(File),
    del_rot_files(File1, 3).

%% test breopen/3 for rotate logs
rotate_breopen(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    Name = a,
    Head1 = "thisishead1",
    {ok, Name} = disk_log:open([{name,Name}, {type,rotate}, {size,{100, 3}},
			     {format,external},
                             {head, Head1},
			     {file, File1}]),
    B = mk_bytes(60),
    ok = disk_log:blog_terms(Name, [B, B, B]),
    FileCont = Head1 ++ B,
    FileCont = get_list(File1, Name),
    FileCont = get_list(File1 ++ ".0.gz", Name, rotate),
    FileCont = get_list(File1 ++ ".1.gz", Name, rotate),
    File2 = filename:join(Dir, "b.LOG"),
    Head2 = "thisishead2",
    ok = disk_log:breopen(Name, File2, Head2),
    Head2 = get_list(File1, Name),
    {error, enoent} = file:read_file_info(File1 ++ ".0.gz"),
    {error, enoent} = file:read_file_info(File1 ++ ".1.gz"),
    FileCont = get_list(File2 ++ ".0.gz", Name, rotate),
    FileCont = get_list(File2 ++ ".1.gz", Name, rotate),
    FileCont = get_list(File2 ++ ".2.gz", Name, rotate),
    ok = disk_log:close(Name),
    file:delete(File1),
    del_rot_files(File2, 3).

%% Test rotate log, force a change to next file.
next_rotate_file(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File1 = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "b.LOG"),

    %% Test that halt and wrap logs get error messages
    {ok, a} = disk_log:open([{name, a}, {type, halt},
			     {format, internal},
			     {file, File1}]),
    ok = disk_log:log(a, "message one"),
    {error, {halt_log, a}} = disk_log:next_file(a),

    %% test a rotate log file
    {ok, b} = disk_log:open([{name, b}, {type, rotate}, {size, {100,3}},
			     {format,external},
			     {file, File2}]),
    ok = disk_log:blog(b, "message one"),
    ok = disk_log:next_file(b),
    ok = disk_log:blog(b, "message two"),
    ok = disk_log:next_file(b),
    ok = disk_log:blog(b, "message three"),
    ok = disk_log:next_file(b),
    ok = disk_log:blog(b, "message four"),
    ok = disk_log:sync(b),
    "message one" = get_list(File2 ++ ".2.gz", b, rotate),
    "message two" = get_list(File2 ++ ".1.gz", b, rotate),
    "message three" = get_list(File2 ++ ".0.gz", b, rotate),
    "message four" = get_list(File2, b),
    ok = disk_log:close(a),
    ok = disk_log:close(b),
    ok = file:delete(File1),
    del_rot_files(File2, 3).

simple_log(Log) ->
    T1 = "hej",
    T2 = hopp,
    T3 = {tjena, 12},
    T4 = mk_bytes(10000),
    ok = disk_log:log(Log, T1),
    ok = disk_log:log_terms(Log, [T2, T3]),
    case get_all_terms(Log) of
	[T1, T2, T3] ->
	    ok;
	E1 ->
	    test_server_fail({bad_terms, E1, [T1, T2, T3]})
    end,
    ok = disk_log:log(a, T4),
    case get_all_terms(Log) of
	[T1, T2, T3, T4] ->
	    ok;
	E2 ->
	    test_server_fail({bad_terms, E2, [T1, T2, T3, T4]})
    end.

xsimple_log(File, Log) ->
    T1 = "hej",
    T2 = list_to_binary("hopp"),
    T3 = list_to_binary(["sena", list_to_binary("sejer")]),
    T4 = list_to_binary(By = mk_bytes(10000)),
    ok = disk_log:blog(Log, T1),
    ok = disk_log:blog_terms(Log, [T2, T3]),
    X = "hejhoppsenasejer",
    X2 = get_list(File, Log),
    case X2 of
	X -> ok;
	Z1 -> test_server_fail({bad_terms, Z1, X2})
    end,
    ok = disk_log:blog(Log, T4),
    Tmp = get_list(File, Log),
    case X ++ By of
	Tmp -> ok;
	Z2 -> test_server_fail({bad_terms, Z2, X ++ By})
    end.

x2simple_log(File, Log) ->
    T1 = "hej",
    T2 = list_to_binary("hopp"),
    T3 = list_to_binary(["sena", list_to_binary("sejer")]),
    T4 = list_to_binary(By = mk_bytes(1000)),
    ok = disk_log:blog(Log, T1),
    ok = disk_log:blog_terms(Log, [T2, T3]),
    X = "hejhoppsenasejer",
    X2 = get_list(File, Log),
    case X2 of
	X -> ok;
	Z1 -> test_server_fail({bad_terms, Z1, X2})
    end,
    ok = disk_log:blog(Log, T4),
    Tmp = get_list(File, Log),
    case X ++ By of
	Tmp -> ok;
	Z2 -> test_server_fail({bad_terms, Z2, X ++ By})
    end.

x_mk_bytes(N) ->
    X = lists:duplicate(N, $a),
    {list_to_binary(X), X}.

mk_bytes(N) when N > 4 ->
    X = lists:duplicate(N-4, $a),
    case byte_size(term_to_binary(X)) of
        N -> X;
        Z -> test_server_fail({bad_terms, Z, N})
    end.

get_list(File, Log) ->
    ct:pal(?HI_VERBOSITY, "File ~p~n", [File]),
    ok = disk_log:sync(Log),
    {ok, B} = file:read_file(File),
    binary_to_list(B).

get_list(File, Log, rotate) ->
    ct:pal(?HI_VERBOSITY, "File ~p~n", [File]),
    ok = disk_log:sync(Log),
    DFile = filename:rootname(File,".gz"),
    decompress_file(File, DFile),
    {ok, B} = file:read_file(DFile),
    file:delete(DFile),
    binary_to_list(B).

decompress_file(FileName, DFileName) ->
    {ok,In} = file:open(FileName,[read,binary]),
    {ok,Out} = file:open(DFileName,[write]),
    Z = zlib:open(),
    zlib:inflateInit(Z, 31),
    decompress_data(Z,In,Out),
    zlib:inflateEnd(Z),
    zlib:close(Z),
    _ = file:close(In),
    _ = file:close(Out),
    ok.

decompress_data(Z,In,Out) ->
    case file:read(In,1000) of
        {ok,Data} ->
            Decompressed = zlib:inflate(Z, Data),
            _ = file:write(Out,Decompressed),
            decompress_data(Z,In,Out);
        eof ->
            ok
    end.


get_all_terms(Log, File, Type) ->
    {ok, _Log} = disk_log:open([{name,Log}, {type,Type}, {size,infinity},
				{format,internal}, {file, File},
				{mode, read_only}]),
    Ts = get_all_terms(Log),
    ok = disk_log:close(Log),
    Ts.

get_all_terms(Log) ->
    get_all_terms1(Log, start, []).

get_all_terms1(Log, Cont, Res) ->
    case disk_log:chunk(Log, Cont) of
	{error, _R} ->
	    test_server_fail({bad_chunk, Log, Cont});
	{Cont2, Terms} ->
	    get_all_terms1(Log, Cont2, Res ++ Terms);
	eof ->
	    Res
    end.

get_all_terms_and_bad(Log, File, Type) ->
    {ok, _Log} = disk_log:open([{name,Log}, {type,Type}, {size,infinity},
				{format,internal}, {file, File},
				{mode, read_only}]),
    Ts = get_all_terms_and_bad(Log),
    ok = disk_log:close(Log),
    Ts.

get_all_terms_and_bad(Log) ->
    read_only = info(Log, mode, foo),
    get_all_terms_and_bad1(Log, start, [], 0).

%% 
get_all_terms_and_bad1(Log, Cont, Res, Bad0) ->
    case disk_log:chunk(Log, Cont) of
	{Cont2, Terms} ->
	    get_all_terms_and_bad1(Log, Cont2, Res ++ Terms, Bad0);
	{Cont2, Terms, Bad} ->
	    get_all_terms_and_bad1(Log, Cont2, Res ++ Terms, Bad0+Bad);
	eof ->
	    {Res, Bad0}
    end.

get_all_binary_terms_and_bad(Log, File, Type) ->
    {ok, _Log} = disk_log:open([{name,Log}, {type,Type}, {size,infinity},
				{format,internal}, {file, File},
				{mode, read_only}]),
    Ts = get_all_binary_terms_and_bad(Log),
    ok = disk_log:close(Log),
    Ts.

get_all_binary_terms_and_bad(Log) ->
    read_only = info(Log, mode, foo),
    get_all_binary_terms_and_bad1(Log, start, [], 0).

%% 
get_all_binary_terms_and_bad1(Log, Cont, Res, Bad0) ->
    case disk_log:bchunk(Log, Cont) of
	{Cont2, BinTerms} ->
	    get_all_binary_terms_and_bad1(Log, Cont2, Res ++ BinTerms, Bad0);
	{Cont2, BinTerms, Bad} ->
	    get_all_binary_terms_and_bad1(Log, Cont2, Res ++ BinTerms, 
                                          Bad0+Bad);
	eof ->
	    {Res, Bad0}
    end.

del(File, 0) ->
    file:delete(File ++ ".siz"),
    file:delete(File ++ ".idx");
del(File, N) ->
    file:delete(File ++ "." ++ integer_to_list(N)),
    del(File, N-1).

del_rot_files(File, 0) ->
    file:delete(File ++ ".0.gz"),
    file:delete(File);
del_rot_files(File, N) ->
    file:delete(File ++ "." ++ integer_to_list(N) ++ ".gz"),
    del_rot_files(File, N-1).

test_server_fail(R) ->
    exit({?MODULE, get(line), R}).

xx() ->
    File = "a.LOG",
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal}, {file, File}]),
    W = xwr(a, 400),
    disk_log:close(a),
    %%    file:delete(File),
    W.

%% old: 6150
%% new: 5910
xwr(Log, BytesItem) ->
    NoW = 1000,
    Item1 = mk_bytes(BytesItem),
    Item2 = mk_bytes(BytesItem),
    Item3 = mk_bytes(BytesItem),
    Item4 = mk_bytes(BytesItem),
    Item5 = mk_bytes(BytesItem),
    Item6 = mk_bytes(BytesItem),
    Item7 = mk_bytes(BytesItem),
    Item8 = mk_bytes(BytesItem),
    Item9 = mk_bytes(BytesItem),
    Item0 = mk_bytes(BytesItem),
    Term = [Item1,Item2,Item3,Item4,Item5,Item6,Item7,Item8,Item9,Item0],
    {W, _} = timer:tc(?MODULE, wr, [Log, Term, NoW]),
    W/NoW.

measure() ->
    proc_lib:start_link(?MODULE, init_m, [self()]).

init_m(Par) ->
    process_flag(trap_exit, true),
    Res = m(),
    proc_lib:init_ack(Par, Res).

m() ->
    {W10, R10, Rep10, C10} = m_halt_int(10),
    {W11, R11, Rep11, C11} = m_halt_int(100),
    {W12, R12, Rep12, C12} = m_halt_int(400),
    {W13, R13, Rep13, C13} = m_halt_int(1000),
    {W14, R14, Rep14, C14} = m_halt_int(10000),
    {W2, R2, Rep2, C2} = m_wrap_int(400),
    {W3, R3, Rep3, C3} = m_many_halt_int(10, 400),
    {W4, R4, Rep4, C4} = m_many_halt_int(20, 400),
    {W5, R5, Rep5, C5} = m_many_halt_int(10, 1000),
    {W6, R6, Rep6, C6} = m_many_halt_int(10, 10),
    {W7, R7, Rep7, C7} = m_many_halt_int(20, 10),

    io:format("Type of log            mysec/write  mysec/read"
	      "  mysec/repair byte  cpu/write\n"),
    io:format("===========            ===========  =========="
	      "  =================  =========\n"),
    one_line("halt,int.inf. (10)", W10, R10, Rep10, C10),
    one_line("halt,int.inf. (100)", W11, R11, Rep11, C11),
    one_line("halt,int.inf. (400)", W12, R12, Rep12, C12),
    one_line("halt,int.inf. (1000)", W13, R13, Rep13, C13),
    one_line("halt,int.inf. (10000)", W14, R14, Rep14, C14),
    one_line("wrap,int.  4. (400)", W2, R2, Rep2, C2),
    one_line("halt,int.inf. (10,10)", W6, R6, Rep6, C6),
    one_line("halt,int.inf. (20,10)", W7, R7, Rep7, C7),
    one_line("halt,int.inf. (10,400)", W3, R3, Rep3, C3),
    one_line("halt,int.inf. (20,400)", W4, R4, Rep4, C4),
    one_line("halt,int.inf. (10,1000)", W5, R5, Rep5, C5),
    io:format("\n"),
    io:format("\tWrap log time depends on how often the log wraps, as this\n"),
    io:format("\tinvolves opening of new files, which costs a lot."),
    io:format("\n").

one_line(Txt, W, R, Rep, C) ->
    io:format("~.22s  ~.10w  ~.10w  ~.17w  ~.9w\n", [Txt, W, R, Rep, C]).

m_halt_int(BytesItem) ->
    File = "a.LOG",
    {ok, a} = disk_log:open([{name,a}, {type,halt}, {size,infinity},
			     {format,internal}, {file, File}]),
    {T,W} = wr(a, BytesItem),
    R = r(a),
    [{_,P}] = ets:lookup(?DISK_LOG_NAME_TABLE, a),
    exit(P, kill),
    receive after 100 -> ok end,
    crash(File, 10),
    Sz = file_size(File),
    Start = start_times(),
    {repaired, a, {recovered, Rec}, {badbytes, Bad}} = 
	disk_log:open([{name,a}, {type,halt}, {size,infinity},
		       {format,internal}, {file, File}]),
    {_,Rep} = end_times(Start),
    io:format("m_halt_int: Rep = ~p, Rec = ~p, Bad = ~p~n", [Rep, Rec, Bad]),
    disk_log:close(a),
    file:delete(File),
    {W,R,1000*Rep/Sz,T}.

m_wrap_int(BytesItem) ->
    File = "a.LOG",
    {ok, a} = disk_log:open([{name,a}, {type,wrap}, {size,{405*1000, 4}},
			     {format,internal}, {file, File}]),
    {T,W} = wr(a, BytesItem),
    R = r(a),
    [{_,P}] = ets:lookup(?DISK_LOG_NAME_TABLE, a),
    exit(P, kill),
    receive after 100 -> ok end,
    del(File, 4),
    {W,R,'n/a',T}.

m_many_halt_int(NoClients, BytesItem) ->
    Name = 'log.LOG',
    File = "log.LOG",
    {ok, _} = disk_log:open([{name,Name}, {type,halt}, 
			     {size,infinity},
			     {format,internal}, {file,File}]),
    NoW = round(lists:max([lists:min([5000000/BytesItem/NoClients,
				      50000/NoClients]),
			   1000])),
    {T,W} = many_wr(NoClients, Name, NoW, BytesItem),
    ok = disk_log:close(Name),
    file:delete(File),
    {1000*W/NoW/NoClients,'n/a','n/a',1000*T/NoW/NoClients}.

many_wr(NoClients, Log, NoW, BytesItem) ->
    Item = mk_bytes(BytesItem),
    Fun = fun(Name, _Pid, _I) -> disk_log:log(Name, Item) end,
    Start = start_times(),
    Pids = spawn_clients(NoClients, client, [self(), Log, NoW, Fun]),
    check_clients(Pids),
    end_times(Start).

wr(Log, BytesItem) ->
    NoW = round(lists:max([lists:min([5000000/BytesItem,50000]),1000])),
    Item = mk_bytes(BytesItem),
    Start = start_times(),
    wr(Log, Item, NoW),
    {T,W} = end_times(Start),
    {1000*T/NoW, 1000*W/NoW}.

wr(Log, _Item, 0) ->
    disk_log:sync(Log),
    ok;
wr(Log, Item, N) ->
    ok = disk_log:log(Log, Item),
    wr(Log, Item, N-1).

r(_) ->
    nyi.

start_times() ->
    {T1, _} = statistics(runtime),
    {W1, _} = statistics(wall_clock),
    {T1, W1}.

end_times({T1,W1}) ->
    {T2, _} = statistics(runtime),
    {W2, _} = statistics(wall_clock),
    {T2-T1, W2-W1}.


%% Test head parameter.
head_func(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    ets:new(xxx, [named_table, set, public]),
    ets:insert(xxx, {wrapc, 0}),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {100,4}},
			     {head_func, {?MODULE, hf, []}}]),
    B = mk_bytes(60),
    disk_log:log(a, B),
    disk_log:alog(a, B),
    disk_log:alog(a, B),
    disk_log:log(a, B),
    H = [1,2,3],
    [{wrapc, 4}] = ets:lookup(xxx, wrapc),
    ets:delete(xxx),
    case get_all_terms(a) of
	[H,B,H,B,H,B,H,B] ->
	    ok;
	E1 ->
	    test_server_fail({bad_terms, E1,
			      [H,B,H,B,H,B,H,B]})
    end,
    8  = no_written_items(a),
    disk_log:close(a),
    del(File, 4),

    %% invalid header function
    {error, {invalid_header, {_, {term}}}} =
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, external},
		       {head_func, {?MODULE, head_fun, [{term}]}}]),
    file:delete(File),

    {error, {invalid_header, _}} =
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, external},
		       {head_func, {?MODULE, head_fun, [{ok,{term}}]}}]),
    file:delete(File),

    {ok,n} =
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, external},
		       {head_func, {?MODULE, head_fun, [{ok,<<"head">>}]}}]),
    ok = disk_log:close(n),
    {ok,<<"head">>} = file:read_file(File),
    file:delete(File),

    {ok,n} =
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, external},
		       {head_func, {?MODULE, head_fun, [{ok,"head"}]}}]),
    ok = disk_log:close(n),
    {ok,<<"head">>} = file:read_file(File),
    file:delete(File),

    Error1 = {error, {badarg, _}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {head_func, {tjo,hej,san}},{size, {100, 4}}]),
    "The argument " ++ _ = format_error(Error1),

    Error2 = {error, {invalid_header, _}} =
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {head_func, {tjo,hej,[san]}}]),
    "The disk log header" ++ _ = format_error(Error2),
    file:delete(File).


head_fun(H) ->
    H.

hf() ->
    ets:update_counter(xxx, wrapc, 1),
    {ok, [1,2,3]}.

hf_bin() ->
    {ok, <<"1", "2", "3">>}.


%% Test head parameter.
plain_head(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    H = [1,2,3],
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {100,4}}, {head, H}]),
    %% This one is not "counted".
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {100,4}}, {head, H}]),
    B = mk_bytes(60),
    disk_log:log(a, B),
    disk_log:alog(a, B),
    disk_log:alog(a, B),
    disk_log:log(a, B),
    case get_all_terms(a) of
	[H,B,H,B,H,B,H,B] ->
	    ok;
	E1 ->
	    test_server_fail({bad_terms, E1,
			      [H,B,H,B,H,B,H,B]})
    end,
    8 = no_written_items(a),
    ok = disk_log:close(a),
    {error, no_such_log} = disk_log:close(a),
    del(File, 4).



%% Test that a header is just printed once in a log file.
one_header(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    H = [1,2,3],
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {100,4}}, {head, H}]),
    B = mk_bytes(60),
    ok = disk_log:log(a, B),
    ok = disk_log:alog(a, B),
    ok = disk_log:alog(a, B),
    ok = disk_log:log(a, B),
    case get_all_terms(a) of
	[H,B,H,B,H,B,H,B] ->
	    ok;
	E1 ->
	    test_server_fail({bad_terms, E1,
			      [H,B,H,B,H,B,H,B]})
    end,
    8  = no_written_items(a),
    ok = disk_log:close(a),
    del(File, 4),

    Fileb = filename:join(Dir, "b.LOG"),
    {ok, b} = disk_log:open([{name,b}, {file, Fileb}, {head, H}]),
    ok = disk_log:close(b),
    {ok, b} = disk_log:open([{name,b}, {file, Fileb}, {head, H}]),
    ok = disk_log:log(b, "first log"),
    ok = disk_log:alog(b, "second log"),
    ok = disk_log:close(b),
    {ok, b} = disk_log:open([{name,b}, {file, Fileb}, {head, H}]),
    ok = disk_log:alog(b, "3rd log"),
    ok = disk_log:log(b, "4th log"),
    case get_all_terms(b) of
	[H, "first log", "second log", "3rd log", "4th log"] ->
	    ok;
	E2 ->
	    test_server_fail({bad_terms, E2,
			      [H, "first log", "second log",
			       "3rd log", "4th log"]})
    end,
    2  = no_written_items(b),
    ok = disk_log:close(b),
    ok = file:delete(Fileb),

    Filec = filename:join(Dir, "c.LOG"),
    H2 = "this is a header ",
    {ok, c} = disk_log:open([{name,c}, {format, external},
			     {file, Filec}, {head, H2}]),
    ok = disk_log:close(c),
    {ok, c} = disk_log:open([{name,c}, {format, external},
			     {file, Filec}, {head, H2}]),
    ok = disk_log:blog(c, "first log"),
    ok = disk_log:balog(c, "second log"),
    ok = disk_log:close(c),
    {ok, c} = disk_log:open([{name,c}, {format, external},
			     {file, Filec}, {head, H2}]),
    ok = disk_log:balog(c, "3rd log"),
    ok = disk_log:blog(c, "4th log"),
    ok = disk_log:sync(c),
    {ok, Fdc} = file:open(Filec, [read]),
    {ok,"this is a header first logsecond log3rd log4th log"} =
	file:read(Fdc, 200),
    ok = file:close(Fdc),
    2  = no_written_items(c),
    disk_log:close(c),
    ok = file:delete(Filec),
    ok.



%% Test notify parameter, wrap.
wrap_notif(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {100,4}}, {notify, true}]),
    B = mk_bytes(60),
    disk_log:log(a, B),
    disk_log:alog(a, B),
    disk_log:alog(a, B),
    disk_log:log(a, B),
    disk_log:log(a, B),
    rec(3, {disk_log, node(), a, {wrap, 0}}),
    rec(1, {disk_log, node(), a, {wrap, 1}}),
    disk_log:close(a),
    del(File, 4).

%% Test notify parameter, wrap, filled file.
full_notif(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    file:delete(File),

    {ok, a} = disk_log:open([{name, a}, {file, File}, {type, halt},
			     {size, 100}, {notify, true}]),
    B = mk_bytes(60),
    disk_log:log(a, B),
    disk_log:alog(a, B),
    rec(1, {disk_log, node(), a, full}),
    disk_log:close(a),
    file:delete(File).

%% Test notify parameter, wrap, truncated file.
trunc_notif(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    File2 = filename:join(Dir, "a.DUMP"),
    {ok, a} = disk_log:open([{name, a}, {file, File}, {type, halt},
			     {size, 100}, {notify, true}]),
    B = mk_bytes(60),
    disk_log:log(a, B),
    disk_log:truncate(a),
    rec(1, {disk_log, node(), a, {truncated, 1}}),
    disk_log:log(a, B),
    ok = disk_log:reopen(a, File2),
    rec(1, {disk_log, node(), a, {truncated, 1}}),
    disk_log:close(a),
    file:delete(File),
    file:delete(File2).

%% Test notify parameters 'format_external' and 'blocked_log.
blocked_notif(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 4,
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {size, {100,No}}, {notify, true},
			     {format, external}]),
    B = mk_bytes(60),
    Error1 = {error,{format_external,n}} = disk_log:log(n, B),
    "The requested operation" ++ _ = format_error(Error1),
    ok = disk_log:blog(n, B),
    ok = disk_log:alog(n, B),
    rec(1, {disk_log, node(), n, {format_external, [term_to_binary(B)]}}),
    ok = disk_log:alog_terms(n, [B,B,B,B]),
    rec(1, {disk_log, node(), n, {format_external,
				  lists:map(fun term_to_binary/1, [B,B,B,B])}}),
    ok = disk_log:block(n, false),
    ok = disk_log:alog(n, B),
    rec(1, {disk_log, node(), n, {blocked_log, [term_to_binary(B)]}}),
    ok = disk_log:balog(n, B),
    rec(1, {disk_log, node(), n, {blocked_log, [list_to_binary(B)]}}),
    ok = disk_log:balog_terms(n, [B,B,B,B]),
    disk_log:close(n),
    rec(1, {disk_log, node(), n, {blocked_log,
				  lists:map(fun list_to_binary/1, [B,B,B,B])}}),
    del(File, No).


%% Test the new version of the .idx file.
new_idx_vsn(Conf) when is_list(Conf) ->
    DataDir = ?datadir(Conf),
    PrivDir = ?privdir(Conf),
    File = filename:join(PrivDir, "new_vsn.LOG"),
    Kurt = filename:join(PrivDir, "kurt.LOG"),
    Kurt2 = filename:join(PrivDir, "kurt2.LOG"),

    %% Test that a wrap log file can have more than 255 files
    {ok, new_vsn} = disk_log:open([{file, File}, {name, new_vsn},
				   {type, wrap}, {size, {40, 270}}]),
    ok = log(new_vsn, 280),
    {ok, Bin} = file:read_file(add_ext(File, "idx")),
    <<0,0:32,2,10:32,1:64,1:64,_/binary>> = Bin,
    disk_log:close(new_vsn),
    del(File, 270),

    %% convert a very old version (0) of wrap log file to the new format (2)
    copy_wrap_log("kurt.LOG", 4, DataDir, PrivDir),

    {repaired, kurt, {recovered, 1}, {badbytes, 0}} =
	disk_log:open([{file, Kurt}, {name, kurt}, 
		       {type, wrap}, {size, {40, 4}}]),
    ok = disk_log:log(kurt, "this is a logged message number X"),
    ok = disk_log:log(kurt, "this is a logged message number Y"),
    {ok, BinK} = file:read_file(add_ext(Kurt, "idx")),
    <<0,0:32,2,2:32,1:64,1:64,1:64,1:64>> = BinK,
    {{40,4}, 2} = disk_log_1:read_size_file_version(Kurt),
    disk_log:close(kurt),
    del(Kurt, 4),

    %% keep the old format (1)
    copy_wrap_log("kurt2.LOG", 4, DataDir, PrivDir),

    {repaired, kurt2, {recovered, 1}, {badbytes, 0}} =
	disk_log:open([{file, Kurt2}, {name, kurt2}, 
		       {type, wrap}, {size, {40, 4}}]),
    ok = disk_log:log(kurt2, "this is a logged message number X"),
    ok = disk_log:log(kurt2, "this is a logged message number Y"),
    {ok, BinK2} = file:read_file(add_ext(Kurt2, "idx")),
    <<0,2:32,1:32,1:32,1:32,1:32>> = BinK2,
    {{40,4}, 1} = disk_log_1:read_size_file_version(Kurt2),
    disk_log:close(kurt2),
    del(Kurt2, 4),

    ok.

%% Test reopen/1 on halt and wrap logs.
reopen(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    NewFile = filename:join(Dir, "nn.LOG"),
    B = mk_bytes(60),

    file:delete(File),    % cleanup
    file:delete(NewFile), % cleanup
    Q = qlen(),

    %% External halt log.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {notify, true}, {head, "header"},
			     {size, infinity},{format, external}]),
    ok = disk_log:blog(n, B),
    ok = disk_log:breopen(n, NewFile, "head"),
    rec(1, {disk_log, node(), n, {truncated, 2}}),
    ok = disk_log:blog(n, B),
    ok = disk_log:blog(n, B),
    ok = disk_log:breopen(n, NewFile, "head"),
    rec(1, {disk_log, node(), n, {truncated, 3}}),
    ok = disk_log:close(n),
    {ok,BinaryFile} = file:read_file(File),
    "head" = binary_to_list(BinaryFile),
    file:delete(File),
    file:delete(NewFile),

    %% Internal halt log.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {notify, true}, {head, header},
			     {size, infinity}]),
    ok = disk_log:log(n, B),
    Error1 = {error, {same_file_name, n}} = disk_log:reopen(n, File),
    "Current and new" ++ _ = format_error(Error1),
    ok = disk_log:reopen(n, NewFile),
    rec(1, {disk_log, node(), n, {truncated, 2}}),
    ok = disk_log:log(n, B),
    ok = disk_log:log(n, B),
    ok = disk_log:reopen(n, NewFile),
    rec(1, {disk_log, node(), n, {truncated, 3}}),
    ok = disk_log:close(n),
    [header, _B, _B] = get_all_terms(nn, NewFile, halt),
    file:delete(File),
    file:delete(NewFile),

    %% Internal wrap log.
    No = 4,
    del(File, No),	% cleanup
    del(NewFile, No),	% cleanup

    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {notify, true},
			     {head, header}, {size, {100, No}}]),
    ok = disk_log:log(n, B),
    ok = disk_log:log_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    rec(3, {disk_log, node(), n, {wrap, 0}}),
    ok = disk_log:log_terms(n, [B]),
    rec(1, {disk_log, node(), n, {wrap, 2}}),
    ok = disk_log:log_terms(n, [B]),
    rec(1, {disk_log, node(), n, {wrap, 2}}),
    ok = disk_log:reopen(n, NewFile, new_header),
    rec(1, {disk_log, node(), n, {truncated, 8}}),
    ok = disk_log:log_terms(n, [B,B]),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    ok = disk_log:log_terms(n, [B]),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    ok = disk_log:close(n),
    [header, _, header, _, header, _, header, _] =
	get_all_terms(nn, NewFile, wrap),
    [new_header, _, header, _, header, _] = get_all_terms(n, File, wrap),

    del(NewFile, No),
    file:delete(File ++ ".2"),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {notify, true},
			     {head, header}, {size, {100, No}}]),
    %% One file is missing...
    ok = disk_log:reopen(n, NewFile),
    rec(1, {disk_log, node(), n, {truncated, 6}}),
    ok = disk_log:close(n),

    del(File, No),
    del(NewFile, No),
    Q = qlen(),
    ok.


%% Test block/1 on external and internal logs.
block_blocked(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    B = mk_bytes(60),
    Halt = join(Dir, "halt.LOG"),

    %% External logs.
    file:delete(Halt), % cleanup
    {ok, halt} = disk_log:open([{name, halt}, {type, halt},
				{format, external}, {file, Halt}]),
    ok = disk_log:sync(halt),
    ok = disk_log:block(halt, false),
    Error1 = {error, {blocked_log, halt}} = disk_log:block(halt),
    "The blocked disk" ++ _ = format_error(Error1),
    {error, {blocked_log, halt}} = disk_log:sync(halt),
    {error, {blocked_log, halt}} = disk_log:truncate(halt),
    {error, {blocked_log, halt}} = disk_log:change_size(halt, infinity),
    {error, {blocked_log, halt}} =
        disk_log:change_notify(halt, self(), false),
    {error, {blocked_log, halt}} =
        disk_log:change_header(halt, {head, header}),
    {error, {blocked_log, halt}} = disk_log:reopen(halt, "foo"),
    ok = disk_log:close(halt),

    {ok, halt} = disk_log:open([{name, halt}, {type, halt},
				{format, external}]),
    ok = disk_log:sync(halt),
    ok = disk_log:block(halt, true),
    {error, {blocked_log, halt}} = disk_log:blog(halt, B),
    {error, {blocked_log, halt}} = disk_log:blog(halt, B),
    {error, {blocked_log, halt}} = disk_log:block(halt),
    {error, {blocked_log, halt}} = disk_log:sync(halt),
    {error, {blocked_log, halt}} = disk_log:truncate(halt),
    {error, {blocked_log, halt}} = disk_log:change_size(halt, infinity),
    {error, {blocked_log, halt}} =
        disk_log:change_notify(halt, self(), false),
    {error, {blocked_log, halt}} =
        disk_log:change_header(halt, {head, header}),
    {error, {blocked_log, halt}} = disk_log:reopen(halt, "foo"),

    ok = disk_log:unblock(halt),
    ok = disk_log:close(halt),
    file:delete(Halt),

    %% Internal logs.
    File = filename:join(Dir, "n.LOG"),
    No = 4,
    del(File, No), % cleanup
    {ok, halt} = disk_log:open([{name, halt}, {file, File}, {type, wrap},
				{size, {100, No}}]),
    ok = disk_log:block(halt, true),
    eof = disk_log:chunk(halt, start),
    Error2 = {error, end_of_log} = disk_log:chunk_step(halt, start, 1),
    "An attempt" ++ _ = format_error(Error2),
    {error, {blocked_log, halt}} = disk_log:log(halt, B),
    {error, {blocked_log, halt}} = disk_log:inc_wrap_file(halt),
    ok = disk_log:unblock(halt),
    ok = disk_log:block(halt, false),
    {error, {blocked_log, halt}} = disk_log:log(halt, B),
    {error, {blocked_log, halt}} = disk_log:inc_wrap_file(halt),
    Parent = self(),
    Pid =
        spawn_link(fun() -> 
                           {error, {blocked_log, halt}} = 
                               disk_log:chunk(halt, start),
                           {error, {blocked_log, halt}} = 
                               disk_log:chunk_step(halt, start, 1),
                           Parent ! {self(), stopped}
                   end),
    receive {Pid,stopped} -> ok end,
    ok = disk_log:close(halt),
    del(File, No).

%% Run commands from the queue by unblocking.
block_queue(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    Q = qlen(),
    File = filename:join(Dir, "n.LOG"),
    No = 4,
    del(File, No), % cleanup
    B = mk_bytes(60),

    Pid = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = sync_do(Pid, {open, File}),

    ok = disk_log:block(n, true),
    async_do(Pid, {blog, B}),
    ok = disk_log:unblock(n),
    ok = get_reply(),
    1 = no_written_items(n),
    Error1 = {error,{not_blocked,n}} = disk_log:unblock(n),
    "The disk log" ++ _ = format_error(Error1),

    ok = disk_log:block(n, true),
    async_do(Pid, {balog, "one string"}),
    ok = disk_log:unblock(n),
    ok = get_reply(),
    2 = no_written_items(n),

    ok = disk_log:block(n, true),
    async_do(Pid, sync),
    ok = disk_log:unblock(n),
    ok = get_reply(),

    ok = disk_log:block(n, true),
    async_do(Pid, truncate),
    ok = disk_log:unblock(n),
    ok = get_reply(),
    0 = no_items(n),

    ok = disk_log:block(n, true),
    async_do(Pid, {block, false}),
    ok = disk_log:unblock(n),
    ok = get_reply(),
    {error, {blocked_log, _}} = disk_log:blog(n, B),
    ok = sync_do(Pid, unblock),

    ok = disk_log:block(n, true),
    async_do(Pid, {change_notify, Pid, true}),
    ok = disk_log:unblock(n),
    ok = get_reply(),
    [{_, true}] = owners(n),

    ok = disk_log:block(n, true),
    async_do(Pid, {change_notify, Pid, false}),
    ok = disk_log:unblock(n),
    ok = get_reply(),
    [{_, false}] = owners(n),

    ok = disk_log:block(n, true),
    async_do(Pid, {change_header, {head, header}}),
    ok = disk_log:unblock(n),
    {error, {badarg, head}} = get_reply(),

    ok = disk_log:block(n, true),
    async_do(Pid, {change_size, 17}),
    ok = disk_log:unblock(n),
    {error, {badarg, size}} = get_reply(),

    ok = disk_log:block(n, true),
    async_do(Pid, inc_wrap_file),
    ok = disk_log:unblock(n),
    ok = get_reply(),

    ok = sync_do(Pid, close),
    del(File, No),

    _Pid2 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = sync_do(Pid, {int_open, File}),

    ok = disk_log:block(n, true),
    async_do(Pid, {chunk, start}),
    ok = disk_log:unblock(n),
    eof = get_reply(),

    ok = disk_log:block(n, true),
    async_do(Pid, {chunk_step, start, 100}),
    ok = disk_log:unblock(n),
    {ok, _Cont} = get_reply(),

    ok = disk_log:block(n, true),
    async_do(Pid, {log,a_term}),
    ok = disk_log:unblock(n),
    ok = get_reply(),
    1 = no_written_items(n),

    ok = sync_do(Pid, close),
    sync_do(Pid, terminate),
    del(File, No),

    %% Test of the queue. Three processes involved here. Pid1's block
    %% request is queued. Pid2's log requests are put in the queue.
    %% When unblock is executed, Pid1's block request is granted.
    %% Pid2's log requests are executed when Pid1 unblocks.
    %% (This example should show that the pair 'queue' and 'messages' 
    %% in State does the trick - one does not need a "real" queue.)
    P0 = pps(),
    Name = n,
    Pid1 = spawn_link(?MODULE, lserv, [Name]),
    {ok, Name} = sync_do(Pid1, {int_open, File, {1000,2}}),
    Pid2 = spawn_link(?MODULE, lserv, [Name]),
    {ok, Name} = sync_do(Pid2, {int_open, File, {1000,2}}),
    ok = disk_log:block(Name),
    async_do(Pid1, {alog,{1,a}}),
    ok = get_reply(),
    async_do(Pid1, {alog,{2,b}}),
    ok = get_reply(),
    async_do(Pid1, {alog,{3,c}}),
    ok = get_reply(),
    async_do(Pid1, {alog,{4,d}}),
    ok = get_reply(),
    async_do(Pid1, block),
    async_do(Pid2, {alog,{5,e}}),
    ok = get_reply(),
    async_do(Pid2, {alog,{6,f}}),
    ok = get_reply(),
    ok = disk_log:unblock(Name),
    ok = get_reply(),
    async_do(Pid2, {alog,{7,g}}),
    ok = get_reply(),
    async_do(Pid2, {alog,{8,h}}),
    ok = get_reply(),
    async_do(Pid1, unblock),
    ok = get_reply(),
    ok = sync_do(Pid1, close),
    ok = sync_do(Pid2, close),
    sync_do(Pid1, terminate),
    sync_do(Pid2, terminate),
    Terms = get_all_terms(Name, File, wrap),
    true = [{1,a},{2,b},{3,c},{4,d},{5,e},{6,f},{7,g},{8,h}] == Terms,
    del(File, 2),
    Q = qlen(),
    check_pps(P0),
    ok.

%% OTP-4880. Blocked processes did not get disk_log_stopped message.
block_queue2(Conf) when is_list(Conf) ->
    Q = qlen(),
    P0 = pps(),
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 4,

    %% log requests are queued, and processed when the log is closed
    Pid = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = sync_do(Pid, {open, File}),
    ok = sync_do(Pid, block),
    %% Asynchronous stuff is ignored.
    ok = disk_log:balog_terms(n, [<<"foo">>,<<"bar">>]),
    ok = disk_log:balog_terms(n, [<<"more">>,<<"terms">>]),
    Fun =
        fun() ->
                {error,no_such_log} = disk_log:sync(n),
                receive {disk_log, _, {error, disk_log_stopped}} -> ok end
        end,
    SyncProc = spawn_link(Fun),
    timer:sleep(500),
    ok = sync_do(Pid, close),

    %% Make sure SyncProc has terminated to that we know that it has
    %% received the disk_log error message.
    SyncProcMonRef = erlang:monitor(process, SyncProc),
    receive {'DOWN',SyncProcMonRef,process,SyncProc,_} -> ok end,
    sync_do(Pid, terminate),
    {ok,<<>>} = file:read_file(File ++ ".1"),
    del(File, No),
    Q = qlen(),
    check_pps(P0),
    ok.


%% Test unblock/1.
unblock(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 1,
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {size, {100,No}}, {notify, true},
			     {format, external}]),
    ok = disk_log:block(n),
    spawn_link(?MODULE, try_unblock, [n]),
    timer:sleep(100),
    disk_log:close(n),
    del(File, No).

try_unblock(Log) ->
    Error = {error, {not_blocked_by_pid, n}} = disk_log:unblock(Log),
    "The disk log" ++ _ = format_error(Error).


%% Test open/1 when old files exist.
open_overwrite(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 4,
    del(File, No), % cleanup

    %% read write
    First = "n.LOG.1",
    make_file(Dir, First, 8),

    Error1 = {error, {not_a_log_file, _}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}, {size, {100, No}}]),
    "The file" ++ _ = format_error(Error1),
    del(File, No),

    make_file(Dir, First, 4),

    {error, {not_a_log_file, _}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}, {size, {100, No}}]),
    del(File, No),

    make_file(Dir, First, 0),

    {error, {not_a_log_file, _}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap}, 
		       {format, internal}, {size, {100, No}}]),
    %% read only
    make_file(Dir, First, 6),

    {error, {not_a_log_file, _}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},{mode, read_only},
		       {format, internal}, {size, {100, No}}]),
    del(File, No),

    make_file(Dir, First, 0),

    {error, {not_a_log_file, _}} =
	disk_log:open([{name, n}, {file, File},{type, wrap}, 
		       {mode, read_only}, {format, internal},
		       {size, {100, No}}]),
    del(File, No),

    {error, _} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				{mode, read_only},
				{format, internal},{size, {100, No}}]),

    file:delete(File),
    {ok,n} = disk_log:open([{name,n},{file,File},
			    {mode,read_write},{type,halt}]),
    ok = disk_log:close(n),
    ok = unwritable(File),
    {error, {file_error, File, _}} =
	disk_log:open([{name,n},{file,File},{mode,read_write},{type,halt}]),
    ok = writable(File),
    file:delete(File),

    {ok,n} = disk_log:open([{name,n},{file,File},{format,external},
			    {mode,read_write},{type,halt}]),
    ok = disk_log:close(n),
    ok = unwritable(File),
    {error, {file_error, File, _}} =
        disk_log:open([{name,n},{file,File},{format,external},
                       {mode,read_write},{type,halt}]),
    ok = writable(File),
    file:delete(File),

    ok.


make_file(Dir, File, N) ->
    {ok, F} = file:open(filename:join(Dir, File), 
                        [raw, binary, read, write]),
    ok = file:truncate(F),
    case N of 
	0 ->
	    true;
	_Else ->
	    ok = file:write(F, [lists:seq(1,N)])
    end,
    ok = file:close(F).

%% Test open/1 option size.
open_size(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),

    No = 4,
    file:delete(File),
    del(File, No),	% cleanup

    %% missing size option
    {error, {badarg, size}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}]),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal},{size, {100, No}}]),
    [n] = disk_log:all(),
    B = mk_bytes(60),
    ok = disk_log:log_terms(n, [B, B, B, B]),
    ok = disk_log:sync(n),
    ok = disk_log:block(n),

    %% size option does not match existing size file, read_only
    Error1 = {error, {size_mismatch, _, _}} =
	disk_log:open([{name, nn}, {file, File}, {type, wrap}, 
		       {mode, read_only}, {format, internal},
		       {size, {100, No + 1}}]),
    "The given size" ++ _ = format_error(Error1),
    {ok, nn} =
        disk_log:open([{name, nn}, {file, File}, {type, wrap},
			      {mode, read_only},
			      {format, internal},{size, {100, No}}]),
    [_, _, _, _] = get_all_terms1(nn, start, []),
    disk_log:close(nn),

    ok = disk_log:unblock(n),
    ok = disk_log:close(n),
    del(File, No),

    ok.

%% OTP-17062. When opening a disk log for the first time, the 'size'
%% option can change the size of the file.
open_change_size(Conf) when is_list(Conf) ->

    %% wrap logs

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),

    No = 4,
    file:delete(File),
    del(File, No),	% cleanup

    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal},{size, {100, No}}]),
    [n] = disk_log:all(),
    1 = curf(n),
    B = mk_bytes(60),
    ok = disk_log:log_terms(n, [B, B, B, B]),
    4 = curf(n),
    disk_log:close(n),

    %% size option does not match existing size file, read_only
    Error1 = {error, {size_mismatch, _, _}} =
	disk_log:open([{name, nn}, {file, File}, {type, wrap},
		       {mode, read_only}, {format, internal},
		       {size, {100, No + 1}}]),
    "The given size" ++ _ = format_error(Error1),
    {ok, nn} =
        disk_log:open([{name, nn}, {file, File}, {type, wrap},
			      {mode, read_only},
			      {format, internal},{size, {100, No}}]),
    [_, _, _, _] = get_all_terms(nn),
    {error, {size_mismatch, _, _}} =
	disk_log:open([{name, nn}, {file, File}, {type, wrap},
		       {mode, read_only}, {format, internal},
		       {size, {100, No + 1}}]),
    {error,{badarg,repair_read_only}} =
	disk_log:open([{name, nn}, {file, File}, {type, wrap},
		       {mode, read_only}, {format, internal},
		       {repair, truncate}, {size, {100, No + 1}}]),
    disk_log:close(nn),

    %% size option does not match existing size file, read_write
    No1 = No + 1,
    No2 = No + 2,
    %% open/1 can change the size of a newly opened wrap log
    {ok, nn} =
	disk_log:open([{name, nn}, {file, File}, {type, wrap},
		       {format, internal}, {size, {100, No1}}]),
    {100, No1} = info(nn, size, foo),
    4 = curf(nn),
    %% open/1 cannot change the size of an open wrap log
    {error, {size_mismatch, _, _}} =
	disk_log:open([{name, nn}, {file, File}, {type, wrap},
		       {format, internal}, {size, {100, No2}}]),
    ok = disk_log:close(nn),

    %% open/1 cannot change the size of a newly opened wrap log to 'infinity'
    {ok, nn} =
	disk_log:open([{name, nn}, {file, File}, {type, wrap},
		       {format, internal}, {size, infinity}]),
    {100, 5} = info(nn, size, foo),
    4 = curf(nn),
    %% open/1 cannot change the size of an open wrap log
    {error, {size_mismatch, {100,5}, infinity}} =
	disk_log:open([{name, nn}, {file, File}, {type, wrap},
		       {format, internal}, {size, infinity}]),
    ok = disk_log:close(nn),

    %% size option does not match existing size file, truncating
    {ok, nn} =
	disk_log:open([{name, nn}, {file, File}, {type, wrap},
		       {repair, truncate}, {format, internal},
		       {size, {100, No2}}]),
    {100, No2} = info(nn, size, foo),
    1 = curf(nn),
    [] = get_all_terms(nn),
    %% open/1 cannot change the size of an open wrap log
    {error, {size_mismatch, _, _}} =
	disk_log:open([{name, nn}, {file, File}, {type, wrap},
		       {repair, truncate}, {format, internal},
		       {size, {100, No2 + 1}}]),
    ok = disk_log:close(nn),

    del(File, No),

    %% halt logs
    Name = 'log.LOG',
    HFile = "log.LOG",
    Finite = 10000,
    {ok, _} = disk_log:open([{name,Name}, {type,halt}, {size,infinity},
			     {format,internal}, {file,HFile}]),
    ok = disk_log:blog(Name, B),

    %% open/1 ignores the given size if the halt log is open,
    %% which is backwards compatible.
    {ok, Name} =
        disk_log:open([{name,Name}, {type,halt}, {size,Finite},
                       {format,internal}, {file,HFile}]),
    infinity = info(Name, size, foo),
    ok = disk_log:close(Name),
    %% open/1 can change the size of a newly opened halt log
    {ok, Name} =
        disk_log:open([{name,Name}, {type,halt}, {size,Finite},
                       {format,internal}, {file,HFile}]),
    Finite = info(Name, size, foo),
    %% open/1 ignores the given size if the halt log is open,
    %% which is backwards compatible.
    {ok, Name} =
        disk_log:open([{name,Name}, {type,halt}, {size,infinity},
                       {format,internal}, {file,HFile}]),
    Finite = info(Name, size, foo),
    ok = disk_log:close(Name),

    %% open/1 can open a halt log even if the given size is too small,
    %% which is backwards compatible.
    {ok, Name} =
        disk_log:open([{name,Name}, {type,halt}, {size,10},
                       {format,internal}, {file,HFile}]),
    file:delete(HFile),
    ok.

%% Test open/1 with {repair, truncate}.
open_truncate(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 4,
    del(File, No),	% cleanup

    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal},{size, {100, No}}]),
    B = mk_bytes(60),
    ok = disk_log:log_terms(n, [B, B, B, B]),
    ok = disk_log:close(n),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {repair,truncate},
			     {format, internal},{size, {100, No}}]),
    ok = disk_log:close(n),
    [] = get_all_terms(n, File, wrap),
    del(File, No),
    ok.


%% Try some invalid open/1 options.
open_error(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),

    File = filename:join(Dir, "n.LOG"),
    No = 4,
    del(File, No),	% cleanup

    {error, {badarg, name}} = disk_log:open([{file, File}]),
    {error, {badarg, file}} = disk_log:open([{name,{foo,bar}}]),
    {error, {badarg, [{foo,bar}]}} = disk_log:open([{foo,bar}]),

    %% external logs, read_only.
    {error, {file_error, _, enoent}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {size, {100,No}},
		       {format, external}, {mode, read_only}]),
    Error5 = {error, {file_error, _, enoent}} =
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {size, 100},
		       {format, external}, {mode, read_only}]),
    true = lists:prefix("\"" ++ File, format_error(Error5)),

    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, external},{size, {100, No}}]),
    %% Already owner, ignored.
    {ok, n} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, external}, {size, {100, No}}]),
    Error2 = {error, {name_already_open, n}} =
	disk_log:open([{name, n}, {file, another_file}, {type, wrap},
		       {format, external}, {size, {100, No}}]),
    "The disk log" ++ _ = format_error(Error2),
    Error1 = {error, {arg_mismatch, notify, false, true}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, external}, {size, {100, No}}, {notify, true}]),
    "The value" ++ _ = format_error(Error1),
    Error3 = {error, {open_read_write, n}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap}, 
		       {mode, read_only},
		       {format, external}, {size, {100, No}}]),
    "The disk log" ++ _ = format_error(Error3),
    {error, {badarg, size}} =
        disk_log:open([{name, n}, {file, File}, {type, halt},
                       {format, external}, {size, {100, No}}]),
    {error, {arg_mismatch, type, wrap, halt}} =
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, external}]),
    {error, {arg_mismatch, format, external, internal}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}, {size, {100, No}}]),
    {error, {arg_mismatch, repair, true, false}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, external}, {repair, false}]),
    {error, {size_mismatch, {100,4}, {1000,4}}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, external}, {size, {1000, No}}]),
    {error, {arg_mismatch, head, none, _}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {head, "header"},
		       {format, external}, {size, {100, No}}]),
    {error, {badarg, size}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, external}, {size, 100}]),

    ok = disk_log:close(n),

    {ok, n} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {mode, read_only},
		       {format, external}, {size, {100, No}}]),
    Error4 = {error, {open_read_only, n}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {mode, read_write},
		       {format, external}, {size, {100, No}}]),
    "The disk log" ++ _ = format_error(Error4),
    ok = disk_log:close(n),

    del(File, No).


%% Do something quickly after close/1.
close_race(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 1,
    del(File, No),  % cleanup
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {size, {100,No}}, {notify, true},
			     {format, internal}]),
    ok = disk_log:close(n),
    Error1 = {error, no_such_log} = disk_log:close(n),
    "There is no disk" ++ _ = format_error(Error1),

    %% Pid1 blocks, Pid2 closes without being suspended.
    Pid1 = spawn_link(?MODULE, lserv, [n]),
    Pid2 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = sync_do(Pid1, {open, File}),
    {ok, n} = sync_do(Pid2, {open, File}),
    ok = sync_do(Pid1, block),
    [{_, false}, {_, false}] = sync_do(Pid1, owners),
    ok = sync_do(Pid2, close),
    [{_, false}] = sync_do(Pid1, owners),
    ok = sync_do(Pid1, close),
    sync_do(Pid1, terminate),
    sync_do(Pid2, terminate),
    {error, no_such_log} = disk_log:info(n),

    %% Pid3 blocks, Pid3 closes. Pid4 should still be ablo to use log.
    Pid3 = spawn_link(?MODULE, lserv, [n]),
    Pid4 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = sync_do(Pid3, {open, File}),
    {ok, n} = sync_do(Pid4, {open, File}),
    ok = sync_do(Pid3, block),
    ok = sync_do(Pid3, close),
    [{_Pid4, false}] = sync_do(Pid4, owners),
    sync_do(Pid3, terminate),
    sync_do(Pid4, terminate),
    {error, no_such_log} = disk_log:info(n),

    %% Pid5 blocks, Pid5 terminates. Pid6 should still be ablo to use log.
    Pid5 = spawn_link(?MODULE, lserv, [n]),
    Pid6 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = sync_do(Pid5, {open, File}),
    {ok, n} = sync_do(Pid6, {open, File}),
    ok = sync_do(Pid5, block),
    sync_do(Pid5, terminate),
    [{_Pid6, false}] = sync_do(Pid6, owners),
    sync_do(Pid6, terminate),
    {error, no_such_log} = disk_log:info(n),
    del(File, No),  % cleanup
    ok.

%% Block, unblock, close, terminate.
close_block(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 1,
    del(File, No),	% cleanup

    P0 = pps(),
    %% One of two owners terminates.
    Pid1 = spawn_link(?MODULE, lserv, [n]),
    Pid2 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = sync_do(Pid1, {open, File}),
    {ok, n} = sync_do(Pid2, {open, File}),
    [_, _] = sync_do(Pid1, owners),
    [_, _] = sync_do(Pid2, owners),
    0 = sync_do(Pid1, users),
    0 = sync_do(Pid2, users),
    sync_do(Pid1, terminate),
    [_] = sync_do(Pid2, owners),
    0 = sync_do(Pid2, users),
    sync_do(Pid2, terminate),
    {error, no_such_log} = disk_log:info(n),
    check_pps(P0),

    %% Users terminate (no link...).
    Pid3 = spawn_link(?MODULE, lserv, [n]),
    Pid4 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = sync_do(Pid3, {open, File, none}),
    {ok, n} = sync_do(Pid4, {open, File, none}),
    [] = sync_do(Pid3, owners),
    [] = sync_do(Pid4, owners),
    2 = sync_do(Pid3, users),
    2 = sync_do(Pid4, users),
    sync_do(Pid3, terminate),
    [] = sync_do(Pid4, owners),
    2 = sync_do(Pid4, users),
    sync_do(Pid4, terminate),
    disk_log:close(n),
    disk_log:close(n),
    {error, no_such_log} = disk_log:info(n),
    check_pps(P0),

    %% Blocking owner terminates.
    Pid5 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {linkto, none},{size, {100,No}},
			     {format, external}]),
    {ok, n} = sync_do(Pid5, {open, File}),
    ok = sync_do(Pid5, block),
    {blocked, true} = status(n),
    [_] = owners(n),
    sync_do(Pid5, terminate),
    ok = status(n),
    [] = owners(n),
    1 = users(n),
    ok = disk_log:close(n),
    {error, no_such_log} = disk_log:info(n),
    check_pps(P0),

    %% Blocking user terminates.
    Pid6 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {size, {100,No}}, {format, external}]),
    {ok, n} = sync_do(Pid6, {open, File, none}),
    ok = sync_do(Pid6, block),
    {blocked, true} = status(n),
    [_] = owners(n),
    1 = users(n),
    sync_do(Pid6, terminate), % very silently...
    ok = status(n),
    [_] = owners(n),
    1 = users(n),
    ok = disk_log:close(n),
    [] = owners(n),
    1 = users(n),
    ok = disk_log:close(n),
    {error, no_such_log} = disk_log:info(n),
    check_pps(P0),

    %% Blocking owner terminates.
    Pid7 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {linkto, none},
			     {size, {100,No}}, {format, external}]),
    {ok, n} = sync_do(Pid7, {open, File}),
    ok = sync_do(Pid7, block),
    {blocked, true} = status(n),
    [_] = owners(n),
    1 = users(n),
    sync_do(Pid7, terminate),
    ok = status(n),
    [] = owners(n),
    1 = users(n),
    ok = disk_log:close(n),
    {error, no_such_log} = disk_log:info(n),
    check_pps(P0),

    %% Two owners, the blocking one terminates.
    Pid8 = spawn_link(?MODULE, lserv, [n]),
    Pid9 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = sync_do(Pid8, {open, File}),
    {ok, n} = sync_do(Pid9, {open, File}),
    ok = sync_do(Pid8, block),
    {blocked, true} = status(n),
    sync_do(Pid8, terminate),
    ok = status(n),
    [_] = sync_do(Pid9, owners),
    0 = sync_do(Pid9, users),
    sync_do(Pid9, terminate),
    {error, no_such_log} = disk_log:info(n),
    check_pps(P0),

    %% Blocking user closes.
    Pid10 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {size, {100,No}}, {format, external}]),
    {ok, n} = sync_do(Pid10, {open, File, none}),
    ok = sync_do(Pid10, block),
    {blocked, true} = status(n),
    [_] = owners(n),
    1 = users(n),
    ok = sync_do(Pid10, close),
    ok = status(n),
    [_] = owners(n),
    0 = users(n),
    ok = disk_log:close(n),
    sync_do(Pid10, terminate),
    {error, no_such_log} = disk_log:info(n),
    check_pps(P0),

    %% Blocking user unblocks and closes.
    Pid11 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {size, {100,No}}, {format, external}]),
    {ok, n} = sync_do(Pid11, {open, File, none}),
    ok = sync_do(Pid11, block),
    {blocked, true} = status(n),
    [_] = owners(n),
    1 = users(n),
    ok = sync_do(Pid11, unblock),
    ok = sync_do(Pid11, close),
    ok = status(n),
    [_] = owners(n),
    0 = users(n),
    ok = disk_log:close(n),
    {error, no_such_log} = disk_log:info(n),
    sync_do(Pid11, terminate),
    check_pps(P0),

    %% Blocking owner closes.
    Pid12 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {linkto, none},
			     {size, {100,No}}, {format, external}]),
    {ok, n} = sync_do(Pid12, {open, File}),
    ok = sync_do(Pid12, block),
    {blocked, true} = status(n),
    [_] = owners(n),
    1 = users(n),
    ok = sync_do(Pid12, close),
    ok = status(n),
    [] = owners(n),
    1 = users(n),
    ok = disk_log:close(n),
    {error, no_such_log} = disk_log:info(n),
    sync_do(Pid12, terminate),
    check_pps(P0),

    %% Blocking owner unblocks and closes.
    Pid13 = spawn_link(?MODULE, lserv, [n]),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {linkto, none},
			     {size, {100,No}}, {format, external}]),
    {ok, n} = sync_do(Pid13, {open, File}),
    ok = sync_do(Pid13, block),
    {blocked, true} = status(n),
    [_] = owners(n),
    1 = users(n),
    ok = sync_do(Pid13, unblock),
    ok = sync_do(Pid13, close),
    ok = status(n),
    [] = owners(n),
    1 = users(n),
    ok = disk_log:close(n),
    {error, no_such_log} = disk_log:info(n),
    sync_do(Pid13, terminate),
    check_pps(P0),

    del(File, No),	% cleanup
    ok.

%% OTP-4745. Deadlock with just an ordinary log could happen.
close_deadlock(Conf) when is_list(Conf) ->
    true = is_alive(),

    PrivDir = ?privdir(Conf),

    F1 = filename:join(PrivDir, "a.LOG"),
    file:delete(F1),
    Self = self(),

    %% One process opens the log at the same time as another process
    %% closes the log. Used to always cause deadlock before OTP-4745.
    Name = a,
    Fun = fun() -> open_close(Self, Name, F1) end,
    P = spawn(Fun),
    receive {P, Name} -> ok end,
    {ok, L} = disk_log:open([{name,Name},{file,F1}]),
    ok = disk_log:close(L),
    receive {P, done} -> ok end,
    file:delete(F1),

    %% One process opens the log at the same time as another process
    %% closes the log due to file error while truncating.
    %% This test is time dependent, but does not fail when it does not
    %% "work". When it works, as it seems to do right now :), the
    %% disk_log_server gets {error, no_such_log}, receives the EXIT
    %% message caused by truncate, and tries to open the log again.
    No = 4,
    LDir = F1 ++ ".2",
    file:del_dir(LDir),
    del(F1, No),
    ok = file:make_dir(LDir),
    Fun2 = fun() -> open_truncate(Self, Name, F1, No) end,
    P2 = spawn(Fun2),
    receive {P2, Name} -> ok end,
    {ok, L} = disk_log:open([{name, Name}, {file, F1}, {type, wrap},
			     {format, external}]),
    %% Note: truncate causes the disk log process to terminate. One
    %% cannot say if open above happened before, after, or during the
    %% termination. The link to the owner is removed before termination.
    case disk_log:close(L) of
	ok -> ok;
	{error,no_such_log} ->
	    ok
    end,
    receive {P2, done} -> ok end,
    del(F1, No),
    file:del_dir(LDir),
    ok.

open_close(Pid, Name, File) ->
    {ok, L} = disk_log:open([{name,Name},{file,File}]),
    Pid ! {self(), Name},
    ok = disk_log:close(L),
    Pid ! {self(), done}.

open_truncate(Pid, Name, File, No) ->
    {ok, L} = disk_log:open([{name, Name}, {file, File}, {type, wrap},
                             {format, external},{size, {100, No}}]),
    Pid ! {self(), Name},
    {error, {file_error, _, _}} = disk_log:truncate(L),
    %% The file has been closed, the disklog process has terminated.
    Pid ! {self(), done}.

async_do(Pid, Req) ->
    Pid ! {self(), Req},
    %% make sure the request is queued
    timer:sleep(100).

get_reply() ->
    receive Reply -> 
	    Reply 
    end.

sync_do(Pid, Req) ->
    Pid ! {self(), Req},
    receive
        Reply when Req =:= terminate ->
            timer:sleep(500),
            Reply;
	Reply ->
	    Reply
    end.

lserv(Log) ->
    receive
	{From, {open, File}} ->
	    From ! disk_log:open([{name, Log}, {file, File}, {type, wrap},
				  {size, {100,1}}, {format, external}]);
	{From, {open, File, LinkTo}} ->
	    From ! disk_log:open([{name, Log}, {file, File}, {type, wrap},
				  {linkto, LinkTo}, {size, {100,1}}, 
				  {format, external}]);
	{From, {int_open, File}} ->
	    From ! disk_log:open([{name, Log}, {file, File}, {type, wrap},
				  {size, {100,1}}]);
	{From, {int_open, File, Size}} ->
	    From ! disk_log:open([{name, Log}, {file, File}, {type, wrap},
				  {size, Size}]);
	{From, block} ->
	    From ! disk_log:block(Log);
	{From, {block, Bool}} ->
	    From ! disk_log:block(Log, Bool);
	{From, unblock} ->
	    From ! disk_log:unblock(Log);
	{From, close} ->
	    From ! disk_log:close(Log);
	{From, owners} ->
	    From ! owners(Log);
	{From, users} ->
	    From ! users(Log);
	{From, sync} ->
	    From ! disk_log:sync(Log);
	{From, truncate} ->
	    From ! disk_log:truncate(Log);
	{From, terminate} ->
	    From ! terminated,
	    exit(normal);
	{From, {log, B}} ->
	    From ! disk_log:log(Log, B);
	{From, {blog, B}} ->
	    From ! disk_log:blog(Log, B);
	{From, {alog, B}} ->
	    From ! disk_log:alog(Log, B);
	{From, {balog, B}} ->
	    From ! disk_log:balog(Log, B);
	{From, {change_notify, Pid, Bool}} ->
	    From ! disk_log:change_notify(Log, Pid, Bool);
	{From, {change_header, Header}} ->
	    From ! disk_log:change_header(Log, Header);
	{From, {change_size, Size}} ->
	    From ! disk_log:change_size(Log, Size);
	{From, inc_wrap_file} ->
	    From ! disk_log:inc_wrap_file(Log);
	{From, {chunk, Cont}} ->
	    From ! disk_log:chunk(Log, Cont);
	{From, {chunk_step, Cont, N}} ->
	    From ! disk_log:chunk_step(Log, Cont, N);
	Any ->
	    io:format("invalid request ~p~n", [Any]),
	    exit(abnormal)
    end,
    lserv(Log).


%% Error while repairing.
error_repair(Conf) when is_list(Conf) ->
    %% not all error situations are covered by this test

    DataDir = ?datadir(Conf),
    PrivDir = ?privdir(Conf),

    File = filename:join(PrivDir, "n.LOG"),
    No = 4,
    file:delete(File),
    del(File, No),	% cleanup

    %% kurt.LOG is not closed and has four logged items, one is recovered
    copy_wrap_log("kurt.LOG", "n.LOG", No, DataDir, PrivDir),
    {repaired,n,{recovered,1},{badbytes,0}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap}, {size,{40,No}}]),
    1 = cur_cnt(n),
    53 = curb(n),
    4 = no_items(n),
    ok = disk_log:close(n),

    %% temporary repair file cannot be created
    copy_wrap_log("kurt.LOG", "n.LOG", No, DataDir, PrivDir),
    Dir = File ++ ".4" ++ ".TMP",
    ok = file:make_dir(Dir),
    P0 = pps(),
    {error, {file_error, _, _}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap}, {size,{40,4}}]),
    check_pps(P0),
    del(File, No),
    ok = file:del_dir(Dir),

    error_logger:add_report_handler(?MODULE, self()),
    %% repair a file
    P1 = pps(),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal}, {size, {38,No}}]),
    ok = disk_log:log_terms(n, [{this,is}]), % first file full
    ok = disk_log:log_terms(n, [{some,terms}]), % second file full
    ok = disk_log:close(n),
    BadFile = add_ext(File, 2), % current file
    set_opened(BadFile),
    crash(BadFile, 26), % the binary is now invalid
    {repaired,n,{recovered,0},{badbytes,24}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}, {size, {38,No}}]),
    ok = disk_log:close(n),
    check_pps(P1),
    del(File, No),
    receive {info_msg, _, "disk_log: repairing" ++ _, _} -> ok
    after 1000 -> ct:fail(failed) end,

    %% yet another repair
    P2 = pps(),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal}, {size, {4000,No}}]),
    ok = disk_log:log_terms(n, [{this,is},{some,terms}]),
    ok = disk_log:close(n),
    BadFile2 = add_ext(File, 1), % current file
    set_opened(BadFile2),
    crash(BadFile2, 47), % the second binary is now invalid
    {repaired,n,{recovered,1},{badbytes,24}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}, {size, {4000,No}}]),
    ok = disk_log:close(n),
    check_pps(P2),
    del(File, No),
    receive {info_msg, _, "disk_log: repairing" ++ _, _} -> ok
    after 1000 -> ct:fail(failed) end,

    %% Repair, large term
    Big = term_to_binary(lists:duplicate(66000,$a)),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal}, {size, {40,No}}]),
    ok = disk_log:log_terms(n, [Big]),
    ok = disk_log:close(n),
    set_opened(add_ext(File, 1)),
    {repaired,n,{recovered,1},{badbytes,0}} =
	disk_log:open([{name, n}, {file, File}, {type, wrap},
		       {format, internal}, {size, {40,No}}]),
    {_, [Got]} = disk_log:chunk(n, start),
    ok = disk_log:close(n),
    Got = Big,
    del(File, No),
    receive {info_msg, _, "disk_log: repairing" ++ _, _} -> ok
    after 1000 -> ct:fail(failed) end,

    %% A term a little smaller than a chunk, then big terms.
    BigSmall = mk_bytes(1024*64-8-12),
    file:delete(File),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}]),
    ok = disk_log:log_terms(n, [BigSmall, Big, Big]),
    ok = disk_log:close(n),
    set_opened(File),
    FileSize = file_size(File),
    crash(File, FileSize-byte_size(Big)-4),
    Error1 = {error, {need_repair, _}} =
        disk_log:open([{name, n}, {file, File}, {repair, false},
                       {type, halt}, {format, internal}]),
    "The disk log" ++ _ = format_error(Error1),
    {repaired,n,{recovered,2},{badbytes,132013}} =
        disk_log:open([{name, n}, {file, File}, {repair, true},
                       {type, halt}, {format, internal}]),
    ok = disk_log:close(n),
    file:delete(File),
    receive {info_msg, _, "disk_log: repairing" ++ _, _} -> ok
    after 1000 -> ct:fail(failed) end,

    %% The header is recovered.
    {ok,n} =
	disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, internal},
		       {head_func, {?MODULE, head_fun, [{ok,"head"}]}}]),
    ok = disk_log:log_terms(n, [list,'of',terms]),
    ["head",list,'of',terms] = get_all_terms(n),
    ok = disk_log:close(n),
    set_opened(File),
    crash(File, 30),
    {repaired,n,{recovered,3},{badbytes,15}} =
        disk_log:open([{name, n}, {file, File}, {type, halt},
		       {format, internal},{repair,true}, {quiet, true},
		       {head_func, {?MODULE, head_fun, [{ok,"head"}]}}]),
    ["head",'of',terms] = get_all_terms(n),
    ok = disk_log:close(n),
    error_logger:delete_report_handler(?MODULE),
    file:delete(File),
    {messages, []} = process_info(self(), messages),

    ok.

set_opened(File) ->
    {ok, Fd} = file:open(File, [raw, binary, read, write]),
    ok = file:write(Fd, [?LOGMAGIC, ?OPENED]),
    ok = file:close(Fd).

%% Error while repairing.
error_log(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),

    File = filename:join(Dir, "n.LOG"),
    No = 4,
    file:delete(File),
    del(File, No),	% cleanup
    LDir = File ++ ".2",

    Q = qlen(),
    %% dummy just to get all processes "above" disk_log going
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, external},{size, {100, No}}]),
    ok = disk_log:close(n),
    del(File, No),

    %% inc_wrap_file fails, the external log is not terminated
    P0 = pps(),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, external},{size, {100, No}}]),
    ok = file:make_dir(LDir),
    {error, {file_error, _, _}} = disk_log:inc_wrap_file(n),
    timer:sleep(500),
    ok = disk_log:close(n),
    del(File, No),

    %% inc_wrap_file fails, the internal log is not terminated, ./File.2/ exists
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal},{size, {100, No}}]),
    {error, {file_error, _, _}} = disk_log:inc_wrap_file(n),
    ok = disk_log:close(n),
    del(File, No),

    %% truncate fails, the log is terminated, ./File.2/ exists
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, external},{size, {100, No}}]),
    {error, {file_error, _, _}} = disk_log:truncate(n),
    check_pps(P0),
    del(File, No),

    %% OTP-4880.
    %% reopen (rename) fails, the log is terminated, ./File.2/ exists
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, external},{size, 100000}]),
    {error, {file_error, _, eisdir}} = disk_log:reopen(n, LDir),
    check_pps(P0),
    file:delete(File),

    B = mk_bytes(60),

    %% OTP-4880. reopen a wrap log, rename fails
    File2 = filename:join(Dir, "n.LOG2"),
    {ok, n} = disk_log:open([{name, n}, {file, File2}, {type, wrap},
			     {format, external},{size, {100, No}}]),
    ok = disk_log:blog_terms(n, [B,B,B]),
    {error, {file_error, _, eisdir}} = disk_log:reopen(n, File),
    {error, no_such_log} = disk_log:close(n),
    del(File2, No),
    del(File, No),

    %% log, external wrap log, ./File.2/ exists
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, external},{size, {100, No}}]),
    {error, {file_error, _, _}} = disk_log:blog_terms(n, [B,B,B]),
    ok = disk_log:close(n),
    del(File, No),

    %% log, internal wrap log, ./File.2/ exists
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal},{size, {100, No}}]),
    {error, {file_error, _, _}} = disk_log:log_terms(n, [B,B,B]),
    ok = disk_log:close(n),
    del(File, No),

    ok = file:del_dir(LDir),

    %% can't remove file when changing size
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal},{size, {100, No}}]),
    ok = disk_log:log_terms(n, [B,B,B,B]),
    ok = disk_log:change_size(n, {100, No-2}),
    Three = File ++ ".3",
    ok = file:delete(Three),
    ok = file:make_dir(Three),
    {error, {file_error, _, _}} = disk_log:log_terms(n, [B,B,B]),
    timer:sleep(500),
    ok = disk_log:close(n),
    ok = file:del_dir(Three),
    del(File, No),
    Q = qlen(),
    ok.

%% Test chunk and chunk_step.
chunk(Conf) when is_list(Conf) ->
    %% See also halt_ro_crash/1 above.

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 4,
    B = mk_bytes(60),
    BB = mk_bytes(64000), % 64 kB chunks
    del(File, No),% cleanup

    %% Make sure chunk_step skips the rest of the binary.
    %% OTP-3716. This was a bug...
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal}, {size, {50,No}}]),
    %% 1, 2 and 3 on file one, 4 on file two.
    ok = disk_log:log_terms(n, [1,2,3,4]),
    {I1, [1]} = disk_log:chunk(n, start, 1),
    [{node,Node}] = disk_log:chunk_info(I1),
    Node = node(),
    Error1 = {error, {no_continuation, foobar}} =
        disk_log:chunk_info(foobar),
    "The term" ++ _ = format_error(Error1),
    {ok, I2} = disk_log:chunk_step(n, I1, 1),
    {error, {badarg, continuation}} = disk_log:chunk_step(n, foobar, 1),
    {I3, [4]} = disk_log:chunk(n, I2, 1),
    {ok, I4} = disk_log:chunk_step(n, I3, -1),
    {_, [1]} = disk_log:chunk(n, I4, 1),
    {error, {badarg, continuation}} = disk_log:bchunk(n, 'begin'),
    {Ib1, [Bin1,Bin2]} = disk_log:bchunk(n, start, 2),
    1 = binary_to_term(Bin1),
    2 = binary_to_term(Bin2),
    {ok, Ib2} = disk_log:chunk_step(n, Ib1, 1),
    {Ib3, [Bin3]} = disk_log:bchunk(n, Ib2, 1),
    4 = binary_to_term(Bin3),
    {ok, Ib4} = disk_log:chunk_step(n, Ib3, -1),
    {_, [Bin4]} = disk_log:bchunk(n, Ib4, 1),
    1 = binary_to_term(Bin4),
    {Ib5, [Bin1, Bin2, Bin17]} = disk_log:bchunk(n, start),
    3 = binary_to_term(Bin17),
    {Ib6, [Bin3]} = disk_log:bchunk(n, Ib5, infinity),
    eof = disk_log:bchunk(n, Ib6, infinity),
    ok = disk_log:close(n),
    del(File, No), % cleanup

    %% external log, cannot read chunks
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, external}, {size, {100,No}}]),
    {error, {badarg, continuation}} = disk_log:chunk(n, 'begin'),
    {error, {format_external, n}} = disk_log:chunk(n, start),
    Error2 = {error, {not_internal_wrap, n}} =
        disk_log:chunk_step(n, start, 1),
    "The requested" ++ _ = format_error(Error2),
    ok = disk_log:close(n),
    del(File, No),

    %% wrap, read_write
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal}, {size, {100,No}}]),
    ok = disk_log:log_terms(n, [B,B,B,B]),
    {C1, [_]} = disk_log:chunk(n, start),
    {C2, [_]} = disk_log:chunk(n, C1),
    {C3, [_]} = disk_log:chunk(n, C2),
    {C4, [_]} = disk_log:chunk(n, C3, 1),
    eof = disk_log:chunk(n, C4),
    {C5, [_]} = disk_log:chunk(n, start),
    {ok, C6} = disk_log:chunk_step(n, C5, 1),
    {C7, [_]} = disk_log:chunk(n, C6),
    {ok, C8} = disk_log:chunk_step(n, C7, 1),
    {_, [_]} = disk_log:chunk(n, C8),
    ok = disk_log:close(n),

    %% wrap, read_only
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {mode, read_only},
			     {format, internal}, {size, {100,No}}]),
    {CC1, [_]} = disk_log:chunk(n, start),
    {CC2, [_]} = disk_log:chunk(n, CC1),
    {CC3, [_]} = disk_log:chunk(n, CC2),
    {CC4, [_]} = disk_log:chunk(n, CC3, 1),
    eof = disk_log:chunk(n, CC4),
    {CC5, [_]} = disk_log:chunk(n, start),
    {ok, CC6} = disk_log:chunk_step(n, CC5, 1),
    {CC7, [_]} = disk_log:chunk(n, CC6),
    {ok, CC8} = disk_log:chunk_step(n, CC7, 1),
    {_, [_]} = disk_log:chunk(n, CC8),
    ok = disk_log:close(n),

    %% OTP-3716. A bug: {Error, List} and {Error, List, Bad} could be
    %% returned from chunk/2.
    %% Magic bytes not OK.
    %% File header (8 bytes) OK, item header not OK.
    InvalidFile = add_ext(File, 1),
    crash(InvalidFile, 15),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {mode, read_only},
			     {format, internal}, {size, {100,No}}]),
    {_, [], 61} = disk_log:chunk(n, start),
    ok = disk_log:close(n),
    %% read_write...
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal}, {size, {100,No}}]),
    Error3 = {error, {corrupt_log_file, Culprit}} =
        disk_log:chunk(n, start),
    "The disk log file" ++ _ = format_error(Error3),
    Culprit = InvalidFile,
    ok = disk_log:close(n),
    del(File, No),

    %% Two wrap log files, writing the second one, then reading the first
    %% one, where a bogus term resides.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal}, {size, {38,No}}]),
    ok = disk_log:log_terms(n, [{this,is}]), % first file full
    ok = disk_log:log_terms(n, [{some,terms}]), % second file full
    2 = curf(n),
    BadFile = add_ext(File, 1),
    crash(BadFile, 26), % the _binary_ is now invalid
    {error, {corrupt_log_file, BFile}} = disk_log:chunk(n, start, 1),
    BadFile = BFile,
    ok = disk_log:close(n),
    %% The same, with a halt log.
    file:delete(File), % cleanup
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}]),
    ok = disk_log:log_terms(n, [{this,is}]),
    ok = disk_log:sync(n),
    crash(File, 26), % the _binary_ is now invalid
    {error, {corrupt_log_file, File2}} = disk_log:chunk(n, start, 1),
    crash(File, 10),
    {error,{corrupt_log_file,_}} = disk_log:bchunk(n, start, 1),
    true = File == File2,
    ok = disk_log:close(n),
    del(File, No),

    %% halt, read_write
    file:delete(File), % cleanup
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}]),
    ok = disk_log:log_terms(n, [BB,BB,BB,BB]),
    {D1, [Ch1]} = disk_log:chunk(n, start, 1),
    Ch1 = BB,
    {D2, [Ch2]} = disk_log:chunk(n, D1, 1),
    Ch2 = BB,
    {D3, [Ch3]} = disk_log:chunk(n, D2, 1),
    Ch3 = BB,
    {D4, [Ch4]} = disk_log:chunk(n, D3, 1),
    Ch4 = BB,
    eof = disk_log:chunk(n, D4),
    ok = disk_log:close(n),

    %% halt, read_only
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal},{mode,read_only}]),
    {E1, [Ch5]} = disk_log:chunk(n, start, 1),
    Ch5 = BB,
    {E2, [Ch6]} = disk_log:chunk(n, E1, 1),
    Ch6 = BB,
    {E3, [Ch7]} = disk_log:chunk(n, E2, 1),
    Ch7 = BB,
    {E4, [Ch8]} = disk_log:chunk(n, E3, 1),
    Ch8 = BB,
    eof = disk_log:chunk(n, E4),
    ok = disk_log:close(n),
    file:delete(File), % cleanup

    %% More than 64 kB term.
    BBB = term_to_binary(lists:duplicate(66000,$a)),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}]),
    ok = disk_log:log_terms(n, [BBB]),
    {F1, [BBB1]} = disk_log:chunk(n, start),
    BBB1 = BBB,
    eof = disk_log:chunk(n, F1),
    ok = disk_log:close(n),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}, {mode, read_only}]),
    {F1r, [BBB2]} = disk_log:chunk(n, start),
    BBB2 = BBB,
    eof = disk_log:chunk(n, F1r),
    ok = disk_log:close(n),

    truncate(File, 8192),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}]),
    {error, {corrupt_log_file, _}} = disk_log:chunk(n, start),
    ok = disk_log:close(n),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}, {mode, read_only}]),
    {K1, [], 8176} = disk_log:chunk(n, start),
    eof = disk_log:chunk(n, K1),
    ok = disk_log:close(n),
    file:delete(File), % cleanup

    %% OTP-3716. A bug: eof in the middle of the last element is not ok.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}]),
    ok = disk_log:log_terms(n, [B,BB]),
    ok = disk_log:close(n),
    truncate(File, 80),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}]),
    {G1, [_]} = disk_log:chunk(n, start, 1),
    {error, {corrupt_log_file, _}} = disk_log:chunk(n, G1, 1),
    ok = disk_log:close(n),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}, {mode, read_only}]),
    {G1r, [_]} = disk_log:chunk(n, start, 1),
    {_, [], 4} = disk_log:chunk(n, G1r, 1),
    ok = disk_log:close(n),
    file:delete(File), % cleanup

    %% Opening a wrap log read-only. The second of four terms is destroyed.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal}, {size, {4000,No}}]),
    ok = disk_log:log_terms(n,
			    [{this,is},{some,terms},{on,a},{wrap,file}]),
    ok = disk_log:close(n),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, internal}, {mode, read_only}]),
    CrashFile = add_ext(File, 1),
    crash(CrashFile, 47), % the binary term {some,terms} is now bad
    {H1, [{this,is}], 16} = disk_log:chunk(n, start, 10),
    {H2, [{on,a},{wrap,file}]} = disk_log:chunk(n, H1),
    eof = disk_log:chunk(n, H2),
    ok = disk_log:close(n),
    del(File, No),

    %% The same as last, but with a halt log.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}, {mode, read_write}]),
    ok = disk_log:alog_terms(n, [{this,is},{some,terms}]),
    ok = disk_log:log_terms(n, [{on,a},{halt,file}]),
    ok = disk_log:close(n),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}, {mode, read_only}]),
    crash(File, 47), % the binary term {some,terms} is now bad
    {J1, [{this,is}], 16} = disk_log:chunk(n, start, 10),
    {J2, [{on,a},{halt,file}]} = disk_log:chunk(n, J1),
    eof = disk_log:chunk(n, J2),
    ok = disk_log:close(n),
    file:delete(File),

    %% OTP-7641. Same as last one, but the size of the bad term is
    %% less than ?HEADERSz (8) bytes.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}, {mode, read_write}]),
    ok = disk_log:alog_terms(n, [{this,is},{s}]),
    ok = disk_log:log_terms(n, [{on,a},{halt,file}]),
    ok = disk_log:close(n),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}, {mode, read_only}]),
    crash(File, 41), % the binary term {s} is now bad
    {J11, [{this,is}], 6} = disk_log:chunk(n, start, 10),
    {J21, [{on,a},{halt,file}]} = disk_log:chunk(n, J11),
    eof = disk_log:chunk(n, J21),
    ok = disk_log:close(n),
    file:delete(File),

    %% Minimal MD5-proctected term, and maximal unprotected term.
    %% A chunk ends in the middle of the MD5-sum.
    MD5term = mk_bytes(64*1024-8),
    NotMD5term = mk_bytes((64*1024-8)-1),
    Term2 = mk_bytes((64*1024-8)-16),
    MD5L = [MD5term,NotMD5term,Term2,MD5term,MD5term,NotMD5term],
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}]),
    ok = disk_log:log_terms(n, MD5L),
    true = MD5L == get_all_terms(n),
    ok = disk_log:close(n),
    true = MD5L == get_all_terms(n, File, halt),
    crash(File, 21), % the MD5-sum of the first term is now bad
    true = {tl(MD5L),64*1024-8} == get_all_terms_and_bad(n, File, halt),
    {_,64*1024-8} = get_all_binary_terms_and_bad(n, File, halt),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, halt},
			     {format, internal}]),
    {error, {corrupt_log_file, _}} = disk_log:chunk(n, start),
    ok = disk_log:close(n),
    file:delete(File),

    %% A file with "old" terms (magic word is MAGICINT).
    DataDir = ?datadir(Conf),
    OldTermsFileOrig = filename:join(DataDir, "old_terms.LOG"),
    OldTermsFile = filename:join(Dir, "old_terms.LOG"),
    copy_file(OldTermsFileOrig, OldTermsFile),
    {[_,_,_,_],0} = get_all_terms_and_bad(n, OldTermsFile, halt),
    {ok, n} = disk_log:open([{name, n}, {file, OldTermsFile},
			     {type, halt}, {format, internal}]),
    [_,_,_,_] = get_all_terms(n),
    ok = disk_log:close(n),
    file:delete(OldTermsFile),

    ok.

%% OTP-5558. Keep the contents of index files after disk crash.
error_index(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),

    File = filename:join(Dir, "n.LOG"),
    IdxFile = File ++ ".idx",
    No = 4,
    file:delete(File),
    del(File, No),	% cleanup

    Args = [{name,n},{type,wrap},{size,{100,No}},{file,File}],
    {ok, n} = disk_log:open(Args),
    ok = disk_log:close(n),
    Q = qlen(),
    P0 = pps(),
    ok = file:write_file(IdxFile, <<"abc">>),
    {error, {invalid_index_file, _}} = disk_log:open(Args),
    {error, {invalid_index_file, _}} = disk_log:open(Args),
    {error, {invalid_index_file, _}} = disk_log:open(Args),

    del(File, No),
    check_pps(P0),
    true = (Q == qlen()),
    ok.

%% Test truncate/1 on halt and wrap logs.
truncate(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),

    Q = qlen(),
    Halt = join(Dir, "halt.LOG"),
    %% Halt logs.

    file:delete(Halt), % cleanup
    {ok, halt} = disk_log:open([{name, halt}, {type, halt}, {file, Halt},
				{head, header}, {notify, true}]),
    infinity = sz(halt),
    ok = disk_log:truncate(halt, tjohej),
    rec(1, {disk_log, node(), halt, {truncated, 1}}),
    ok = disk_log:change_size(halt, 10000),
    10000 = sz(halt),
    disk_log:close(halt),
    [tjohej] = get_all_terms(halt, Halt, halt),
    file:delete(Halt),

    {ok, halt} = disk_log:open([{name, halt}, {type, halt}, {file, Halt},
				{head, header}, {notify, true}]),
    ok = disk_log:truncate(halt),
    rec(1, {disk_log, node(), halt, {truncated, 1}}),
    disk_log:close(halt),
    [header] = get_all_terms(halt, Halt, halt),
    file:delete(Halt),

    {ok, halt} = disk_log:open([{name, halt}, {type, halt},
				{file, Halt}, {format, external},
				{head, "header"}, {notify, false}]),
    ok = disk_log:btruncate(halt, "apa"),
    disk_log:close(halt),
    3 = file_size(Halt),
    file:delete(Halt),

    %% Wrap logs.
    File = filename:join(Dir, "n.LOG"),
    No = 4,
    B = mk_bytes(60),
    del(File, No),	% cleanup

    %% Internal with header.
    Size = {100, No},
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {head, header}, {notify, true},
			     {size, Size}]),
    ok = disk_log:log_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    rec(2, {disk_log, node(), n, {wrap, 0}}),
    ok = disk_log:truncate(n, apa),
    rec(1, {disk_log, node(), n, {truncated, 6}}),
    {0, 0} = no_overflows(n),
    22 = curb(n),
    1 = curf(n),
    1 = cur_cnt(n),
    true = (Size == sz(n)),

    ok = disk_log:log_terms(n, [B, B]),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    ok = disk_log:close(n),
    [apa, _, header, _] = get_all_terms(n, File, wrap),
    del(File, No),

    %% Internal without general header.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {notify, true},
			     {size, {100, No}}]),
    ok = disk_log:log_terms(n, [B,B,B]),
    rec(2, {disk_log, node(), n, {wrap, 0}}),
    ok = disk_log:truncate(n, apa),
    rec(1, {disk_log, node(), n, {truncated, 3}}),
    {0, 0} = no_overflows(n),
    22 = curb(n),
    1 = curf(n),
    1 = cur_cnt(n),
    true = (Size == sz(n)),

    ok = disk_log:log_terms(n, [B, B]),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    ok = disk_log:close(n),
    [apa, _, _] = get_all_terms(n, File, wrap),
    del(File, No),

    %% Internal without any header.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {notify, true},
			     {size, {100, No}}]),
    ok = disk_log:log_terms(n, [B,B,B]),
    rec(2, {disk_log, node(), n, {wrap, 0}}),
    ok = disk_log:truncate(n),
    rec(1, {disk_log, node(), n, {truncated, 3}}),
    {0, 0} = no_overflows(n),
    8 = curb(n),
    1 = curf(n),
    0 = cur_cnt(n),
    true = (Size == sz(n)),

    ok = disk_log:log_terms(n, [B, B]),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    ok = disk_log:close(n),
    [_, _] = get_all_terms(n, File, wrap),
    del(File, No),
    Q = qlen(),
    ok.


%% Test many users logging and sync:ing at the same time.
many_users(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    N = 100,
    NoClients = 10,
    Fun1 = fun(Name, Pid, I) -> disk_log:log(Name, {Pid, I}) end,
    Fun2 = fun(Name, Pid, I) -> ok = disk_log:log(Name, {Pid, I}),
				disk_log:sync(Name) end,
    {C1, T1} = many(Fun2, NoClients, N, halt, internal, infinity, Dir),
    true = lists:duplicate(NoClients, ok) == C1,
    true = length(T1) == N*NoClients,
    {C2, T2} = many(Fun1, NoClients, N, halt, internal, 1000, Dir),
    true = lists:duplicate(NoClients, {error, {full,"log.LOG"}}) == C2,
    true = length(T2) > 0,
    {C3, T3} = many(Fun2, NoClients, N, wrap, internal,
		    {300*NoClients,200}, Dir),
    true = lists:duplicate(NoClients, ok) == C3,
    true = length(T3) == N*NoClients,
    ok.

many(Fun, NoClients, N, Type, Format, Size, Dir) ->
    Name = "log.LOG",
    File = filename:join(Dir, Name),
    del_files(Size, File),
    Q = qlen(),
    {ok, _} = disk_log:open([{name,Name}, {type,Type}, {size,Size},
			     {format,Format}, {file,File}]),
    Pids = spawn_clients(NoClients, client, [self(), Name, N, Fun]),
    Checked = check_clients(Pids),
    ok = disk_log:close(Name),
    Terms = get_all_terms(Name, File, Type),
    del_files(Size, File),
    Q = qlen(),
    {Checked, Terms}.

spawn_clients(0, _F, _A) ->
    [];
spawn_clients(I, F, A) ->
    [spawn_link(?MODULE, F, A) | spawn_clients(I-1, F, A)].

check_clients(Pids) ->
    lists:map(fun(Pid) -> receive {Pid, Reply} -> Reply end end, Pids).

client(From, _Name, 0, _Fun) ->
    From ! {self(), ok};
client(From, Name, N, Fun) ->
    %% Fun is called N times.
    case Fun(Name, self(), N) of
	ok -> client(From, Name, N-1, Fun);
	Else -> From ! {self(), Else}
    end.

del_files({_NoBytes,NoFiles}, File) ->
    del(File, NoFiles);
del_files(_Size, File) ->
    file:delete(File).




%% Test no_current_{bytes, items} as returned by info/0.
info_current(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 4,
    B = mk_bytes(60),
    BB = mk_bytes(160), % bigger than a single wrap log file
    SB = mk_bytes(10),  % much smaller than a single wrap log file
    del(File, No),% cleanup

    Q = qlen(),
    %% Internal with header.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {head, header}, {size, {100,No}}]),
    {25, 1} = {curb(n), cur_cnt(n)},
    {1, 1}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log(n, B),
    {93, 2} = {curb(n), cur_cnt(n)},
    {2, 2}  = {no_written_items(n), no_items(n)},
    ok = disk_log:close(n),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {notify, true},
			     {head, header}, {size, {100,No}}]),
    {93, 2} = {curb(n), cur_cnt(n)},
    {0, 2}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log(n, B),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    {93, 2} = {curb(n), cur_cnt(n)},
    {2, 4}  = {no_written_items(n), no_items(n)},
    disk_log:inc_wrap_file(n),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    {25, 1} = {curb(n), cur_cnt(n)},
    {3, 4}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    rec(1, {disk_log, node(), n, {wrap, 2}}),
    {93, 2} = {curb(n), cur_cnt(n)},
    {8, 7}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log_terms(n, [B]),
    rec(1, {disk_log, node(), n, {wrap, 2}}),
    ok = disk_log:log_terms(n, [B]),
    rec(1, {disk_log, node(), n, {wrap, 2}}),
    {93, 2} = {curb(n), cur_cnt(n)},
    {12, 7}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log_terms(n, [BB,BB]),
    %% Used to be one message, but now one per wrapped file.
    rec(2, {disk_log, node(), n, {wrap, 2}}),
    {193, 2} = {curb(n), cur_cnt(n)},
    {16, 7}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log_terms(n, [SB,SB,SB]),
    rec(1, {disk_log, node(), n, {wrap, 2}}),
    {79, 4} = {curb(n), cur_cnt(n)},
    {20, 9}  = {no_written_items(n), no_items(n)},
    ok = disk_log:close(n),
    del(File, No),

    %% Internal without header.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {size, {100,No}}]),
    {8, 0} = {curb(n), cur_cnt(n)},
    {0, 0}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log(n, B),
    {76, 1} = {curb(n), cur_cnt(n)},
    {1, 1}  = {no_written_items(n), no_items(n)},
    ok = disk_log:close(n),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {notify, true}, {size, {100,No}}]),
    {76, 1} = {curb(n), cur_cnt(n)},
    {0, 1}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log(n, B),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    {76, 1} = {curb(n), cur_cnt(n)},
    {1, 2}  = {no_written_items(n), no_items(n)},
    disk_log:inc_wrap_file(n),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    {8, 0} = {curb(n), cur_cnt(n)},
    {1, 2}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    rec(1, {disk_log, node(), n, {wrap, 1}}),
    {76, 1} = {curb(n), cur_cnt(n)},
    {4, 4}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log_terms(n, [B]),
    rec(1, {disk_log, node(), n, {wrap, 1}}),
    ok = disk_log:log_terms(n, [B]),
    rec(1, {disk_log, node(), n, {wrap, 1}}),
    {76, 1} = {curb(n), cur_cnt(n)},
    {6, 4}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log_terms(n, [BB,BB]),
    %% Used to be one message, but now one per wrapped file.
    rec(2, {disk_log, node(), n, {wrap, 1}}),
    {176, 1} = {curb(n), cur_cnt(n)},
    {8, 4}  = {no_written_items(n), no_items(n)},
    ok = disk_log:log_terms(n, [SB,SB,SB]),
    rec(1, {disk_log, node(), n, {wrap, 1}}),
    {62, 3} = {curb(n), cur_cnt(n)},
    {11, 6}  = {no_written_items(n), no_items(n)},
    ok = disk_log:close(n),
    del(File, No),

    %% External with header.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, external}, {head, "header"},
			     {size, {100,No}}]),
    {6, 1} = {curb(n), cur_cnt(n)},
    {1, 1}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog(n, B),
    {62, 2} = {curb(n), cur_cnt(n)},
    {2, 2}  = {no_written_items(n), no_items(n)},
    ok = disk_log:close(n),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, external}, {head, "header"},
			     {notify, true}, {size, {100,No}}]),
    {62, 2} = {curb(n), cur_cnt(n)},
    {0, 2}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog(n, B),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    {62, 2} = {curb(n), cur_cnt(n)},
    {2, 4}  = {no_written_items(n), no_items(n)},
    disk_log:inc_wrap_file(n),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    {6, 1} = {curb(n), cur_cnt(n)},
    {3, 4}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    rec(1, {disk_log, node(), n, {wrap, 2}}),
    {62, 2} = {curb(n), cur_cnt(n)},
    {8, 7}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog_terms(n, [B]),
    rec(1, {disk_log, node(), n, {wrap, 2}}),
    ok = disk_log:blog_terms(n, [B]),
    rec(1, {disk_log, node(), n, {wrap, 2}}),
    {62, 2} = {curb(n), cur_cnt(n)},
    {12, 7}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog_terms(n, [BB,BB]),
    %% Used to be one message, but now one per wrapped file.
    rec(2, {disk_log, node(), n, {wrap, 2}}),
    {162, 2} = {curb(n), cur_cnt(n)},
    {16, 7}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog_terms(n, [SB,SB,SB]),

    rec(1, {disk_log, node(), n, {wrap, 2}}),
    {24, 4} = {curb(n), cur_cnt(n)},
    {20, 9}  = {no_written_items(n), no_items(n)},
    ok = disk_log:close(n),
    del(File, No),

    %% External without header.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {format, external}, {size, {100,No}}]),
    {0, 0} = {curb(n), cur_cnt(n)},
    {0, 0}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog(n, B),
    {56, 1} = {curb(n), cur_cnt(n)},
    {1, 1}  = {no_written_items(n), no_items(n)},
    ok = disk_log:close(n),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
			     {notify, true},
			     {format, external}, {size, {100,No}}]),
    {56, 1} = {curb(n), cur_cnt(n)},
    {0, 1}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog(n, B),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    {56, 1} = {curb(n), cur_cnt(n)},
    {1, 2}  = {no_written_items(n), no_items(n)},
    disk_log:inc_wrap_file(n),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    {0, 0} = {curb(n), cur_cnt(n)},
    {1, 2}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog_terms(n, [B,B,B]),
    %% Used to be one message, but now one per wrapped file.
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    rec(1, {disk_log, node(), n, {wrap, 1}}),
    {56, 1} = {curb(n), cur_cnt(n)},
    {4, 4}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog_terms(n, [B]),
    rec(1, {disk_log, node(), n, {wrap, 1}}),
    ok = disk_log:blog_terms(n, [B]),
    rec(1, {disk_log, node(), n, {wrap, 1}}),
    {56, 1} = {curb(n), cur_cnt(n)},
    {6, 4}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog_terms(n, [BB,BB]),
    %% Used to be one message, but now one per wrapped file.
    rec(2, {disk_log, node(), n, {wrap, 1}}),
    {156, 1} = {curb(n), cur_cnt(n)},
    {8, 4}  = {no_written_items(n), no_items(n)},
    ok = disk_log:blog_terms(n, [SB,SB,SB]),
    rec(1, {disk_log, node(), n, {wrap, 1}}),
    {18, 3} = {curb(n), cur_cnt(n)},
    {11, 6}  = {no_written_items(n), no_items(n)},
    ok = disk_log:close(n),
    del(File, No),

    Q = qlen(),
    ok.



change_size_before(doc) -> 
    ["Change size of a wrap log file before we have reached "
     "to the file index corresponding to the new size"];
change_size_before(Conf) when is_list(Conf) ->

    Log_1_1 = "first log  first message",
    Log_1_2 = "first log  second message",
    Log_2_1 = "second log  first message",
    Log_2_2 = "second log  second message",
    Log_3_1 = "third log  first message",
    Log_3_2 = "third log  second message",
    Log_4_1 = "fourth log  first message",
    Log_4_2 = "fourth log  second message",
    Log_5_1 = "fifth log  first message",
    Log_5_2 = "fifth log  second message",
    Log_1_2_1 = "first log  second round 1",
    Log_1_2_2 = "first log  second round 2",


    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    del(File, 5),
    {ok, a} = disk_log:open([{name,a}, {file, File},
			     {type, wrap}, {size, {100,5}}]),
    disk_log:log(a, Log_1_1),
    disk_log:log(a, Log_1_2),
    disk_log:log(a, Log_2_1),
    disk_log:log(a, Log_2_2),
    disk_log:change_size(a, {100, 3}),
    [Log_1_1, Log_1_2,
     Log_2_1, Log_2_2] = get_all_terms(a),
    disk_log:log(a, Log_3_1),
    disk_log:log(a, Log_3_2),
    disk_log:log(a, Log_1_2_1),
    disk_log:log(a, Log_1_2_2),
    [Log_2_1, Log_2_2,
     Log_3_1, Log_3_2,
     Log_1_2_1, Log_1_2_2] = get_all_terms(a),

    disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {100,3}}]),
    [Log_2_1, Log_2_2,
     Log_3_1, Log_3_2,
     Log_1_2_1, Log_1_2_2] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5),

    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {60,5}}, {format, external}]),
    disk_log:blog(a, Log_1_1),
    disk_log:blog(a, Log_1_2),
    disk_log:blog(a, Log_2_1),
    disk_log:blog(a, Log_2_2),
    disk_log:change_size(a, {60, 3}),
    ok = disk_log:sync(a),
    {ok, Fd1} = file:open(File ++ ".1", [read]),
    Log11_12 = Log_1_1 ++ Log_1_2,
    {ok,Log11_12} = file:read(Fd1, 200),
    ok = file:close(Fd1),
    {ok, Fd2} = file:open(File ++ ".2", [read]),
    Log21_22 = Log_2_1 ++ Log_2_2,
    {ok,Log21_22} = file:read(Fd2, 200),
    ok = file:close(Fd2),
    disk_log:blog(a, Log_3_1),
    disk_log:blog(a, Log_3_2),
    disk_log:blog(a, Log_1_2_1),
    disk_log:blog(a, Log_1_2_2),
    ok = disk_log:sync(a),
    {ok, Fd2a} = file:open(File ++ ".2", [read]),
    {ok,Log21_22} = file:read(Fd2a, 200),
    ok = file:close(Fd2a),
    {ok, Fd3a} = file:open(File ++ ".3", [read]),
    Log31_32 = Log_3_1 ++ Log_3_2,
    {ok,Log31_32} = file:read(Fd3a, 200),
    ok = file:close(Fd3a),
    {ok, Fd1a} = file:open(File ++ ".1", [read]),
    Log121_122 = Log_1_2_1 ++ Log_1_2_2,
    {ok,Log121_122} = file:read(Fd1a, 200),
    ok = file:close(Fd1a),

    disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {60,3}}, {format, external}]),
    {ok, Fd2b} = file:open(File ++ ".2", [read]),
    {ok,Log21_22} = file:read(Fd2b, 200),
    ok = file:close(Fd2b),
    {ok, Fd3b} = file:open(File ++ ".3", [read]),
    {ok,Log31_32} = file:read(Fd3b, 200),
    ok = file:close(Fd3b),
    {ok, Fd1b} = file:open(File ++ ".1", [read]),
    {ok,Log121_122} = file:read(Fd1b, 200),
    ok = file:close(Fd1b),
    disk_log:close(a),
    del(File, 5),

    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,5}}]),
    disk_log:log(a, Log_1_1),
    disk_log:log(a, Log_1_2),
    disk_log:log(a, Log_2_1),
    disk_log:log(a, Log_2_2),
    disk_log:change_size(a, {60, 3}),
    [Log_1_1, Log_1_2,
     Log_2_1, Log_2_2] = get_all_terms(a),
    disk_log:log(a, Log_3_1),
    disk_log:log(a, Log_1_2_1),
    [Log_2_1, Log_2_2,
     Log_3_1,
     Log_1_2_1] = get_all_terms(a),

    disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {60,3}}]),
    [Log_2_1, Log_2_2,
     Log_3_1,
     Log_1_2_1] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5),

    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {60, 3}}]),
    disk_log:log(a, Log_1_1),
    disk_log:log(a, Log_2_1),
    disk_log:change_size(a, {100, 5}),
    [Log_1_1,
     Log_2_1] = get_all_terms(a),
    disk_log:log(a, Log_2_2),
    disk_log:log(a, Log_3_1),
    disk_log:log(a, Log_3_2),
    disk_log:log(a, Log_4_1),
    disk_log:log(a, Log_4_2),
    disk_log:log(a, Log_5_1),
    disk_log:log(a, Log_5_2),
    disk_log:log(a, Log_1_2_1),
    [Log_2_1, Log_2_2,
     Log_3_1, Log_3_2,
     Log_4_1, Log_4_2,
     Log_5_1, Log_5_2,
     Log_1_2_1] = get_all_terms(a),

    disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100, 5}}]),
    [Log_2_1, Log_2_2,
     Log_3_1, Log_3_2,
     Log_4_1, Log_4_2,
     Log_5_1, Log_5_2,
     Log_1_2_1] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5).



%% Change size of a wrap log file while logging to a file index
%% between the old and the new size.
change_size_during(Conf) when is_list(Conf) ->

    Log_1_1 = "first log  first message",
    Log_1_2 = "first log  second message",
    Log_2_1 = "second log  first message",
    Log_2_2 = "second log  second message",
    Log_3_1 = "third log  first message",
    Log_3_2 = "third log  second message",
    Log_4_1 = "fourth log  first message",
    Log_4_2 = "fourth log  second message",
    Log_5_1 = "fifth log  first message",
    Log_5_2 = "fifth log  second message",
    Log_1_2_1 = "first log  second round 1",
    Log_1_2_2 = "first log  second round 2",
    Log_2_2_1 = "second log  second round 1",
    Log_2_2_2 = "second log  second round 2",
    Log_3_2_1 = "third log  second round 1",
    Log_3_2_2 = "third log  second round 2",
    Log_1_3_1 = "first log  third round 1",
    Log_1_3_2 = "first log  third round 2",

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,5}}]),
    disk_log:log(a, Log_1_1),
    disk_log:log(a, Log_1_2),
    disk_log:log(a, Log_2_1),
    disk_log:log(a, Log_2_2),
    disk_log:log(a, Log_3_1),
    disk_log:log(a, Log_3_2),
    disk_log:log(a, Log_4_1),
    disk_log:log(a, Log_4_2),
    disk_log:log(a, Log_5_1),
    disk_log:log(a, Log_5_2),
    disk_log:log(a, Log_1_1),
    disk_log:log(a, Log_1_2),
    disk_log:log(a, Log_2_1),
    disk_log:log(a, Log_2_2),
    disk_log:log(a, Log_3_1),
    disk_log:log(a, Log_3_2),
    disk_log:log(a, Log_4_1),
    disk_log:log(a, Log_4_2),
    disk_log:change_size(a, {100, 3}),
    [Log_5_1, Log_5_2,
     Log_1_1, Log_1_2,
     Log_2_1, Log_2_2,
     Log_3_1, Log_3_2,
     Log_4_1, Log_4_2] = get_all_terms(a),
    disk_log:log(a, Log_1_2_1),
    disk_log:log(a, Log_1_2_2),
    [Log_2_1, Log_2_2,
     Log_3_1, Log_3_2,
     Log_4_1, Log_4_2,
     Log_1_2_1, Log_1_2_2] = get_all_terms(a),

    disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,3}}]),
    [Log_2_1, Log_2_2,
     Log_3_1, Log_3_2,
     Log_4_1, Log_4_2,
     Log_1_2_1, Log_1_2_2] = get_all_terms(a),
    disk_log:log(a, Log_2_2_1),
    disk_log:log(a, Log_2_2_2),
    disk_log:log(a, Log_3_2_1),
    disk_log:log(a, Log_3_2_2),
    disk_log:log(a, Log_1_3_1),
    disk_log:log(a, Log_1_3_2),
    [Log_2_2_1, Log_2_2_2,
     Log_3_2_1, Log_3_2_2,
     Log_1_3_1, Log_1_3_2] = get_all_terms(a),
    disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,3}}]),
    [Log_2_2_1, Log_2_2_2,
     Log_3_2_1, Log_3_2_2,
     Log_1_3_1, Log_1_3_2] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5),

    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,5}}]),
    disk_log:log(a, Log_1_1),
    disk_log:log(a, Log_1_2),
    disk_log:log(a, Log_2_1),
    disk_log:log(a, Log_2_2),
    disk_log:log(a, Log_3_1),
    disk_log:log(a, Log_3_2),
    disk_log:log(a, Log_4_1),
    disk_log:log(a, Log_4_2),
    disk_log:log(a, Log_5_1),
    disk_log:log(a, Log_5_2),
    disk_log:log(a, Log_1_1),
    disk_log:log(a, Log_1_2),
    disk_log:log(a, Log_2_1),
    disk_log:log(a, Log_2_2),
    disk_log:log(a, Log_3_1),
    disk_log:log(a, Log_3_2),
    disk_log:log(a, Log_4_1),
    disk_log:log(a, Log_4_2),
    disk_log:log(a, Log_5_1),
    disk_log:log(a, Log_5_2),
    disk_log:change_size(a, {100, 3}),
    [Log_1_1, Log_1_2,
     Log_2_1, Log_2_2,
     Log_3_1, Log_3_2,
     Log_4_1, Log_4_2,
     Log_5_1, Log_5_2] = get_all_terms(a),
    disk_log:log(a, Log_1_2_1),
    disk_log:log(a, Log_1_2_2),
    disk_log:log(a, Log_2_2_1),
    disk_log:log(a, Log_2_2_2),
    disk_log:log(a, Log_3_2_1),
    disk_log:log(a, Log_3_2_2),
    disk_log:log(a, Log_1_3_1),
    disk_log:log(a, Log_1_3_2),
    [Log_2_2_1, Log_2_2_2,
     Log_3_2_1, Log_3_2_2,
     Log_1_3_1, Log_1_3_2] = get_all_terms(a),

    disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}, {size, {100,3}}]),
    [Log_2_2_1, Log_2_2_2,
     Log_3_2_1, Log_3_2_2,
     Log_1_3_1, Log_1_3_2] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5).


%% Change size of a wrap log file before we have reached (on the
%% second round) to the file index corresponding to the new size.
change_size_after(Conf) when is_list(Conf) ->

    Log_1_1 = "first log  first message",
    Log_1_2 = "first log  second message",
    Log_2_1 = "second log  first message",
    Log_2_2 = "second log  second message",
    Log_3_1 = "third log  first message",
    Log_3_2 = "third log  second message",
    Log_4_1 = "fourth log  first message",
    Log_4_2 = "fourth log  second message",
    Log_5_1 = "fifth log  first message",
    Log_5_2 = "fifth log  second message",
    Log_1_2_1 = "first log  second round 1",
    Log_1_2_2 = "first log  second round 2",

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {100,5}}]),
    disk_log:log(a, Log_1_1),
    disk_log:log(a, Log_1_2),
    disk_log:log(a, Log_2_1),
    disk_log:log(a, Log_2_2),
    disk_log:log(a, Log_3_1),
    disk_log:log(a, Log_3_2),
    disk_log:log(a, Log_4_1),
    disk_log:log(a, Log_4_2),
    disk_log:log(a, Log_5_1),
    disk_log:log(a, Log_5_2),
    disk_log:log(a, Log_1_1),
    disk_log:log(a, Log_1_2),
    disk_log:log(a, Log_2_1),
    disk_log:log(a, Log_2_2),
    disk_log:change_size(a, {100, 3}),
    [Log_3_1,Log_3_2,
     Log_4_1, Log_4_2,
     Log_5_1, Log_5_2,
     Log_1_1, Log_1_2,
     Log_2_1, Log_2_2] = get_all_terms(a),
    disk_log:log(a, Log_3_1),
    disk_log:log(a, Log_3_2),
    disk_log:log(a, Log_1_2_1),
    disk_log:log(a, Log_1_2_2),
    [Log_2_1, Log_2_2,
     Log_3_1, Log_3_2,
     Log_1_2_1, Log_1_2_2] = get_all_terms(a),

    disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {100,3}}]),
    [Log_2_1, Log_2_2,
     Log_3_1, Log_3_2,
     Log_1_2_1, Log_1_2_2] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5),

    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {100,5}}]),
    disk_log:log(a, Log_1_1),
    disk_log:log(a, Log_1_2),
    disk_log:log(a, Log_2_1),
    disk_log:log(a, Log_2_2),
    disk_log:log(a, Log_3_1),
    disk_log:log(a, Log_3_2),
    disk_log:log(a, Log_4_1),
    disk_log:log(a, Log_4_2),
    disk_log:log(a, Log_5_1),
    disk_log:log(a, Log_5_2),
    disk_log:log(a, Log_1_1),
    disk_log:log(a, Log_1_2),
    disk_log:log(a, Log_2_1),
    disk_log:log(a, Log_2_2),
    disk_log:change_size(a, {60, 3}),
    [Log_3_1,Log_3_2,
     Log_4_1, Log_4_2,
     Log_5_1, Log_5_2,
     Log_1_1, Log_1_2,
     Log_2_1, Log_2_2] = get_all_terms(a),
    disk_log:log(a, Log_3_1),
    disk_log:log(a, Log_1_2_1),
    [Log_2_1, Log_2_2,
     Log_3_1,
     Log_1_2_1] = get_all_terms(a),

    disk_log:close(a),
    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
			     {size, {60,3}}]),
    [Log_2_1, Log_2_2,
     Log_3_1,
     Log_1_2_1] = get_all_terms(a),
    disk_log:close(a),
    del(File, 5).



%% Open an existing wrap log without size option .
default_size(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "a.LOG"),
    {error, {badarg, size}} = disk_log:open([{name,a}, {file, File},
                                                   {type, wrap}]),

    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap},
                                   {size, {100,5}}]),
    disk_log:close(a),

    {ok, a} = disk_log:open([{name,a}, {file, File}, {type, wrap}]),
    {100, 5} = disk_log_1:read_size_file(File),
    ok = disk_log:close(a),
    del(File, 5).

%% Testing change_size/2 a bit more...
change_size2(Conf) when is_list(Conf) ->
    
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 4,
    del(File, No),	% cleanup

    %% External halt.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {size, 100000},
                                   {format, external}, {type, halt}]),
    B = mk_bytes(60), % 56 actually...
    ok = disk_log:blog_terms(n, [B,list_to_binary(B),B]),
    Error1 = {error, {new_size_too_small,n,168}} =
        disk_log:change_size(n, 167),
    "The current size" ++ _ = format_error(Error1),
    ok = disk_log:change_size(n, infinity),
    ok = disk_log:change_size(n, 168),
    ok = disk_log:close(n),
    file:delete(File), % cleanup

    %% External wrap.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {notify, true},
				   {format, external}]),
    BB = mk_bytes(160),
    ok = disk_log:blog_terms(n, [BB, BB, BB, BB]), % create all files
    %% Used to be one message, but now one per wrapped file.
    rec(3, {disk_log, node(), n, {wrap, 0}}),
    ok = disk_log:blog_terms(n, [BB, BB]),
    %% Used to be one message, but now one per wrapped file.
    rec(2, {disk_log, node(), n, {wrap, 1}}),
    ok = disk_log:change_size(n, {100, 2}),
    ok = disk_log:change_size(n, {100, 2}),
    {100, 2} = sz(n),
    ok = disk_log:balog_terms(n, [BB, BB]),
    ok = disk_log:balog_terms(n, [BB]),
    ok = disk_log:blog_terms(n, [BB]),
    %% Used to be one message, but now one per wrapped file.
    rec(4, {disk_log, node(), n, {wrap, 1}}),
    ok = disk_log:change_size(n, {100, 4}),
    ok = disk_log:close(n),
    del(File, No),

    %% Internal wrap.
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {notify, true},
				   {format, internal}]),
    ok = disk_log:blog_terms(n, [BB, BB, BB, BB]), % create all files
    %% Used to be one message, but now one per wrapped file.
    rec(3, {disk_log, node(), n, {wrap, 0}}),
    ok = disk_log:blog_terms(n, [BB, BB]),
    %% Used to be one message, but now one per wrapped file.
    rec(2, {disk_log, node(), n, {wrap, 1}}),
    ok = disk_log:change_size(n, {100, 2}),
    {100, 2} = sz(n),
    ok = disk_log:blog_terms(n, [BB, BB, BB, BB]),
    %% Used to be one message, but now one per wrapped file.
    rec(4, {disk_log, node(), n, {wrap, 1}}),
    ok = disk_log:close(n),
    del(File, No).

%% OTP-3484: truncating index file.
change_size_truncate(Conf) when is_list(Conf) ->
    
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "bert.LOG"),
    No = 3,
    B = mk_bytes(60),

    %% The problem here is truncation of the index file. One cannot easily
    %% check that the index file is correctly updated, but print_index_file()
    %% can be used to follow the progress more closely.

    %% Part 1.
    %% Change the size immediately after creating the log, while there
    %% are no log files. This used to write stuff a negative offset
    %% from the beginning of the file.
    del(File, No+1),
    {ok, bert} = disk_log:open([{name,bert}, {type,wrap}, {file, File},
				      {notify, true}, {size,{1000,255}}]),
    ok = disk_log:change_size(bert,{100,No}),
    ok = disk_log:blog(bert, B),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 0}}),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 0}}),
    3 = curf(bert),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),
    1 = curf(bert),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),

    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),

    %% Three items expected.
    %% disk_log_1:print_index_file("bert.LOG.idx"),
    3 = curf(bert),
    ok = disk_log:change_size(bert,{100,1}),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),
    %% Three items expected.
    %% disk_log_1:print_index_file("bert.LOG.idx"),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),
    %% One item expected.
    %% disk_log_1:print_index_file("bert.LOG.idx"),

    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ok = disk_log:close(bert),
    del(File, No),

    %% Part 2.
    %% Change the size twice, the second time while the the effects of
    %% the first changed have not yet been handled. Finally close before
    %% the index file has been truncated.

    del(File, No),
    {ok, bert} = disk_log:open([{name,bert}, {type,wrap}, {file, File},
				      {notify, true}, {size,{100,No}}]),
    ok = disk_log:blog(bert, B),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 0}}),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 0}}),

    3 = curf(bert),
    ok = disk_log:change_size(bert,{100,No-1}),

    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),

    1 = curf(bert),
    ok = disk_log:change_size(bert,{100,No+1}),

    %% Three items expected.
    %% disk_log_1:print_index_file("bert.LOG.idx"),

    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),

    %% Three items expected.
    %% disk_log_1:print_index_file("bert.LOG.idx"),

    2 = curf(bert),
    ok = disk_log:change_size(bert,{100,1}),

    %% Three items expected.
    %% disk_log_1:print_index_file("bert.LOG.idx"),

    ok = disk_log:close(bert),
    
    %% State: .siz is 1, current file is 2, index file size is 3...

    {ok, bert} = disk_log:open([{name,bert}, {file, File},
				      {type,wrap}, {notify, true}]),
    
    %% Three items expected.
    %% disk_log_1:print_index_file("bert.LOG.idx"),
    
    2 = curf(bert),
    ok = disk_log:blog(bert, B),
    rec(1, {disk_log, node(), bert, {wrap, 1}}),
    ok = disk_log:close(bert),

    {ok, bert} = disk_log:open([{name,bert}, {file, File},
				      {type,wrap}, {notify, true}]),
    
    %% Two items expected.
    %% disk_log_1:print_index_file("bert.LOG.idx"),
    
    1 = curf(bert),
    ok = disk_log:blog(bert, B),
    %% Expect {wrap 0}. Nothing lost now, last wrap notification
    %% reported one lost item.
    rec(1, {disk_log, node(), bert, {wrap, 0}}),

    %% One item expected.
    %% disk_log_1:print_index_file("bert.LOG.idx"),
    ok = disk_log:close(bert),

    del(File, No),
    ok.

%% Change notify and head.
change_attribute(Conf) when is_list(Conf) ->

    Dir = ?privdir(Conf),
    File = filename:join(Dir, "n.LOG"),
    No = 4,
    del(File, No),	% cleanup
    B = mk_bytes(60),

    Q = qlen(),

    %% test change_notify
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}]),
    {ok, n} = disk_log:open([{name, n}]), % ignored...
    ok = disk_log:log_terms(n, [B,B]),
    {error, {badarg, notify}} = disk_log:change_notify(n, self(), wrong),
    ok = disk_log:change_notify(n, self(), false),
    ok = disk_log:change_notify(n, self(), true),
    Error1 = {error, {not_owner, _}} =
        disk_log:change_notify(n, none, true),
    "The pid" ++ _ = format_error(Error1),
    2 = no_written_items(n),
    0 = users(n),
    Parent = self(),
    Pid = spawn(fun() -> disk_log:close(n), Parent ! {self(),done} end),
    receive {Pid, done} -> ok end,
    0 = users(n),
    1 = length(owners(n)),

    %% test change_header
    {error, {badarg, head}} = disk_log:change_header(n, none),
    {error, {badarg, head}} =
	disk_log:change_header(n, {head_func, {1,2,3}}),
    ok = disk_log:change_header(n, {head, header}),
    ok = disk_log:log(n, B),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    4 = no_written_items(n),
    ok = disk_log:change_header(n, {head, none}),
    ok = disk_log:log(n, B),
    rec(1, {disk_log, node(), n, {wrap, 0}}),
    5 = no_written_items(n),
    ok = disk_log:change_header(n,
			     {head_func, {?MODULE, head_fun, [{ok,header}]}}),
    ok = disk_log:log(n, B),
    rec(1, {disk_log, node(), n, {wrap, 1}}),
    7 = no_written_items(n),
    ok = disk_log:close(n),
    {error, no_such_log} = disk_log:close(n),
    del(File, No),
    file:delete(File), % cleanup
    {ok, n} = disk_log:open([{name, n}, {file, File}, {format, external},
				   {type, halt}]),
    {error, {badarg, head}} = disk_log:change_header(n, {head, header}),
    ok = disk_log:change_header(n, {head, "header"}),
    ok = disk_log:close(n),
    file:delete(File),
    
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}]),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}]),
    ok = disk_log:change_notify(n, self(), true),
    ok = disk_log:change_header(n, {head, tjolahopp}),
    {ok, n} = disk_log:open([{name, n}, {file, File}, {type, wrap},
				   {size, {100,No}}, {notify, true}]),
    ok = disk_log:close(n),
    {error, no_such_log} = disk_log:info(n),
    Q = qlen(),
    del(File, No).
    

%% OTP-6278. open/1 creates no status or crash report.
otp_6278(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    File = filename:join(Dir, "no_such_dir/no_such_file"),
    error_logger:add_report_handler(?MODULE, self()),
    {error, {file_error, _, _}} =
        disk_log:open([{name,n},{file,File}]),
    receive
	{crash_report,_Pid,Report} ->
	    io:format("Unexpected: ~p\n", [Report]),
	    ct:fail(failed)
    after 1000 ->
            ok
    end,
    error_logger:delete_report_handler(?MODULE).

%% OTP-10131. head_func type.
otp_10131(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    Log = otp_10131,
    File = filename:join(Dir, lists:concat([Log, ".LOG"])),
    HeadFunc = {?MODULE, head_fun, [{ok,"head"}]},
    {ok, Log} = disk_log:open([{name,Log},{file,File},
                               {head_func, HeadFunc}]),
    HeadFunc = info(Log, head, undef),
    HeadFunc2 = {?MODULE, head_fun, [{ok,"head2"}]},
    ok = disk_log:change_header(Log, {head_func, HeadFunc2}),
    HeadFunc2 = info(Log, head, undef),
    ok = disk_log:close(Log),
    ok.

%% OTP-16768. Bad number of items with truncate/1. ERL-1312, ERL-1313.
otp_16768(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    Log = otp_16768,
    File = filename:join(Dir, Log),
    Header = <<"123456789\n">>,
    head_count(Log, File, Header, external, 25),
    head_count(Log, File, none, external, 20),
    head_count(Log, File, Header, internal, 30),
    head_count(Log, File, none, internal, 20),
    ok.

head_count(Log, File, Header, Format, Expected) ->
    del(File, 10),
    Content = <<"1234567890123456789\n">>,
    HeaderSize = case Header of
                     none -> 0;
                     _ -> byte_size(Header)
                 end,
    %% 5 files for the external format, more for the internal format
    MaxSizePerFile = HeaderSize + (5 * byte_size(Content)) - 1,
    {ok, Log} = disk_log:open([{file, File},
                               {name, Log},
                               {format, Format},
                               {head, Header},
                               {size, {MaxSizePerFile, 999}},
                               {type, wrap}
                              ]),
    ok = disk_log:truncate(Log),
    lists:foreach(fun(_I) -> disk_log:blog(Log, Content) end,
                  lists:seq(1, 20)),
    DiskLogInfo = disk_log:info(Log),
    Expected = proplists:get_value(no_items, DiskLogInfo),
    ok = disk_log:close(Log).

otp_16809(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    Log = otp_16809,
    File = filename:join(Dir, lists:concat([Log, ".LOG"])),
    {ok, Log} = disk_log:open([{name,Log},{file,File}]),
    none = info(Log, head, undef),
    ok = disk_log:change_header(Log, {head, none}),
    none = info(Log, head, undef),
    ok = disk_log:change_header(Log, {head, some_header}),
    %% Notice: the head argument as a binary:
    true = term_to_binary(some_header) =:= info(Log, head, undef),
    HeadFunc = {?MODULE, head_fun, [{ok,a_header}]},
    ok = disk_log:change_header(Log, {head_func, HeadFunc}),
    HeadFunc = info(Log, head, undef),
    ok = disk_log:close(Log),

    {ok, Log} = disk_log:open([{name,Log},
                               {file,File},
                               {format,external}]),
    none = info(Log, head, undef),
    ok = disk_log:change_header(Log, {head, none}),
    none = info(Log, head, undef),
    ok = disk_log:change_header(Log, {head, "some header"}),
    %% Notice: the head argument as a binary:
    <<"some header">> = info(Log, head, undef),
    HeadFunc2 = {?MODULE, head_fun, [{ok,"a header"}]},
    ok = disk_log:change_header(Log, {head_func, HeadFunc2}),
    HeadFunc2 = info(Log, head, undef),
    ok = disk_log:close(Log).

decrease_size_with_chunk_step(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    Log = decrease_size_with_chunk_step,
    File = filename:join(Dir, lists:concat([Log, ".LOG"])),
    {ok, Log} = disk_log:open([{size, {50, 3}}, {name, Log}, {type, wrap},
                               {file, File}, {notify, true}]),
    eof = disk_log:chunk(Log, start, 1),
    {error, end_of_log} = disk_log:chunk_step(Log, start, 1),
    ok = disk_log:log_terms(Log, [1, 2, 3, 4, 5, 6, 7, 8, 9]),
    ok = disk_log:close(Log),
    {ok, Log} = disk_log:open([{name, Log}, {type, wrap}, {file, File},
                               {notify, true}]),
    %% Decrease maximum number of files from 3 to 2.
    ok = disk_log:change_size(Log, {50, 2}),
    %% The exception error of rem/2 operator should not occur in here.
    {ok, Cont} = disk_log:chunk_step(Log, start, 2),
    %% Verify that chunk_step has stepped to old max file (3)
    {_, [7, 8, 9]} = disk_log:chunk(Log, Cont),
    %% Continue to append the items to the log in order to make sure it can work
    %% as normal.
    ok = disk_log:log_terms(Log, [9, 8, 7, 6, 5, 4, 3, 2, 1]),
    %% Verify that log files were decreased to 2 after wrapping
    [6, 5, 4, 3, 2, 1] = get_all_terms(Log),
    ok = disk_log:close(Log),
    del(File, 2).

decrease_size_twice(Conf) when is_list(Conf) ->
    Dir = ?privdir(Conf),
    Log = decrease_size_twice,
    File = filename:join(Dir, lists:concat([Log, ".LOG"])),

    Data_1 = [1, 2, 3],
    Data_2 = [4, 5, 6],
    Data_3 = [7, 8, 9],
    Data_2_3 = Data_2 ++ Data_3,
    Data = Data_1 ++ Data_2_3,

    {ok, Log} = disk_log:open([{size, {50, 3}}, {name, Log}, {type, wrap},
                               {file, File}, {notify, true}]),

    ok = disk_log:log_terms(Log, Data),

    %% Changing size to the same size should make no changes
    ok = disk_log:change_size(Log, {50, 3}),
    Data = get_all_terms(Log),

    %% Changing size to smaller and then again to the same smaller size,
    %% should leave OldMaxF as it was before
    ok = disk_log:change_size(Log, {50, 2}),
    ok = disk_log:change_size(Log, {50, 2}),
    Data = get_all_terms(Log),

    %% After writing data with new (smaller) log size, some terms should be truncated
    ok = disk_log:log_terms(Log, Data),
    Data_2_3 = get_all_terms(Log),

    %% Changing size to bigger should remove OldMaxF
    ok = disk_log:change_size(Log, {50, 3}),
    ok = disk_log:log_terms(Log, Data),
    Data = get_all_terms(Log),

    %% Changing size to smaller, and then to even smaller should keep bigger OldMaxF
    ok = disk_log:change_size(Log, {50, 2}),
    ok = disk_log:change_size(Log, {50, 1}),
    Data = get_all_terms(Log),

    %% After writing data with new (even smaller) log size, some terms should be truncated
    ok = disk_log:log_terms(Log, Data),
    Data_3 = get_all_terms(Log),

    %% Changing size to bigger should remove OldMaxF
    ok = disk_log:change_size(Log, {50, 3}),
    ok = disk_log:log_terms(Log, Data),
    Data = get_all_terms(Log),

    %% Changing size to smaller, writing some data (but less than required to cause logs to wrap),
    %% and then to even smaller should keep bigger OldMaxF
    ok = disk_log:log_terms(Log, lists:seq(1, 7)),
    ok = disk_log:change_size(Log, {50, 2}),
    ok = disk_log:log_terms(Log, [8, 9]),
    ok = disk_log:change_size(Log, {50, 1}),
    Data = get_all_terms(Log),

    %% Changing size to bigger and than to bigger again, should allow to read all data
    ok = disk_log:change_size(Log, {50, 2}),
    Data = get_all_terms(Log),
    ok = disk_log:change_size(Log, {50, 3}),
    Data = get_all_terms(Log),

    ok = disk_log:close(Log),

    del(File, 3).

mark(FileName, What) ->
    {ok,Fd} = file:open(FileName, [raw, binary, read, write]),
    {ok,_} = file:position(Fd, 4),
    ok = file:write(Fd, What),
    ok = file:close(Fd).

crash(File, Where) ->
    {ok, Fd} = file:open(File, [read,write]),
    file:position(Fd, Where),
    ok = file:write(Fd, [10]),
    ok = file:close(Fd).

unwritable(Fname) ->
    {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode - 8#00200,
    file:write_file_info(Fname, Info#file_info{mode = Mode}).

writable(Fname) ->
    {ok, Info} = file:read_file_info(Fname),
    Mode = Info#file_info.mode bor 8#00200,
    file:write_file_info(Fname, Info#file_info{mode = Mode}).

truncate(File, Where) ->
    {ok, Fd} = file:open(File, [read,write]),
    file:position(Fd, Where),
    ok = file:truncate(Fd),
    ok = file:close(Fd).

file_size(File) ->
    {ok, F} = file:read_file_info(File),
    F#file_info.size.

copy_wrap_log(FromName, N, FromDir, ToDir) ->
    copy_wrap_log(FromName, FromName, N, FromDir, ToDir).

copy_wrap_log(FromName, ToName, N, FromDir, ToDir) ->
    Fun = fun(E) ->
	     From = join(FromDir, io_lib:format("~s.~p", [FromName, E])),
	     To = join(ToDir, io_lib:format("~s.~p", [ToName, E])),
	     case file:read_file_info(From) of
                 {ok, _FileInfo} ->
                     copy_file(From, To);
                 _Else ->
                     ok
             end
	  end,
    Exts = [idx, siz | lists:seq(1, N)],
    lists:foreach(Fun, Exts).

-define(BUFSIZE, 8192).

copy_file(Src, Dest) ->
    %% io:format("copying from ~p to ~p~n", [Src, Dest]),
    {ok, InFd} = file:open(Src, [raw, binary, read]),
    {ok, OutFd} = file:open(Dest, [raw, binary, write]),
    ok = copy_file1(InFd, OutFd),
    file:close(InFd),
    file:close(OutFd),
    ok = file:change_mode(Dest, 8#0666).

copy_file1(InFd, OutFd) ->
    case file:read(InFd, ?BUFSIZE) of
        {ok, Bin} ->
            ok = file:write(OutFd, Bin),
            copy_file1(InFd, OutFd);
        eof  ->
            ok
    end.


join(A, B) ->
    filename:nativename(filename:join(A, B)).

add_ext(Name, Ext) ->
    lists:concat([Name, ".", Ext]).

log(_Name, 0) ->
    ok;
log(Name, N) ->
    ok = disk_log:log(Name, "this is a logged message number " ++ 
                      integer_to_list(N)),
    log(Name, N-1).

format_error(E) ->
    lists:flatten(disk_log:format_error(E)).

check_pps({Ports0,Procs0} = P0) ->
    case pps() of
        P0 ->
            ok;
        _ ->
            timer:sleep(500),
            case pps() of
                P0 ->
                    ok;
                {Ports1,Procs1} = P1 ->
		    case {Ports1 -- Ports0, Procs1 -- Procs0} of
			{[], []} -> ok;
			{PortsDiff,ProcsDiff} ->
			    io:format("failure, got ~p~n, expected ~p\n", [P1, P0]),
			    show("Old port", Ports0 -- Ports1),
			    show("New port", PortsDiff),
			    show("Old proc", Procs0 -- Procs1),
			    show("New proc", ProcsDiff),
			    ct:fail(failed)
		    end
	    end
    end.

show(_S, []) ->
    ok;
show(S, [{Pid, Name, InitCall}|Pids]) when is_pid(Pid) ->
    io:format("~s: ~w (~w), ~w: ~p~n",
              [S, Pid, proc_reg_name(Name), InitCall,
               erlang:process_info(Pid)]),
    show(S, Pids);
show(S, [{Port, _}|Ports]) when is_port(Port)->
    io:format("~s: ~w: ~p~n", [S, Port, erlang:port_info(Port)]),
    show(S, Ports).

pps() ->
    timer:sleep(100),
    {port_list(), process_list()}.

port_list() ->
    [{P,safe_second_element(erlang:port_info(P, name))} ||
        P <- erlang:ports()].

process_list() ->
    [{P,process_info(P, registered_name),
      safe_second_element(process_info(P, initial_call))} ||
        P <- processes(), erlang:is_process_alive(P)].

proc_reg_name({registered_name, Name}) -> Name;
proc_reg_name([]) -> no_reg_name.

safe_second_element({_,Info}) -> Info;
safe_second_element(Other) -> Other.


qlen() ->
    {_, {_, N}} = lists:keysearch(message_queue_len, 1, process_info(self())),
    N.

owners(Log) ->
%%     io:format("owners ~p~n", [info(Log, owners, -1)]),
    info(Log, owners, -1).
users(Log) ->
%%     io:format("users ~p~n", [info(Log, users, -1)]),
    info(Log, users, -1).
status(Log) ->
%%     io:format("status ~p~n", [info(Log, status, -1)]),
    info(Log, status, -1).
no_items(Log) ->
%%     io:format("no_items ~p~n", [info(Log, no_items, -1)]),
    info(Log, no_items, -1).
no_written_items(Log) ->
%%     io:format("no_written_items ~p~n", [info(Log, no_written_items, -1)]),
    info(Log, no_written_items, -1).
sz(Log) -> 
%%     io:format("sz ~p~n", [info(Log, size, -1)]),
    info(Log, size, -1).
curb(Log) -> 
%%     io:format("curb ~p~n", [info(Log, no_current_bytes, -1)]),
    info(Log, no_current_bytes, -1).
curf(Log) -> 
%%     io:format("curf ~p~n", [info(Log, current_file, -1)]),
    info(Log, current_file, -1).
cur_cnt(Log) -> 
%%     io:format("cur_cnt ~p~n", [info(Log, no_current_items, -1)]),
    info(Log, no_current_items, -1).
no_overflows(Log) ->
%%     io:format("no_overflows ~p~n", [info(Log, no_overflows, -1)]),
    info(Log, no_overflows, -1).

info(Log, What, Undef) ->
    case lists:keysearch(What, 1, disk_log:info(Log)) of
        {value, {What, Value}} -> Value;
        false -> Undef
    end.

rec(0, _) ->
     ok;
rec(N, Msg) ->
    receive
	Msg ->
	    rec(N-1, Msg)
    after 100 ->
	    test_server_fail({no_msg, N, Msg})
    end.

%%-----------------------------------------------------------------
%% The error_logger handler used.
%% (Copied from stdlib/test/proc_lib_SUITE.erl.)
%%-----------------------------------------------------------------
init(Tester) ->
    {ok, Tester}.
    
handle_event({error_report, _GL, {Pid, crash_report, Report}}, Tester) ->
    Tester ! {crash_report, Pid, Report},
    {ok, Tester};
handle_event({info_msg, _GL, {Pid, F,A}}, Tester) ->
    Tester ! {info_msg, Pid, F, A},
    {ok, Tester};
handle_event(_Event, State) ->
    {ok, State}.

handle_info(_, State) ->
    {ok, State}.

handle_call(_Query, State) -> {ok, {error, bad_query}, State}.

terminate(_Reason, State) ->
    State.
