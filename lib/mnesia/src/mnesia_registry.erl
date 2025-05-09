%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright Ericsson AB 1998-2025. All Rights Reserved.
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

%%
-module(mnesia_registry).
-moduledoc """
This module is deprecated and the functions should not be used.

This module was intended for internal use within OTP by the `erl_interface` application,
but it has two functions that are exported for public use.

Since the `erl_interface` have removed the registry functionality a long time ago,
these functions are deprecated.

## See Also
 `m:mnesia`
""".

%%%----------------------------------------------------------------------
%%% File    : mnesia_registry.erl
%%% Purpose : Support dump and restore of a registry on a C-node
%%%           This is an OTP internal module and is not public available.
%%%
%%% Example : Dump some hardcoded records into the Mnesia table Tab
%%%
%%% 	  case rpc:call(Node, mnesia_registry, start_dump, [Tab, self()]) of
%%% 	     Pid when pid(Pid) ->
%%% 		 Pid ! {write, key1, key_size1, val_type1, val_size1, val1},
%%% 		 Pid ! {delete, key3},
%%% 		 Pid ! {write, key2, key_size2, val_type2, val_size2, val2},
%%% 		 Pid ! {write, key4, key_size4, val_type4, val_size4, val4},
%%% 		 Pid ! {commit, self()},
%%% 		 receive
%%% 		     {ok, Pid} ->
%%% 			 ok;
%%% 		     {'EXIT', Pid, Reason} ->
%%% 			 exit(Reason)
%%% 		 end;
%%% 	     {badrpc, Reason} ->
%%% 		 exit(Reason)
%%% 	 end.
%%%
%%% Example : Restore the corresponding Mnesia table Tab
%%%
%%% 	  case rpc:call(Node, mnesia_registry, start_restore, [Tab, self()]) of
%%% 	     {size, Pid, N, LargestKey, LargestVal} ->
%%% 		 Pid ! {send_records, self()},
%%%              Fun = fun() ->
%%%                        receive
%%%                            {restore, KeySize, ValSize, ValType, Key, Val} -> 
%%%                                {Key, Val};
%%% 		               {'EXIT', Pid, Reason} ->
%%% 			           exit(Reason)
%%%                        end
%%% 		       end,
%%%              lists:map(Fun, lists:seq(1, N));
%%% 	     {badrpc, Reason} ->
%%% 		 exit(Reason)
%%% 	 end.
%%%
%%%----------------------------------------------------------------------

%% External exports
%% Avoid warning for local function max/2 clashing with autoimported BIF.
-compile({no_auto_import,[max/2]}).
-export([start_dump/2, start_restore/2]).
-export([create_table/1, create_table/2]).

-deprecated([{create_table, '_', "use mnesia:create_table/2 instead"}]).

%% Internal exports
-export([init/4]).

-record(state, {table, ops = [], link_to}).

-record(registry_entry, {key, key_size, val_type, val_size, val}).

-record(size, {pid = self(), n_values = 0, largest_key = 0, largest_val = 0}).

%%%----------------------------------------------------------------------
%%% Client
%%%----------------------------------------------------------------------

start(Type, Tab, LinkTo) ->
    Starter = self(),
    Args = [Type, Starter, LinkTo, Tab],
    Pid = spawn_link(?MODULE, init, Args),
    %% The receiver process may unlink the current process
    receive
	{ok, Res} ->
	    Res;
	{'EXIT', Pid, Reason} when LinkTo == Starter ->
	    exit(Reason)
    end.

%% Starts a receiver process and optionally creates a Mnesia table
%% with suitable default values. Returns the Pid of the receiver process
%% 
%% The receiver process accumulates Mnesia operations and performs
%% all operations or none at commit. The understood messages are:
%% 
%%    {write, Key, KeySize, ValType, ValSize, Val} ->
%%        accumulates mnesia:write({Tab, Key, KeySize, ValType, ValSize, Val})
%%                                                    (no reply)
%%    {delete, Key}     ->
%%        accumulates mnesia:delete({Tab, Key})       (no reply)
%%    {commit, ReplyTo} ->
%%        commits all accumulated operations
%%        and stops the process                       (replies {ok, Pid})
%%    abort             ->
%%        stops the process                           (no reply)
%%    
%% The receiver process is linked to the process with the process identifier
%% LinkTo. If some error occurs the receiver process will invoke exit(Reason)
%% and it is up to he LinkTo process to act properly when it receives an exit
%% signal.

-doc false.
start_dump(Tab, LinkTo) ->
    start(dump, Tab, LinkTo).

%% Starts a sender process which sends restore messages back to the
%% LinkTo process. But first are some statistics about the table
%% determined and returned as a 5-tuple:
%% 
%%    {size, SenderPid, N, LargestKeySize, LargestValSize}
%%
%% where N is the number of records in the table. Then the sender process
%% waits for a 2-tuple message:
%% 
%%    {send_records, ReplyTo}
%%
%% At last N 6-tuple messages is sent to the ReplyTo process:
%% 
%%    ReplyTo !  {restore, KeySize, ValSize, ValType, Key, Val}
%%
%% If some error occurs the receiver process will invoke exit(Reason)
%% and it is up to he LinkTo process to act properly when it receives an
%% exit signal.

-doc false.
start_restore(Tab, LinkTo) ->
    start(restore, Tab, LinkTo).

-doc """
> #### Warning {: .warning }
>
> _This function is deprecated. Do not use it._
>

A wrapper function for `mnesia:create_table/2`, which creates a table (if there
is no existing table) with an appropriate set of `attributes`. The table only
resides on the local node and its storage type is the same as the `schema` table
on the local node, that is, `{ram_copies,[node()]}` or `{disc_copies,[node()]}`.

This function is used by `erl_interface` to create the Mnesia table if it does
not already exist.
""".
-spec create_table(Tab :: atom()) -> 'ok'.
%% Optionally creates the Mnesia table Tab with suitable default values.
%% Returns ok or EXIT's
create_table(Tab) ->
    Storage = mnesia:table_info(schema, storage_type),
    create_table(Tab, [{Storage, [node()]}]).

-doc """
> #### Warning {: .warning }
>
> _This function is deprecated. Do not use it._
>

A wrapper function for `mnesia:create_table/2`, which creates a table (if there
is no existing table) with an appropriate set of `attributes`. The attributes
and `TabDef` are forwarded to `mnesia:create_table/2`. For example, if the table
is to reside as `disc_only_copies` on all nodes, a call looks as follows:

```erlang
          TabDef = [{{disc_only_copies, node()|nodes()]}],
          mnesia_registry:create_table(my_reg, TabDef)
```
""".
-spec create_table(Tab :: atom(), Opt :: [{atom(), term()}]) -> ok.
create_table(Tab, TabDef) ->
    Attrs = record_info(fields, registry_entry),
    case mnesia:create_table(Tab, [{attributes, Attrs} | TabDef]) of
	{atomic, ok} ->
	    ok;
	{aborted, {already_exists, Tab}} ->
	    ok;
	{aborted, Reason} ->
	    exit(Reason)
    end.
    
%%%----------------------------------------------------------------------
%%% Server
%%%----------------------------------------------------------------------

-doc false.
init(Type, Starter, LinkTo, Tab) ->
    if
	LinkTo /= Starter ->
	    link(LinkTo),
	    unlink(Starter);
	true ->
	    ignore
    end,
    case Type of
	dump ->
	    Starter ! {ok, self()},
	    dump_loop(#state{table = Tab, link_to = LinkTo});
	restore ->
	    restore_table(Tab, Starter, LinkTo)
    end.

%%%----------------------------------------------------------------------
%%% Dump loop    
%%%----------------------------------------------------------------------

dump_loop(S) ->
    Tab = S#state.table,
    Ops = S#state.ops,
    receive
	{write, Key, KeySize, ValType, ValSize, Val} ->
	    RE = #registry_entry{key = Key,
				 key_size = KeySize,
				 val_type = ValType,
				 val_size = ValSize,
				 val = Val},
	    dump_loop(S#state{ops = [{write, RE} | Ops]});
	{delete, Key} ->
	    dump_loop(S#state{ops = [{delete, Key} | Ops]});
	{commit, ReplyTo} ->
	    create_table(Tab),
	    RecName = mnesia:table_info(Tab, record_name),
	    %% The Ops are in reverse order, but there is no need
	    %% for reversing the list of accumulated operations
	    case mnesia:transaction(fun handle_ops/3, [Tab, RecName, Ops]) of
                {atomic, ok} ->
                    ReplyTo ! {ok, self()},
                    stop(S#state.link_to);
                {aborted, Reason} ->
                    exit({aborted, Reason})
            end;
	abort ->
	    stop(S#state.link_to);
        BadMsg ->
            exit({bad_message, BadMsg})					   
    end.

stop(LinkTo) ->
    unlink(LinkTo),
    exit(normal).

%% Grab a write lock for the entire table
%% and iterate over all accumulated operations
handle_ops(Tab, RecName, Ops) ->
    mnesia:write_lock_table(Tab),
    do_handle_ops(Tab, RecName, Ops).

do_handle_ops(Tab, RecName, [{write, RegEntry} | Ops]) ->
    Record = setelement(1, RegEntry, RecName),
    mnesia:write(Tab, Record, write),
    do_handle_ops(Tab, RecName, Ops);
do_handle_ops(Tab, RecName, [{delete, Key} | Ops]) ->
    mnesia:delete(Tab, Key, write),
    do_handle_ops(Tab, RecName, Ops);
do_handle_ops(_Tab, _RecName, []) ->
    ok.
    
%%%----------------------------------------------------------------------
%%% Restore table
%%%----------------------------------------------------------------------

restore_table(Tab, Starter, LinkTo) ->
    Pat = mnesia:table_info(Tab, wild_pattern),
    Fun = fun() -> mnesia:match_object(Tab, Pat, read) end,
    case mnesia:transaction(Fun) of
	{atomic, AllRecords} ->
	    Size = calc_size(AllRecords, #size{}),
	    Starter ! {ok, Size},
	    receive
		{send_records, ReplyTo} -> 
		    send_records(AllRecords, ReplyTo),
		    unlink(LinkTo),
		    exit(normal);
		BadMsg ->
		    exit({bad_message, BadMsg})
	    end;
	{aborted, Reason} ->
            exit(Reason)
    end.

calc_size([H | T], S) ->
    KeySize = max(element(#registry_entry.key_size, H), S#size.largest_key),
    ValSize = max(element(#registry_entry.val_size, H), S#size.largest_val),
    N = S#size.n_values + 1,
    calc_size(T, S#size{n_values = N, largest_key = KeySize, largest_val = ValSize});
calc_size([], Size) ->
    Size.

max(New, Old) when New > Old -> New;
max(_New, Old) -> Old.

send_records([H | T], ReplyTo) ->
    KeySize = element(#registry_entry.key_size, H),
    ValSize = element(#registry_entry.val_size, H),
    ValType = element(#registry_entry.val_type, H),
    Key = element(#registry_entry.key, H),
    Val = element(#registry_entry.val, H),
    ReplyTo ! {restore, KeySize, ValSize, ValType, Key, Val},
    send_records(T, ReplyTo);
send_records([], _ReplyTo) ->
    ok.

