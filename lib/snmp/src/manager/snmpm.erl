%% 
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2004-2021. All Rights Reserved.
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

-module(snmpm).

%%----------------------------------------------------------------------
%% This module implements a simple SNMP manager for Erlang.
%%----------------------------------------------------------------------

%% User interface
%% Avoid warning for local function demonitor/1 clashing with autoimported BIF.
-compile({no_auto_import,[demonitor/1]}).
-export([
	 %% 
	 %% Management API
	 start/0, start/1, 
	 start_link/0, start_link/1, 
	 stop/0, stop/1,

	 monitor/0, demonitor/1, 
	 notify_started/1, cancel_notify_started/1, 

	 backup/1, 

	 load_mib/1, unload_mib/1, 
	 which_mibs/0, 
	 name_to_oid/1, oid_to_name/1, oid_to_type/1, 

	 register_user/3, register_user/4, 
	 register_user_monitor/3, register_user_monitor/4, 
	 unregister_user/1, 
	 which_users/0, 

	 register_agent/2, register_agent/3, register_agent/4, 
	 unregister_agent/2, unregister_agent/3,
	 which_agents/0, which_agents/1, 
	 agent_info/2, update_agent_info/3, update_agent_info/4, 
	 
	 register_usm_user/3, unregister_usm_user/2, 
	 which_usm_users/0, which_usm_users/1, 
	 usm_user_info/3, update_usm_user_info/4, 
	 
	 %% 
	 %% Basic SNMP API (version "3").
	 sync_get2/3, sync_get2/4, 
	 async_get2/3, async_get2/4, 
	 sync_get_next2/3, sync_get_next2/4, 
	 async_get_next2/3, async_get_next2/4, 
	 sync_set2/3, sync_set2/4, 
	 async_set2/3, async_set2/4, 
	 sync_get_bulk2/5, sync_get_bulk2/6, 
	 async_get_bulk2/5, async_get_bulk2/6, 

	 cancel_async_request/2, 
	 
	 %% 
	 %% Extended SNMP API
	 %% discovery/2, discovery/3, discovery/4, discovery/5, discovery/6, 

	 %% 
	 %% Logging
	 log_to_txt/1, log_to_txt/2, log_to_txt/3, log_to_txt/4, 
	 log_to_txt/5, log_to_txt/6, log_to_txt/7, log_to_txt/8, 
	 log_to_io/1,  log_to_io/2,  log_to_io/3,  log_to_io/4, 
	 log_to_io/5,  log_to_io/6,  log_to_io/7, 
	 change_log_size/1,
	 get_log_type/0,
	 set_log_type/1,

	 reconfigure/0,

	 system_start_time/0,
	 sys_up_time/0,

	 info/0, info/1,
	 verbosity/2,
         restart/1
	]).

-export([format_reason/1, format_reason/2]).

%% Application internal export
-export([start_link/3, snmpm_start_verify/2, snmpm_start_verify/3]).
-export([target_name/1, target_name/2]).

-export_type([
	      register_timeout/0, 
	      agent_config/0, 
	      target_name/0,
              pdu_type/0
	     ]).

-include_lib("snmp/src/misc/snmp_debug.hrl").
-include_lib("snmp/include/snmp_types.hrl").
-include("snmpm_atl.hrl").
-include("snmpm_internal.hrl").
-include("snmp_verbosity.hrl").

-define(DEFAULT_AGENT_PORT, 161).
-define(ATL_BLOCK_DEFAULT,  true).


%%-----------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------

-type register_timeout() :: pos_integer() | snmp:snmp_timer().
-type agent_config() :: {engine_id,        snmp:engine_id()}   | % Mandatory
			{address,          inet:ip_address()}  | % Mandatory
			{port,             inet:port_number()} | % Optional
			{tdomain,          snmp:tdomain()}     | % Optional
			{community,        snmp:community()}   | % Optional
			{timeout,          register_timeout()} | % Optional
			{max_message_size, snmp:mms()}         | % Optional
			{version,          snmp:version()}     | % Optional
			{sec_moduel,       snmp:sec_model()}   | % Optional
			{sec_name,         snmp:sec_name()}    | % Optional
			{sec_level,        snmp:sec_level()}.    % Optional
-type target_name() :: string().
-type pdu_type() :: snmp:pdu_type() | 'trappdu'.


%% This function is called when the snmp application
%% starts. 
start_link(Opts, normal, []) ->
    start_link(Opts).


simple_conf() ->
    Vsns      = [v1, v2, v3],
    {ok, Cwd} = file:get_cwd(),
    %% Check if the manager config file exist, if not create it
    MgrConf = filename:join(Cwd, "manager.conf"),
    case file:read_file_info(MgrConf) of
	{ok, _} ->
	    ok;
	_ ->
	    ok = snmp_config:write_manager_config(Cwd, "",
						  [{port, 5000},
						   {engine_id, "mgrEngine"},
						   {max_message_size, 484}])
    end,
    Conf = [{dir, Cwd}, {db_dir, Cwd}],
    [{versions, Vsns}, {config, Conf}].

%% Simple start. Start a manager with default values.
start_link() ->
    start_link(simple_conf()).

%% This function is normally not used. Instead the manager is
%% started as a consequence of a call to application:start(snmp)
%% when {snmp, [{manager, Options}]} is present in the
%% node config file.
start_link(Opts) ->
    %% This start the manager top supervisor, which in turn
    %% starts the other processes.
    {ok, _} = snmpm_supervisor:start_link(normal, Opts),
    ok.

%% Simple start. Start a manager with default values.
start() ->
    start(simple_conf()).
    
start(Opts) ->
    %% This start the manager top supervisor, which in turn
    %% starts the other processes.
    {ok, Pid} = snmpm_supervisor:start_link(normal, Opts),
    unlink(Pid),
    ok.

stop() ->
    stop(0).

stop(Timeout) when (Timeout =:= infinity) orelse
                   (is_integer(Timeout) andalso (Timeout >= 0)) ->
    snmpm_supervisor:stop(Timeout).



-spec monitor() -> Ref when Ref :: reference().
monitor() ->
    erlang:monitor(process, snmpm_supervisor).

%% -spec demonitor(Ref) -> void() when Ref :: reference().
demonitor(Ref) ->
    erlang:demonitor(Ref).
	

-define(NOTIFY_START_TICK_TIME, 500).

-spec notify_started(Timeout) -> Pid when Timeout :: integer(),
   Pid :: pid().
notify_started(To) when is_integer(To) andalso (To > 0) ->
    spawn_link(?MODULE, snmpm_start_verify, [self(), To]).

%% -spec cancel_notify_started(Pid) -> void() when Pid :: pid().
cancel_notify_started(Pid) ->
    Pid ! {cancel, self()},
    ok.

snmpm_start_verify(Parent, To) ->
    ?d("starting", []),
    snmpm_start_verify(Parent, monitor(), To).

snmpm_start_verify(Parent, _Ref, To) when (To =< 0) ->
    ?d("timeout", []),
    unlink(Parent),
    Parent ! {snmpm_start_timeout, self()};
snmpm_start_verify(Parent, Ref, To) ->
    T0 = t(),
    receive
	{cancel, Parent} ->
	    ?d("cancel", []),
	    demonitor(Ref),
	    unlink(Parent),
	    exit(normal);
	{'EXIT', Parent, _} ->
	    exit(normal);
	{'DOWN', Ref, process, _Object, _Info} ->
	    ?d("down", []),
	    sleep(?NOTIFY_START_TICK_TIME),
	    ?MODULE:snmpm_start_verify(Parent, monitor(), t(T0, To))
    after ?NOTIFY_START_TICK_TIME ->
	    ?d("down timeout", []),
	    demonitor(Ref),
	    case snmpm_server:is_started() of
		true ->
		    unlink(Parent),
		    Parent ! {snmpm_started, self()};
		_ ->
		    ?MODULE:snmpm_start_verify(Parent, monitor(), t(T0, To))
	    end
    end.

t(T0, T)  -> T - (t() - T0).
t()       -> snmp_misc:now(ms).
sleep(To) -> snmp_misc:sleep(To).


%% -- Misc --

-spec backup(BackupDir) -> ok | {error, Reason :: term()}
                when BackupDir :: string().
backup(BackupDir) ->
    snmpm_config:backup(BackupDir).


%% -- Mibs --

%% Load a mib into the manager
-spec load_mib(Mib) -> ok | {error, Reason} when Mib :: MibName,
   MibName :: string(),
   Reason :: term().
load_mib(MibFile) ->
    snmpm_server:load_mib(MibFile).

%% Unload a mib from the manager
-spec unload_mib(Mib) -> ok | {error, Reason} when Mib :: MibName,
   MibName :: string(),
   Reason :: term().
unload_mib(Mib) ->
    snmpm_server:unload_mib(Mib).

%% Which mib's are loaded
-spec which_mibs() -> Mibs when Mibs :: [{MibName, MibFile}],
   MibName :: atom(),
   MibFile :: string().
which_mibs() ->
    snmpm_config:which_mibs().

%% Get all the possible oid's for the aliasname
%% -spec name_to_oid(Name) -> {ok, Oids} | {error, Reason :: term()}
%%                      when Name :: atom(), Oids :: [oid()].
name_to_oid(Name) ->
    snmpm_config:name_to_oid(Name).

%% Get the aliasname for an oid
%% -spec oid_to_name(Oid) -> {ok, Name} | {error, Reason}
%%                      when Oid :: oid(), Name :: atom(), Reason :: term().
oid_to_name(Oid) ->
    snmpm_config:oid_to_name(Oid).

%% Get the type for an oid
%% -spec oid_to_type(Oid) -> {ok, Type} | {error, Reason}
%%                      when Oid :: oid(), Type :: atom(), Reason :: term().
oid_to_type(Oid) ->
    snmpm_config:oid_to_type(Oid).


%% -- Info -- 

-spec info() -> [{Key, Value}] when Key :: atom(),
   Value :: term().
info() ->
    snmpm_server:info().

info(Key) ->
    proplists:get_value(Key, info(), {error, not_found}).


%% -- Verbosity -- 

%% Change the verbosity of a process in the manager
%% -spec verbosity(Ref, Verbosity) -> void() when Ref :: server | config | net_if | note_store | all,
%%    Verbosity :: verbosity(),
%%    verbosity() :: silence | info | log | debug | trace.
verbosity(config, V) ->
    snmpm_config:verbosity(V);
verbosity(server, V) ->
    snmpm_server:verbosity(V);
verbosity(net_if, V) ->
    snmpm_server:verbosity(net_if, V);
verbosity(note_store, V) ->
    snmpm_server:verbosity(note_store, V);
verbosity(all, V) ->
    snmpm_config:verbosity(V),
    snmpm_server:verbosity(V),
    snmpm_server:verbosity(net_if, V),
    snmpm_server:verbosity(note_store, V).


%% -- Restart -- 

%% Restart various component processes in the manager
%% Note that the effects of this is diffiult to
%% predict, so it should be use with *caution*!

%% -spec restart(Ref) -> void() when Ref :: net_if.
restart(net_if = What) ->
    snmpm_server:restart(What).


%% -- Users --

%% Register the 'user'. 
%% The manager entity responsible for a specific agent. 
%% Module is the callback module (snmpm_user behaviour) which 
%% will be called whenever something happens (detected 
%% agent, incoming reply or incoming trap/notification).
%% Note that this could have already been done as a 
%% consequence of the node config.
%% -spec register_user(Id, Module, Data) -> ok | {error, Reason} when Id :: term(),
%%    Module :: snmpm_user(),
%%    Data :: term(),
%%    DefaultAgentConfig :: [default_agent_config()],
%%    default_agent_config() :: {Item, Val},
%%    Item :: community | timeout | max_message_size | version | sec_model | sec_name | sec_level,
%%    Val :: term(),
%%    Reason :: term(),
%%    snmpm_user() :: Module implementing the snmpm_user behaviour.
register_user(Id, Module, Data) ->
    register_user(Id, Module, Data, []).

%% Default config for agents registered by this user
%% -spec register_user(Id, Module, Data, DefaultAgentConfig) -> ok | {error, Reason} when Id :: term(),
%%    Module :: snmpm_user(),
%%    Data :: term(),
%%    DefaultAgentConfig :: [default_agent_config()],
%%    default_agent_config() :: {Item, Val},
%%    Item :: community | timeout | max_message_size | version | sec_model | sec_name | sec_level,
%%    Val :: term(),
%%    Reason :: term(),
%%    snmpm_user() :: Module implementing the snmpm_user behaviour.
register_user(Id, Module, Data, DefaultAgentConfig) ->
    snmpm_server:register_user(Id, Module, Data, DefaultAgentConfig).

%% -spec register_user_monitor(Id, Module, Data) -> ok | {error, Reason} when Id :: term(),
%%    Module :: snmpm_user(),
%%    DefaultAgentConfig :: [default_agent_config()],
%%    default_agent_config() :: {Item, Val},
%%    Item :: community | timeout | max_message_size | version | sec_model | sec_name | sec_level,
%%    Val :: term(),
%%    Data :: term(),
%%    Reason :: term(),
%%    snmpm_user() :: Module implementing the snmpm_user behaviour.
register_user_monitor(Id, Module, Data) ->
    register_user_monitor(Id, Module, Data, []).

%% -spec register_user_monitor(Id, Module, Data, DefaultAgentConfig) -> ok | {error, Reason} when Id :: term(),
%%    Module :: snmpm_user(),
%%    DefaultAgentConfig :: [default_agent_config()],
%%    default_agent_config() :: {Item, Val},
%%    Item :: community | timeout | max_message_size | version | sec_model | sec_name | sec_level,
%%    Val :: term(),
%%    Data :: term(),
%%    Reason :: term(),
%%    snmpm_user() :: Module implementing the snmpm_user behaviour.
register_user_monitor(Id, Module, Data, DefaultAgentConfig) ->
    snmpm_server:register_user_monitor(Id, Module, Data, DefaultAgentConfig).

-spec unregister_user(Id) -> ok | {error, Reason :: term()}
                         when Id :: term().
unregister_user(Id) ->
    snmpm_server:unregister_user(Id).

-spec which_users() -> Users when Users :: [UserId],
   UserId :: term().
which_users() ->
    snmpm_config:which_users().


%% -- Agents --

%% Explicitly instruct the manager to handle this agent.
%% Called to instruct the manager that this agent 
%% shall be handled. These functions is used when
%% the user know's in advance which agents the
%% manager shall handle.
%% Note that there is an alternate way to do the same thing:
%% Add the agent to the manager config files.
%% 
%% UserId     -> Id of the user responsible for this agent: term()
%% TargetName -> Unique name for the agent: (string())
%% Config     -> Agent configuration: [config()]

do_register_agent(UserId, TargetName, Config) ->
    snmpm_config:register_agent(UserId, TargetName, Config).

%% -spec register_agent(UserId, TargetName, Config) -> ok | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    Config :: [agent_config()],
%%    agent_config() :: {Item, Val},
%%    Item :: engine_id | address | port | community | timeout | max_message_size | version | sec_model | sec_name | sec_level | tdomain,
%%    Val :: term(),
%%    Reason :: term().
register_agent(UserId, TargetName, Config) 
  when (is_list(TargetName) andalso 
	(length(TargetName) > 0) andalso 
	is_list(Config)) ->
    do_register_agent(UserId, TargetName, [{reg_type, target_name} | Config]);

%% Backward compatibility 
%% Note that the agent engine id is a mandatory config option,
%% so this function *will* fail!
register_agent(UserId, Addr, Port) when is_integer(Port) ->
    register_agent(UserId, Addr, Port, []);

%% Backward compatibility 
register_agent(UserId, Addr, Config) when is_list(Config) ->
    register_agent(UserId, Addr, ?DEFAULT_AGENT_PORT, Config).

%% Backward compatibility 
%% Note that the agent engine id is a mandatory config option,
%% so this function *will* fail!
register_agent(UserId, Addr) ->
    register_agent(UserId, Addr, ?DEFAULT_AGENT_PORT, []).

%% Backward compatibility 
register_agent(UserId, Domain, Addr, Config0) when is_atom(Domain) ->
    case lists:keymember(target_name, 1, Config0) of
	false ->
	    TargetName = mk_target_name(Domain, Addr, Config0),
	    Config =
		[{reg_type, addr_port},
		 {tdomain, Domain}, {taddress, Addr} | Config0],
	    do_register_agent(UserId, TargetName, ensure_engine_id(Config));
	true ->
	    {value, {_, TargetName}} = 
		lists:keysearch(target_name, 1, Config0),
	    Config1 = lists:keydelete(target_name, 1, Config0),
	    Config2 =
		[{reg_type, addr_port},
		 {tdomain, Domain}, {taddress, Addr} | Config1],
	    register_agent(UserId, TargetName, ensure_engine_id(Config2))
    end;
register_agent(UserId, Ip, Port, Config) when is_integer(Port) ->
    Domain = snmpm_config:default_transport_domain(),
    Addr =
	case snmp_conf:check_address(Domain, {Ip, Port}) of
	    ok ->
		{Ip, Port};
	    {ok, FixedAddr} ->
		FixedAddr
	end,
    register_agent(UserId, Domain, Addr, Config).

-spec unregister_agent(UserId, TargetName) ->
                          ok | {error, Reason :: term()}
                          when
                              UserId :: term(),
                              TargetName :: target_name().
unregister_agent(UserId, TargetName) when is_list(TargetName) ->
    snmpm_config:unregister_agent(UserId, TargetName);

%% Backward compatibility functions
unregister_agent(UserId, Addr) ->
    unregister_agent(UserId, Addr, ?DEFAULT_AGENT_PORT).

unregister_agent(UserId, DomainIp, AddressPort) ->
    case target_name(DomainIp, AddressPort) of
	{ok, TargetName} ->
	    unregister_agent(UserId, TargetName);
	Error ->
	    Error
    end.


-spec agent_info(TargetName, Item) ->
                    {ok, Val :: term()} | {error, Reason}
                    when
                        TargetName :: target_name(),
                        Item :: atom(),
                        Reason :: term().
agent_info(TargetName, Item) ->
    snmpm_config:agent_info(TargetName, Item).

%% -spec update_agent_info(UserId, TargetName, Info) -> ok | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    Info :: [{item(), item_value()}],
%%    Item :: item(),
%%    item() :: atom(),
%%    Val :: item_value(),
%%    item_value() :: term(),
%%    Reason :: term().
update_agent_info(UserId, TargetName, Info) when is_list(Info) ->
    snmpm_config:update_agent_info(UserId, TargetName, Info).

%% -spec update_agent_info(UserId, TargetName, Item, Val) -> ok | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    Info :: [{item(), item_value()}],
%%    Item :: item(),
%%    item() :: atom(),
%%    Val :: item_value(),
%%    item_value() :: term(),
%%    Reason :: term().
update_agent_info(UserId, TargetName, Item, Val) ->
    update_agent_info(UserId, TargetName, [{Item, Val}]).


%% -spec which_agents() -> Agents when UserId :: term(),
%%    Agents :: [TargetName],
%%    TargetName :: target_name().
which_agents() ->
    snmpm_config:which_agents().

-spec which_agents(UserId) -> Agents
                      when
                          UserId :: term(),
                          Agents :: [TargetName],
                          TargetName :: target_name().
which_agents(UserId) ->
    snmpm_config:which_agents(UserId).


%% -- USM users --

%% -spec register_usm_user(EngineID, UserName, Conf) -> ok | {error, Reason} when EngineID :: string(),
%%    UserName :: string(),
%%    Conf :: [usm_config()],
%%    usm_config() :: {Item, Val},
%%    Item :: sec_name | auth | auth_key | priv | priv_key,
%%    Val :: term(),
%%    Reason :: term().
register_usm_user(EngineID, UserName, Conf) 
  when is_list(EngineID) andalso is_list(UserName) andalso is_list(Conf) ->
    snmpm_config:register_usm_user(EngineID, UserName, Conf).

-spec unregister_usm_user(EngineID, UserName) -> ok | {error, Reason} when EngineID :: string(),
   UserName :: string(),
   Reason :: term().
unregister_usm_user(EngineID, UserName) 
  when is_list(EngineID) andalso is_list(UserName) ->
    snmpm_config:unregister_usm_user(EngineID, UserName).

%% -spec usm_user_info(EngineID, UserName, Item) -> {ok, Val} | {error, Reason} when EngineID :: string(),
%%    UsmName :: string(),
%%    Item :: sec_name | auth | auth_key | priv | priv_key,
%%    Reason :: term().
usm_user_info(EngineID, UserName, Item) 
  when is_list(EngineID) andalso is_list(UserName) andalso is_atom(Item) ->
    snmpm_config:usm_user_info(EngineID, UserName, Item).

%% -spec update_usm_user_info(EngineID, UserName, Item, Val) -> ok | {error, Reason} when EngineID :: string(),
%%    UsmName :: string(),
%%    Item :: sec_name | auth | auth_key | priv | priv_key,
%%    Val :: term(),
%%    Reason :: term().
update_usm_user_info(EngineID, UserName, Item, Val) 
  when is_list(EngineID) andalso is_list(UserName) andalso is_atom(Item) ->
    snmpm_config:update_usm_user_info(EngineID, UserName, Item, Val).

%% -spec which_usm_users() -> UsmUsers when UsmUsers :: [{EngineID,UserName}],
%%    EngineID :: string(),
%%    UsmName :: string().
which_usm_users() ->
    snmpm_config:which_usm_users().

-spec which_usm_users(EngineID :: term()) -> UsmUsers
                         when
                             UsmUsers :: [UserName],
                             UserName :: string().
which_usm_users(EngineID) when is_list(EngineID) ->
    snmpm_config:which_usm_users(EngineID).


%% -- Discovery --

%% Start a discovery process
%% discovery(UserId, BAddr) ->
%%     snmpm_server:discovery(UserId, BAddr).

%% discovery(UserId, BAddr, ExpireOrConfig) ->
%%     snmpm_server:discovery(UserId, BAddr, ExpireOrConfig).

%% discovery(UserId, BAddr, Config, Expire) ->
%%     snmpm_server:discovery(UserId, BAddr, Config, Expire).

%% discovery(UserId, BAddr, Port, Config, Expire) ->
%%     snmpm_server:discovery(UserId, BAddr, Port, Config, Expire).

%% discovery(UserId, BAddr, Port, Config, Expire, ExtraInfo) ->
%%     snmpm_server:discovery(UserId, BAddr, Port, Config, Expire, ExtraInfo).


%% -- Requests --

%% --- synchronous get-request ---
%% 

%% -spec sync_get2(UserId, TargetName, Oids) -> {ok, SnmpReply, Remaining} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    Oids :: [oid()],
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    SnmpReply :: snmp_reply(),
%%    Remaining :: integer(),
%%    Reason :: {send_failed, ReqId, ActualReason} | {invalid_sec_info, SecInfo, SnmpInfo} | term(),
%%    ReqId :: term(),
%%    ActualReason :: term(),
%%    SecInfo :: [sec_info()],
%%    sec_info() :: {sec_tag(), ExpectedValue, ReceivedValue},
%%    sec_tag() :: atom(),
%%    ExpectedValue :: term(),
%%    ReceivedValue :: term(),
%%    SnmpInfo :: term().
sync_get2(UserId, TargetName, Oids) ->
    sync_get2(UserId, TargetName, Oids, []).

%% -spec sync_get2(UserId, TargetName, Oids, SendOpts) -> {ok, SnmpReply, Remaining} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    Oids :: [oid()],
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    SnmpReply :: snmp_reply(),
%%    Remaining :: integer(),
%%    Reason :: {send_failed, ReqId, ActualReason} | {invalid_sec_info, SecInfo, SnmpInfo} | term(),
%%    ReqId :: term(),
%%    ActualReason :: term(),
%%    SecInfo :: [sec_info()],
%%    sec_info() :: {sec_tag(), ExpectedValue, ReceivedValue},
%%    sec_tag() :: atom(),
%%    ExpectedValue :: term(),
%%    ReceivedValue :: term(),
%%    SnmpInfo :: term().
sync_get2(UserId, TargetName, Oids, SendOpts) 
  when is_list(Oids) andalso is_list(SendOpts) ->
    snmpm_server:sync_get(UserId, TargetName, Oids, SendOpts).


%% --- asynchronous get-request ---
%% 
%% The reply will be delivered to the user
%% through a call to handle_pdu/5
%% 

%% -spec async_get2(UserId, TargetName, Oids) -> {ok, ReqId} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    Oids :: [oid()],
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    ReqId :: term(),
%%    Reason :: term().
async_get2(UserId, TargetName, Oids) ->
    async_get2(UserId, TargetName, Oids, []).

%% -spec async_get2(UserId, TargetName, Oids, SendOpts) -> {ok, ReqId} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    Oids :: [oid()],
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    ReqId :: term(),
%%    Reason :: term().
async_get2(UserId, TargetName, Oids, SendOpts) 
  when is_list(Oids) andalso is_list(SendOpts) ->
    snmpm_server:async_get(UserId, TargetName, Oids, SendOpts).


%% --- synchronous get_next-request ---
%% 

%% -spec sync_get_next2(UserId, TargetName, Oids) -> {ok, SnmpReply, Remaining} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    Oids :: [oid()],
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    SnmpReply :: snmp_reply(),
%%    Remaining :: integer(),
%%    Reason :: {send_failed, ReqId, ActualReason} | {invalid_sec_info, SecInfo, SnmpInfo} | term(),
%%    ReqId :: term(),
%%    ActualReason :: term(),
%%    SecInfo :: [sec_info()],
%%    sec_info() :: {sec_tag(), ExpectedValue, ReceivedValue},
%%    sec_tag() :: atom(),
%%    ExpectedValue :: term(),
%%    ReceivedValue :: term(),
%%    SnmpInfo :: term().
sync_get_next2(UserId, TargetName, Oids) ->
    sync_get_next2(UserId, TargetName, Oids, []).

%% -spec sync_get_next2(UserId, TargetName, Oids, SendOpts) -> {ok, SnmpReply, Remaining} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    Oids :: [oid()],
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    SnmpReply :: snmp_reply(),
%%    Remaining :: integer(),
%%    Reason :: {send_failed, ReqId, ActualReason} | {invalid_sec_info, SecInfo, SnmpInfo} | term(),
%%    ReqId :: term(),
%%    ActualReason :: term(),
%%    SecInfo :: [sec_info()],
%%    sec_info() :: {sec_tag(), ExpectedValue, ReceivedValue},
%%    sec_tag() :: atom(),
%%    ExpectedValue :: term(),
%%    ReceivedValue :: term(),
%%    SnmpInfo :: term().
sync_get_next2(UserId, TargetName, Oids, SendOpts) 
  when is_list(Oids) andalso is_list(SendOpts) ->
    snmpm_server:sync_get_next(UserId, TargetName, Oids, SendOpts).


%% --- asynchronous get_next-request ---
%% 

%% -spec async_get_next2(UserId, TargetName, Oids) -> {ok, ReqId} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    Oids :: [oid()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    ReqId :: integer(),
%%    Reason :: term().
async_get_next2(UserId, TargetName, Oids) ->
    async_get_next2(UserId, TargetName, Oids, []).

%% -spec async_get_next2(UserId, TargetName, Oids, SendOpts) -> {ok, ReqId} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    Oids :: [oid()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    ReqId :: integer(),
%%    Reason :: term().
async_get_next2(UserId, TargetName, Oids, SendOpts) 
  when is_list(Oids) andalso is_list(SendOpts) ->
    snmpm_server:async_get_next(UserId, TargetName, Oids, SendOpts).


%% --- synchronous set-request ---
%% 

%% -spec sync_set2(UserId, TargetName, VarsAndVals) -> {ok, SnmpReply, Remaining} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    VarsAndVals :: vars_and_vals(),
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    SnmpReply :: snmp_reply(),
%%    Remaining :: integer(),
%%    Reason :: {send_failed, ReqId, ActualReason} | {invalid_sec_info, SecInfo, SnmpInfo} | term(),
%%    ReqId :: term(),
%%    ActualReason :: term(),
%%    SecInfo :: [sec_info()],
%%    sec_info() :: {sec_tag(), ExpectedValue, ReceivedValue},
%%    sec_tag() :: atom(),
%%    ExpectedValue :: term(),
%%    ReceivedValue :: term(),
%%    SnmpInfo :: term().
sync_set2(UserId, TargetName, VarsAndVals) ->
    sync_set2(UserId, TargetName, VarsAndVals, []).

%% -spec sync_set2(UserId, TargetName, VarsAndVals, SendOpts) -> {ok, SnmpReply, Remaining} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    VarsAndVals :: vars_and_vals(),
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    SnmpReply :: snmp_reply(),
%%    Remaining :: integer(),
%%    Reason :: {send_failed, ReqId, ActualReason} | {invalid_sec_info, SecInfo, SnmpInfo} | term(),
%%    ReqId :: term(),
%%    ActualReason :: term(),
%%    SecInfo :: [sec_info()],
%%    sec_info() :: {sec_tag(), ExpectedValue, ReceivedValue},
%%    sec_tag() :: atom(),
%%    ExpectedValue :: term(),
%%    ReceivedValue :: term(),
%%    SnmpInfo :: term().
sync_set2(UserId, TargetName, VarsAndVals, SendOpts) 
  when is_list(VarsAndVals) andalso is_list(SendOpts) ->
    snmpm_server:sync_set(UserId, TargetName, VarsAndVals, SendOpts).


%% --- asynchronous set-request ---
%% 

%% -spec async_set2(UserId, TargetName, VarsAndVals) -> {ok, ReqId} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    VarsAndVals :: vars_and_vals(),
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    ReqId :: term(),
%%    Reason :: term().
async_set2(UserId, TargetName, VarsAndVals) ->
    async_set2(UserId, TargetName, VarsAndVals, []).

%% -spec async_set2(UserId, TargetName, VarsAndVals, SendOpts) -> {ok, ReqId} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    VarsAndVals :: vars_and_vals(),
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    ReqId :: term(),
%%    Reason :: term().
async_set2(UserId, TargetName, VarsAndVals, SendOpts) 
  when is_list(VarsAndVals) andalso is_list(SendOpts) ->
    snmpm_server:async_set(UserId, TargetName, VarsAndVals, SendOpts).


%% --- synchronous get-bulk ---
%% 

%% -spec sync_get_bulk2(UserId, TragetName, NonRep, MaxRep, Oids) -> {ok, SnmpReply, Remaining} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    NonRep :: integer(),
%%    MaxRep :: integer(),
%%    Oids :: [oid()],
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    SnmpReply :: snmp_reply(),
%%    Remaining :: integer(),
%%    Reason :: {send_failed, ReqId, ActualReason} | {invalid_sec_info, SecInfo, SnmpInfo} | term(),
%%    ReqId :: term(),
%%    ActualReason :: term(),
%%    SecInfo :: [sec_info()],
%%    sec_info() :: {sec_tag(), ExpectedValue, ReceivedValue},
%%    sec_tag() :: atom(),
%%    ExpectedValue :: term(),
%%    ReceivedValue :: term(),
%%    SnmpInfo :: term().
sync_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids) ->
    sync_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, []).

%% -spec sync_get_bulk2(UserId, TragetName, NonRep, MaxRep, Oids, SendOpts) -> {ok, SnmpReply, Remaining} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    NonRep :: integer(),
%%    MaxRep :: integer(),
%%    Oids :: [oid()],
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    SnmpReply :: snmp_reply(),
%%    Remaining :: integer(),
%%    Reason :: {send_failed, ReqId, ActualReason} | {invalid_sec_info, SecInfo, SnmpInfo} | term(),
%%    ReqId :: term(),
%%    ActualReason :: term(),
%%    SecInfo :: [sec_info()],
%%    sec_info() :: {sec_tag(), ExpectedValue, ReceivedValue},
%%    sec_tag() :: atom(),
%%    ExpectedValue :: term(),
%%    ReceivedValue :: term(),
%%    SnmpInfo :: term().
sync_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, SendOpts) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) andalso 
       is_list(SendOpts) ->
    snmpm_server:sync_get_bulk(UserId, TargetName, 
                               NonRep, MaxRep, Oids, SendOpts).


%% --- asynchronous get-bulk ---
%% 

%% -spec async_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids) -> {ok, ReqId} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    NonRep :: integer(),
%%    MaxRep :: integer(),
%%    Oids :: [oid()],
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    ReqId :: integer(),
%%    Reason :: term().
async_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids) ->
    async_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, []).

%% -spec async_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, SendOpts) -> {ok, ReqId} | {error, Reason} when UserId :: term(),
%%    TargetName :: target_name(),
%%    NonRep :: integer(),
%%    MaxRep :: integer(),
%%    Oids :: [oid()],
%%    SendOpts :: send_opts(),
%%    send_opts() :: [send_opt()],
%%    send_opt() :: {context, string()} | {timeout, pos_integer()} | {extra, term()} | {community, community()} | {sec_model, sec_model()} | {sec_name, string()} | {sec_level, sec_level()} | {max_message_size, pos_integer()},
%%    ReqId :: integer(),
%%    Reason :: term().
async_get_bulk2(UserId, TargetName, NonRep, MaxRep, Oids, SendOpts) 
  when is_integer(NonRep) andalso 
       is_integer(MaxRep) andalso 
       is_list(Oids) andalso 
       is_list(SendOpts) ->
    snmpm_server:async_get_bulk(UserId, TargetName, 
                                NonRep, MaxRep, Oids, SendOpts).



-spec cancel_async_request(UserId, ReqId) -> ok | {error, Reason} when UserId :: term(),
   ReqId :: term(),
   Reason :: term().
cancel_async_request(UserId, ReqId) ->
    snmpm_server:cancel_async_request(UserId, ReqId).


%%%-----------------------------------------------------------------
%%% Audit Trail Log functions (for backward compatibility)
%%%-----------------------------------------------------------------

-spec log_to_txt(LogDir :: snmp:dir()) ->
    snmp:void().

log_to_txt(LogDir) ->
    log_to_txt(LogDir, []). 

-spec log_to_txt(LogDir :: snmp:dir(), 
		 Block  :: boolean()) ->
    snmp:void();
                (LogDir :: snmp:dir(), 
		 Mibs   :: [snmp:mib_name()]) ->
    snmp:void().

log_to_txt(LogDir, Block) 
  when ((Block =:= true) orelse (Block =:= false)) ->
    Mibs    = [], 
    OutFile = "snmpm_log.txt",       
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs) ->
    Block   = ?ATL_BLOCK_DEFAULT, 
    OutFile = "snmpm_log.txt",       
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).

-spec log_to_txt(LogDir :: snmp:dir(), 
		 Mibs   :: [snmp:mib_name()], 
		 Block  :: boolean()) ->
    snmp:void();
                (LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename()) ->
    snmp:void().

log_to_txt(LogDir, Mibs, Block)  
  when ((Block =:= true) orelse (Block =:= false)) ->
    OutFile = "snmpm_log.txt",       
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile) ->
    Block   = ?ATL_BLOCK_DEFAULT, 
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).

-spec log_to_txt(LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 Block   :: boolean()) ->
    snmp:void();
                (LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string()) ->
    snmp:void().

log_to_txt(LogDir, Mibs, OutFile, Block)  
  when ((Block =:= true) orelse (Block =:= false)) ->
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block); 
log_to_txt(LogDir, Mibs, OutFile, LogName) ->
    Block   = ?ATL_BLOCK_DEFAULT, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).

-spec log_to_txt(LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 Block   :: boolean()) ->
    snmp:void();
                (LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string()) ->
    snmp:void().

log_to_txt(LogDir, Mibs, OutFile, LogName, Block)  
  when ((Block =:= true) orelse (Block =:= false)) -> 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block).

-spec log_to_txt(LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string(), 
		 Block   :: boolean()) ->
    snmp:void();
                (LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string(), 
		 Start   :: snmp_log:log_time()) ->
    snmp:void().

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block)  
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block);
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start).

-spec log_to_txt(LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string(), 
		 Block   :: boolean(), 
		 Start   :: snmp_log:log_time()) ->
    snmp:void();
                (LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string(), 
		 Start   :: snmp_log:log_time(), 
		 Stop    :: snmp_log:log_time()) ->
    snmp:void().

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start)  
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start);
log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Start, Stop) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start, Stop).

-spec log_to_txt(LogDir  :: snmp:dir(), 
		 Mibs    :: [snmp:mib_name()], 
		 OutFile :: file:filename(), 
		 LogName :: string(), 
		 LogFile :: string(), 
		 Block   :: boolean(), 
		 Start   :: snmp_log:log_time(), 
		 Stop    :: snmp_log:log_time()) ->
    snmp:void().

log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start, Stop) -> 
    snmp:log_to_txt(LogDir, Mibs, OutFile, LogName, LogFile, Block, Start, Stop).


%% -spec log_to_io(LogDir) -> ok | {ok, Cnt} | {error, Reason} when LogDir :: string(),
%%    Mibs :: [MibName],
%%    MibName :: string(),
%%    Block :: boolean(),
%%    LogName :: string(),
%%    LogFile :: string(),
%%    Start :: null | calendar:datetime() | {local_time, calendar:datetime()} | {universal_time, calendar:datetime()},
%%    Stop :: null | calendar:datetime() | {local_time, calendar:datetime()} | {universal_time, calendar:datetime()},
%%    Cnt :: {NumOK, NumERR},
%%    NumOK :: non_neg_integer(),
%%    NumERR :: pos_integer(),
%%    Reason :: disk_log_open_error() | file_open_error() | term(),
%%    disk_log_open_error() :: {LogName, term()},
%%    file_open_error() :: {OutFile, term()}.
log_to_io(LogDir) ->
    log_to_io(LogDir, []).

%% -spec log_to_io(LogDir, Mibs) -> ok | {error, Reason}log_to_io(LogDir, Block | Mibs) -> ok | {ok, Cnt} | {error, Reason}
log_to_io(LogDir, Block) 
  when ((Block =:= true) orelse (Block =:= false)) ->
    Mibs    = [], 
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs) ->
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile).

%% -spec log_to_io(LogDir, Mibs, Block | LogName) ->  ok | {ok, Cnt} | {error, Reason} when LogDir :: string(),
%%    Mibs :: [MibName],
%%    MibName :: string(),
%%    Block :: boolean(),
%%    LogName :: string(),
%%    LogFile :: string(),
%%    Start :: null | calendar:datetime() | {local_time, calendar:datetime()} | {universal_time, calendar:datetime()},
%%    Stop :: null | calendar:datetime() | {local_time, calendar:datetime()} | {universal_time, calendar:datetime()},
%%    Cnt :: {NumOK, NumERR},
%%    NumOK :: non_neg_integer(),
%%    NumERR :: pos_integer(),
%%    Reason :: disk_log_open_error() | file_open_error() | term(),
%%    disk_log_open_error() :: {LogName, term()},
%%    file_open_error() :: {OutFile, term()}.
log_to_io(LogDir, Mibs, Block) 
  when ((Block =:= true) orelse (Block =:= false)) ->
    LogName = ?audit_trail_log_name, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName) ->
    Block   = ?ATL_BLOCK_DEFAULT, 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block).

%% -spec log_to_io(LogDir, Mibs, LogName, Block | LogFile) ->  ok | {ok, Cnt} | {error, Reason} when LogDir :: string(),
%%    Mibs :: [MibName],
%%    MibName :: string(),
%%    Block :: boolean(),
%%    LogName :: string(),
%%    LogFile :: string(),
%%    Start :: null | calendar:datetime() | {local_time, calendar:datetime()} | {universal_time, calendar:datetime()},
%%    Stop :: null | calendar:datetime() | {local_time, calendar:datetime()} | {universal_time, calendar:datetime()},
%%    Cnt :: {NumOK, NumERR},
%%    NumOK :: non_neg_integer(),
%%    NumERR :: pos_integer(),
%%    Reason :: disk_log_open_error() | file_open_error() | term(),
%%    disk_log_open_error() :: {LogName, term()},
%%    file_open_error() :: {OutFile, term()}.
log_to_io(LogDir, Mibs, LogName, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    LogFile = ?audit_trail_log_file, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName, LogFile) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block).

%% -spec log_to_io(LogDir, Mibs, LogName, LogFile, Block | Start) ->  ok | {ok, Cnt} | {error, Reason} when LogDir :: string(),
%%    Mibs :: [MibName],
%%    MibName :: string(),
%%    Block :: boolean(),
%%    LogName :: string(),
%%    LogFile :: string(),
%%    Start :: null | calendar:datetime() | {local_time, calendar:datetime()} | {universal_time, calendar:datetime()},
%%    Stop :: null | calendar:datetime() | {local_time, calendar:datetime()} | {universal_time, calendar:datetime()},
%%    Cnt :: {NumOK, NumERR},
%%    NumOK :: non_neg_integer(),
%%    NumERR :: pos_integer(),
%%    Reason :: disk_log_open_error() | file_open_error() | term(),
%%    disk_log_open_error() :: {LogName, term()},
%%    file_open_error() :: {OutFile, term()}.
log_to_io(LogDir, Mibs, LogName, LogFile, Block) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block);
log_to_io(LogDir, Mibs, LogName, LogFile, Start) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start).

%% -spec log_to_io(LogDir, Mibs, LogName, LogFile, Start, Stop)  -> ok | {ok, Cnt} | {error, Reason}log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start)  -> ok | {ok, Cnt} | {error, Reason}
log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start) 
  when ((Block =:= true) orelse (Block =:= false)) -> 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start); 
log_to_io(LogDir, Mibs, LogName, LogFile, Start, Stop) -> 
    Block = ?ATL_BLOCK_DEFAULT, 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop).

%% -spec log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop)  -> ok | {ok, Cnt} | {error, Reason} when LogDir :: string(),
%%    Mibs :: [MibName],
%%    MibName :: string(),
%%    Block :: boolean(),
%%    LogName :: string(),
%%    LogFile :: string(),
%%    Start :: null | calendar:datetime() | {local_time, calendar:datetime()} | {universal_time, calendar:datetime()},
%%    Stop :: null | calendar:datetime() | {local_time, calendar:datetime()} | {universal_time, calendar:datetime()},
%%    Cnt :: {NumOK, NumERR},
%%    NumOK :: non_neg_integer(),
%%    NumERR :: pos_integer(),
%%    Reason :: disk_log_open_error() | file_open_error() | term(),
%%    disk_log_open_error() :: {LogName, term()},
%%    file_open_error() :: {OutFile, term()}.
log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop) -> 
    snmp:log_to_io(LogDir, Mibs, LogName, LogFile, Block, Start, Stop).
    

-spec change_log_size(NewSize) -> ok | {error, Reason} when NewSize :: {MaxBytes, MaxFiles},
   MaxBytes :: integer(),
   MaxFiles :: integer(),
   Reason :: term().
change_log_size(NewSize) ->
    LogName = ?audit_trail_log_name, 
    snmp:change_log_size(LogName, NewSize).


get_log_type() ->
    snmpm_server:get_log_type().

%% NewType -> atl_type()
%% -spec set_log_type(NewType) -> {ok, OldType} | {error, Reason}
%%                       when
%%                           NewType :: atl_type(),
%%                           OldType :: atl_type(),
%%                           Reason :: term().
set_log_type(NewType) ->
    snmpm_server:set_log_type(NewType).


reconfigure() ->
    snmpm_server:reconfigure().


%%%-----------------------------------------------------------------

system_start_time() ->
    {ok, Time} = snmpm_config:system_start_time(),
    Time.

sys_up_time() ->
    % time in 0.01 seconds.
    StartTime = system_start_time(),
    (snmp_misc:now(cs) - StartTime) rem (2 bsl 31).


%%%-----------------------------------------------------------------
%%% This is just some simple utility functions to create a pretty-
%%% printable string of the error reason received from either:
%%% 
%%%    * If any of the sync/async get/get-next/set/get-bulk
%%%      returns {error, Reason} 
%%%    * The Reason parameter in the handle_error user callback 
%%%      function
%%% 
%%%-----------------------------------------------------------------

%% -spec format_reason(Reason) -> string() when Reason :: term(),
%%    Prefix :: integer() | string().
format_reason(Reason) ->
    format_reason("", Reason).

-spec format_reason(Prefix, Reason) -> string() when Reason :: term(),
   Prefix :: integer() | string().
format_reason(Prefix, Reason) when is_integer(Prefix) andalso (Prefix >= 0) ->
    format_reason(lists:duplicate(Prefix, $ ), Reason);
format_reason(Prefix, Reason) when is_list(Prefix) ->
    case (catch do_format_reason(Prefix, Reason)) of
	FL when is_list(FL) ->
	    FL;
	_ ->
	    %% Crap, try it without any fancy formatting
	    case (catch io_lib:format("~sInternal manager error: ~n"
				      "~s   ~p~n", 
				      [Prefix, Prefix, Reason])) of
		L1 when is_list(L1) ->
		    lists:flatten(L1);
		_ ->
		    %% Really crap, try it without the prefix
		    case (catch io_lib:format("Internal manager error: ~n"
					      "   ~p~n", 
					      [Reason])) of
			L2 when is_list(L2) ->
			    lists:flatten(L2);
			_ ->
			    %% Ok, I give up
			    "Illegal input. Unable to format error reason"
		    end
	    end
    end.
		    

do_format_reason(Prefix, {failed_generating_response, {RePdu, Reason}}) ->
    FmtPdu = format_pdu(Prefix ++ "   ", RePdu),
    lists:flatten(io_lib:format("~sFailed generating response: ~n"
				"~s"
				"~s   ~p~n", 
				[Prefix, FmtPdu, Prefix, Reason]));
do_format_reason(Prefix, {failed_processing_message, Reason})  ->
    lists:flatten(io_lib:format("~sFailed processing message: ~n"
				"~s   ~p~n", 
				[Prefix, Prefix, Reason]));
do_format_reason(Prefix, {unexpected_pdu, SnmpInfo})  ->
    FmtSnmpInfo = format_snmp_info(Prefix ++ "   ", SnmpInfo),
    lists:flatten(io_lib:format("~sUnexpected PDU: ~n~s", 
				[Prefix, FmtSnmpInfo]));
do_format_reason(Prefix, {send_failed, ReqId, Reason})  ->
    lists:flatten(io_lib:format("~sSend failed: ~n"
				"~s   Request id: ~w~n"
				"~s   Reason:     ~p~n", 
				[Prefix, Prefix, ReqId, Prefix, Reason]));
do_format_reason(Prefix, {invalid_sec_info, SecInfo, SnmpInfo})  ->
    FmtSecInfo  = format_sec_info(Prefix ++ "   ", SecInfo),
    FmtSnmpInfo = format_snmp_info(Prefix ++ "   ", SnmpInfo),
    lists:flatten(io_lib:format("~sInvalid security info: ~n"
				"~s"
				"~s", 
				[Prefix, FmtSecInfo, FmtSnmpInfo]));
do_format_reason(Prefix, Reason)  ->
    lists:flatten(io_lib:format("~sInternal manager error: ~n"
				"~s   ~p~n", [Prefix, Prefix, Reason])).

format_pdu(Prefix, #pdu{type         = Type,
			request_id   = ReqId,
			error_status = ES,
			error_index  = EI,
			varbinds     = VBs}) ->
    FmtPdyType   = format_pdu_type(Type),
    FmtErrStatus = format_error_status(ES),
    FmtErrIdx    = format_error_index(EI),
    FmtVBs       = format_varbinds(Prefix ++ "   ", VBs),
    lists:flatten(io_lib:format("~s~s: ~n"
				"~s   Request-id:   ~w~n"
				"~s   Error-status: ~s~n"
				"~s   Error-index:  ~s~n"
				"~s",
				[Prefix, FmtPdyType,
				 Prefix, ReqId, 
				 Prefix, FmtErrStatus, 
				 Prefix, FmtErrIdx, 
				 FmtVBs]));
format_pdu(Prefix, #trappdu{enterprise    = E,
			    agent_addr    = AA,
			    generic_trap  = GT,
			    specific_trap = ST,
			    time_stamp    = TS,
			    varbinds      = VBs}) ->
    FmtVBs = format_varbinds(Prefix ++ "   ", VBs),
    lists:flatten(io_lib:format("~sTrap PDU: ~n"
				"~s   Enterprise:    ~p~n"
				"~s   Agent address: ~p~n"
				"~s   Generic trap:  ~p~n"
				"~s   Specific trap: ~p~n"
				"~s   Time stamp:    ~p~n"
				"~s",
				[Prefix, 
				 Prefix, E,
				 Prefix, AA, 
				 Prefix, GT, 
				 Prefix, ST, 
				 Prefix, TS, 
				 FmtVBs]));
format_pdu(Prefix, PDU) ->
    lists:flatten(io_lib:format("~s~p~n", [Prefix, PDU])).

format_pdu_type('get-request') ->
    "GetRequest-PDU";
format_pdu_type('get-next-request') ->
    "GetNextRequest-PDU";
format_pdu_type('get-response') ->
    "Response-PDU";
format_pdu_type('set-request') ->
    "SetRequest-PDU";
format_pdu_type('get-bulk-request') ->
    "GetBulkRequest-PDU";
format_pdu_type('inform-request') ->
    "InformRequest-PDU";
format_pdu_type('snmpv2-trap') ->
    "SNMPv2-Trap-PDU";
format_pdu_type(report) ->
    "Report-PDU";
format_pdu_type(T) ->
    lists:flatten(io_lib:format("~p", [T])).
    
format_snmp_info(Prefix, {ES, EI, VBs}) ->
    lists:flatten(io_lib:format("~sSNMP info: ~n"
				"~s   Error-status: ~s~n"
				"~s   Error-index:  ~s~n"
				"~s",
				[Prefix, 
				 Prefix, format_error_status(ES),
				 Prefix, format_error_index(EI),
				 format_varbinds(Prefix ++ "   ", VBs)]));
format_snmp_info(Prefix, JunkSnmpInfo) ->
    lists:flatten(io_lib:format("~sJunk SNMP info: ~n"
				"~s   ~p~n",
				[Prefix, Prefix, JunkSnmpInfo])).

format_error_status(ES) ->
    lists:flatten(io_lib:format("~p", [ES])).

format_error_index(EI) ->
    lists:flatten(io_lib:format("~p", [EI])).

format_sec_info(Prefix, Info) ->
    FmtSecInfo = do_format_sec_info(Prefix ++ "   ", Info),
    lists:flatten(io_lib:format("~sSecurity info: ~n~s", 
				[Prefix, FmtSecInfo])).

do_format_sec_info(_Prefix, []) ->
    "";
do_format_sec_info(Prefix, [{Tag, ExpVal, Val}|T]) ->
    format_sec_info(Prefix, Tag, ExpVal, Val) ++
	do_format_sec_info(Prefix, T).


format_sec_info(_Prefix, _Tag, Val, Val) ->
    "";
format_sec_info(Prefix, Tag, ExpVal, Val) ->
    lists:flatten(io_lib:format("~s~s:~n"
				"~s   Expected value: ~p~n"
				"~s   Actual value:   ~p~n",
				[Prefix, format_sec_info_tag(Tag),
				 Prefix, ExpVal,
				 Prefix, Val])).

format_sec_info_tag(sec_engine_id) ->
    "Sec engine id";
format_sec_info_tag(msg_sec_model) ->
    "Msg sec model";
format_sec_info_tag(sec_name) ->
    "Sec name";
format_sec_info_tag(sec_level) ->
    "Sec level";
format_sec_info_tag(ctx_engine_id) ->
    "Context engine id";
format_sec_info_tag(ctx_name) ->
    "Context name";
format_sec_info_tag(request_id) ->
    "Request id";
format_sec_info_tag(T) ->
    lists:flatten(io_lib:format("~p", [T])).

format_varbinds(Prefix, []) ->
    lists:flatten(io_lib:format("~sVarbinds:    []~n", [Prefix])); 
format_varbinds(Prefix, VBs) when is_list(VBs) ->
    lists:flatten(io_lib:format("~sVarbinds: ~n~s", 
				[Prefix, format_vbs(Prefix ++ "   ", VBs)]));
format_varbinds(Prefix, VBs) ->
    lists:flatten(io_lib:format("~sInvalid varbinds: ~n"
				"~s   ~p~n", 
				[Prefix, Prefix, VBs])).

format_vbs(_Prefix, []) ->
    "";
format_vbs(Prefix, [VB|VBs]) ->
    format_vb(Prefix, VB) ++ format_vbs(Prefix, VBs).
    
format_vb(Prefix, #varbind{oid          = Oid0,
			   variabletype = Type,
			   value        = Val,
			   org_index    = Idx}) ->
    Oid = 
	case snmpm:oid_to_name(Oid0) of
	    {ok, O} ->
		O;
	    _ ->
		Oid0
	end,
    FmtVT  = format_vb_variabletype(Prefix ++ "   ", Type),
    FmtVal = format_vb_value(Prefix ++ "   ", Type, Val),
    lists:flatten(io_lib:format("~s~w:~n"
				"~s"
				"~s"
				"~s   Org-index:     ~p~n", 
				[Prefix, Oid, 
				 FmtVT, 
				 FmtVal, 
				 Prefix, Idx]));
format_vb(Prefix, JunkVB) ->
    lists:flatten(io_lib:format("~sJunk varbind:~n"
				"~s   ~p~n", [Prefix, Prefix, JunkVB])).

format_vb_variabletype(Prefix, Type) when is_atom(Type) ->
    lists:flatten(io_lib:format("~sVariable-type: ~s~n", 
				[Prefix, atom_to_list(Type)]));
format_vb_variabletype(Prefix, Type) ->
    lists:flatten(io_lib:format("~sVariable-type: ~p~n", [Prefix, Type])).

format_vb_value(Prefix, _Type, Val) ->
    lists:flatten(io_lib:format("~sValue:         ~p~n", [Prefix, Val])).


%% ---------------------------------------------------------------------------
%% 
%% --- Internal utility functions ---
%% 

target_name(Ip) ->
    target_name(Ip, ?DEFAULT_AGENT_PORT).

target_name(DomainIp, AddressPort) ->
    snmpm_config:agent_info(DomainIp, AddressPort, target_name).

mk_target_name(Addr, Port, Config) ->
    R = snmpm_config:mk_target_name(Addr, Port, Config),
    p(?MODULE_STRING":mk_target_name(~p, ~p, ~p) -> ~p.~n",
      [Addr, Port, Config, R]),
    R.

ensure_engine_id(Config) ->
    case lists:keymember(engine_id, 1, Config) of
	true ->
	    Config;
	false ->
	    DefaultEngineId = "agentEngine-default", 
	    [{engine_id, DefaultEngineId} | Config]
    end.



%% p(F) ->
%%     p(F, []).

p(F, A) ->
    io:format("~w:" ++ F ++ "~n", [?MODULE | A]).
