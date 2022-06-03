%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2021. All Rights Reserved.
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
-module(user_drv).

%% Basic interface to a port.

-export([start/0]).

%% gen_statem state callbacks
-export([init/3,server/3,switch_loop/3]).

%% gen_statem callbacks
-export([init/1, callback_mode/0]).

-include_lib("kernel/include/logger.hrl").

-define(OP_PUTC,0).
-define(OP_MOVE,1).
-define(OP_INSC,2).
-define(OP_DELC,3).
-define(OP_BEEP,4).
-define(OP_PUTC_SYNC,5).
% Control op
-define(ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER, 16#018b0900).
-define(CTRL_OP_GET_WINSIZE, (100 + ?ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER)).
-define(CTRL_OP_GET_UNICODE_STATE, (101 + ?ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER)).
-define(CTRL_OP_SET_UNICODE_STATE, (102 + ?ERTS_TTYSL_DRV_CONTROL_MAGIC_NUMBER)).

-record(state, { port, user, current_group, groups, queue }).

%% start()

-spec start() -> pid().

start() ->					%Default line editing shell
    case gen_statem:start({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} -> Pid;
        {error, _Reason} ->
            user:start()
    end.

callback_mode() -> state_functions.

init([]) ->
    process_flag(trap_exit, true),
    case catch open_port({spawn,"tty_sl -c -e"}, [eof]) of
        {'EXIT', _Reason} ->
            {stop, normal};
        Port ->
            {ok, init, #state{ user = start_user() },
             {next_event, internal, Port}}
    end.

init(internal, Port, State = #state{ user = User }) ->

    %% Cleanup ancestors so that observer looks nice
    put('$ancestors',[User|get('$ancestors')]),

    %% Initialize standard_error
    Encoding =
        case get_unicode_state(Port) of
            true -> unicode;
            false -> latin1
        end,
    ok = io:setopts(standard_error, [{encoding, Encoding}, {onlcr,true}]),

    %% Initialize the starting shell
    {Curr,Shell} =
	case init:get_argument(remsh) of
	    {ok,[[Node]]} ->
                ANode =
                    if
                        node() =:= nonode@nohost ->
                            %% We try to connect to the node if the current node is not
                            %% a distributed node yet. If this succeeds it means that we
                            %% are running using "-sname undefined".
                            _ = net_kernel:start([undefined, shortnames]),
                            NodeName = append_hostname(Node, net_kernel:nodename()),
                            case net_kernel:connect_node(NodeName) of
                                true ->
                                    NodeName;
                                _Else ->
                                    ?LOG_ERROR("Could not connect to ~p",[Node])
                            end;
                        true ->
                            append_hostname(Node, node())
                    end,

		RShell = {ANode,shell,start,[]},
		{group:start(self(), RShell, rem_sh_opts(ANode)), RShell};
	    E when E =:= error ; E =:= {ok,[[]]} ->
		LShell = {shell,start,[init]},
		{group:start(self(), LShell), LShell}
	end,

    Gr1 = gr_add_cur(gr_new(), User, {}),
    Gr = gr_add_cur(Gr1, Curr, Shell),

    NewState = State#state{ port = Port, current_group = Curr, user = User,
                            groups = Gr, queue = {false, queue:new()}
                          },

    %% Print some information.
    Slogan = case application:get_env(stdlib, shell_slogan,
                                      fun() -> erlang:system_info(system_version) end) of
                 Fun when is_function(Fun, 0) ->
                     Fun();
                 SloganEnv ->
                     SloganEnv
             end,

    {next_state, server, NewState,
     {next_event, info,
      {Curr, {put_chars, unicode, lists:flatten(io_lib:format("~ts\n", [Slogan]))}}}}.

append_hostname(Node, LocalNode) ->
    case string:find(Node,"@") of
        nomatch ->
            list_to_atom(Node ++ string:find(atom_to_list(LocalNode),"@"));
        _ ->
            list_to_atom(Node)
    end.

rem_sh_opts(Node) ->
    [{expand_fun,fun(B)-> rpc:call(Node,edlin_expand,expand,[B]) end}].

%% start_user()
%%  Start a group leader process and register it as 'user', unless,
%%  of course, a 'user' already exists.

start_user() ->
    case whereis(user) of
	undefined ->
	    User = group:start(self(), {}),
	    register(user, User),
	    User;
	User ->
	    User
    end.

server(info, {Port,{data,Bs}}, State = #state{ port = Port }) ->
    UTF8Binary = list_to_binary(Bs),
    case contains_ctrl_g_or_ctrl_c(UTF8Binary) of
        ctrl_g -> {next_state, switch_loop, State, {next_event, internal, init}};
        ctrl_c ->
            case gr_get_info(State#state.groups, State#state.current_group) of
                undefined -> ok;
                _ -> exit(State#state.current_group, interrupt)
            end,
            keep_state_and_data;
        none ->
            State#state.current_group !
                {self(), {data, unicode:characters_to_list(UTF8Binary, utf8)}},
            keep_state_and_data
    end;
server(info, {Port,eof}, State = #state{ port = Port }) ->
    State#state.current_group ! {self(),eof},
    keep_state_and_data;
server(info, {Requester,tty_geometry}, #state{ port = Port }) ->
    Requester ! {self(),tty_geometry,get_tty_geometry(Port)},
    keep_state_and_data;
server(info, {Requester,get_unicode_state}, #state{ port = Port }) ->
    Requester ! {self(),get_unicode_state,get_unicode_state(Port)},
    keep_state_and_data;
server(info, {Requester,set_unicode_state,Bool}, #state{ port = Port }) ->
    Requester ! {self(),set_unicode_state,set_unicode_state(Port, Bool)},
    keep_state_and_data;
server(info, Req, State = #state{ user = User, current_group = Curr })
  when element(1,Req) =:= User orelse element(1,Req) =:= Curr,
       tuple_size(Req) =:= 2 orelse tuple_size(Req) =:= 3 ->
    %% We match {User|Curr,_}|{User|Curr,_,_}
    {keep_state, State#state{ queue = handle_req(Req, State#state.port, State#state.queue) }};
server(info, {Port, ok}, State = #state{ port = Port, queue = {{Origin, Reply}, IOQ} }) ->
    %% We get this ok from the port, in io_request we store
    %% info about where to send reply at head of queue
    Origin ! {reply,Reply},
    {keep_state, State#state{ queue = handle_req(next, Port, {false, IOQ}) }};
server(info,{'EXIT',Port, _Reason}, #state{ port = Port }) ->
    keep_state_and_data;
server(info,{'EXIT',User, shutdown}, #state{ user = User }) ->
    keep_state_and_data;
server(info,{'EXIT',User, _Reason}, State = #state{ user = User }) ->
    NewUser = start_user(),
    {keep_state, State#state{ user = NewUser,
                              groups = gr_set_num(State#state.groups, 1, NewUser, {})}};
server(info,{'EXIT', Group, Reason}, State) -> % shell and group leader exit
    case gr_cur_pid(State#state.groups) of
        Group when Reason =/= die ,
                   Reason =/= terminated  ->	% current shell exited
            if Reason =/= normal ->
                    io_requests([{put_chars,unicode,"*** ERROR: "}], State#state.port);
               true -> % exit not caused by error
                    io_requests([{put_chars,unicode,"*** "}], State#state.port)
            end,
            io_requests([{put_chars,unicode,"Shell process terminated! "}], State#state.port),
            Gr1 = gr_del_pid(State#state.groups, Group),
            case gr_get_info(State#state.groups, Group) of
                {Ix,{shell,start,Params}} -> % 3-tuple == local shell
                    io_requests([{put_chars,unicode,"***\n"}], State#state.port),
                    %% restart group leader and shell, same index
                    NewGroup = group:start(self(), {shell,start,Params}),
                    {ok,Gr2} = gr_set_cur(gr_set_num(Gr1, Ix, NewGroup,
                                                     {shell,start,Params}), Ix),
                    {keep_state, State#state{ current_group = NewGroup, groups = Gr2 }};
                _ -> % remote shell
                    io_requests([{put_chars,unicode,"(^G to start new job) ***\n"}],
                                State#state.port),
                    {keep_state, State#state{ groups = Gr1 }}
            end;
        _ ->  % not current, just remove it
            {keep_state, State#state{ groups = gr_del_pid(State#state.groups, Group) }}
    end;
server(info,{Requester, {put_chars_sync, _, _, Reply}}, _State) ->
    %% This is a sync request from an unknown or inactive group.
    %% We need to ack the Req otherwise originating process will hang forever.
    %% We discard the output to non visible shells
    Requester ! {reply, Reply},
    keep_state_and_data;
server(_, _, _) ->
    %% Ignore unknown messages.
    keep_state_and_data.

handle_req(next,Port,{false,IOQ}=IOQueue) ->
    case queue:out(IOQ) of
        {empty,_} ->
	    IOQueue;
        {{value,{Origin,Req}},ExecQ} ->
            case io_request(Req,Port) of
                ok ->
		    handle_req(next,Port,{false,ExecQ});
                Reply ->
		    {{Origin,Reply},ExecQ}
            end
    end;
handle_req(Msg,Port,{false,IOQ}=IOQueue) ->
    empty = queue:peek(IOQ),
    {Origin,Req} = Msg,
    case io_request(Req, Port) of
	ok ->
	    IOQueue;
	Reply ->
	    {{Origin,Reply}, IOQ}
    end;
handle_req(Msg,_Port,{Resp, IOQ}) ->
    %% All requests are queued when we have outstanding sync put_chars
    {Resp, queue:in(Msg,IOQ)}.

contains_ctrl_g_or_ctrl_c(<<$\^G,_/binary>>) ->
    ctrl_g;
contains_ctrl_g_or_ctrl_c(<<$\^C,_/binary>>) ->
    ctrl_c;
contains_ctrl_g_or_ctrl_c(<<_/utf8,T/binary>>) ->
    contains_ctrl_g_or_ctrl_c(T);
contains_ctrl_g_or_ctrl_c(<<>>) ->
    none.

switch_loop(internal, init, State) ->
    case application:get_env(stdlib, shell_esc, jcl) of
	abort ->
	    CurrGroup = gr_cur_pid(State#state.groups),
	    exit(CurrGroup, die),
	    Gr1 =
		case gr_get_info(State#state.groups, CurrGroup) of
		    {_Ix,{}} ->	% no shell
			State#state.groups;
		    _ ->
			receive {'EXIT',CurrGroup,_} ->
				gr_del_pid(State#state.groups, CurrGroup)
			after 1000 ->
				State#state.groups
			end
		end,
	    NewGroup = group:start(self(), {shell,start,[]}),
	    io_request({put_chars,unicode,"\n"}, State#state.port),
            {next_state, server,
             State#state{ groups = gr_add_cur(Gr1, NewGroup, {shell,start,[]})}};
	jcl ->
	    io_request({put_chars,unicode,"\nUser switch command\n"}, State#state.port),
	    %% init edlin used by switch command and have it copy the
	    %% text buffer from current group process
	    edlin:init(gr_cur_pid(State#state.groups)),
            {keep_state_and_data,
             {next_event, internal, line}}
    end;
switch_loop(internal, line, State) ->
    {more_chars, Cont, Rs} = edlin:start(" --> "),
    io_requests(Rs, State#state.port),
    {keep_state, {Cont, State}};
switch_loop(internal, {line, Line}, State) ->
    case erl_scan:string(Line) of
        {ok, Tokens, _} ->
            case switch_cmd(Tokens, State#state.port, State#state.groups) of
                {ok, Groups} ->
                    {next_state, server,
                     State#state{ current_group = gr_cur_pid(Groups), groups = Groups } };
                retry ->
                    {keep_state_and_data,
                     {next_event, internal, line}};
                {retry, Groups} ->
                    {keep_state, State#state{ current_group = gr_cur_pid(Groups),
                                              groups = Groups },
                     {next_event, internal, line}}
            end;
        {error, _, _} ->
            io_request({put_chars,unicode,"Illegal input\n"}, State#state.port),
            {keep_state_and_data,
             {next_event, internal, line}}
    end;
switch_loop(info,{Port,{data,Cs}}, {Cont, State}) ->
    case edlin:edit_line(Cs, Cont) of
        {done,Line,_Rest, Rs} ->
            io_requests(Rs, State#state.port),
            {keep_state, State, {next_event, internal, {line, Line}}};
        {undefined,_Char,MoreCs,NewCont,Rs} ->
            io_requests(Rs, State#state.port),
            io_request(beep, State#state.port),
            {keep_state, {NewCont, State},
             {next_state, info, {Port,{data,MoreCs}}}};
        {more_chars,NewCont,Rs} ->
            io_requests(Rs, State#state.port),
            {keep_state, {NewCont, State}};
        {blink,NewCont,Rs} ->
            io_requests(Rs, State#state.port),
            {keep_state, {NewCont, State}, 1000}
    end;
switch_loop(timeout, _, State) ->
    {keep_state_and_data,
     {next_state, info,{State#state.port,{data,[]}}}};
switch_loop(info, _Unknown, _State) ->
    {keep_state_and_data, postpone}.

switch_cmd([{atom,_,Key},{Type,_,Value}], Port, Gr)
  when Type =:= atom; Type =:= integer ->
    switch_cmd({Key, Value}, Port, Gr);
switch_cmd([{atom,_,Key},{atom,_,V1},{atom,_,V2}], Port, Gr) ->
    switch_cmd({Key, V1, V2}, Port, Gr);
switch_cmd([{atom,_,Key}], Port, Gr) ->
    switch_cmd(Key, Port, Gr);
switch_cmd([{'?',_}], Port, Gr) ->
    switch_cmd(h, Port, Gr);

switch_cmd(Cmd, Port, Gr) when Cmd =:= c; Cmd =:= i; Cmd =:= k ->
    switch_cmd({Cmd, gr_cur_index(Gr)}, Port, Gr);
switch_cmd({c, I}, Port, Gr0) ->
    case gr_set_cur(Gr0, I) of
	{ok,Gr} -> {ok, Gr};
	undefined -> unknown_group(Port)
    end;
switch_cmd({i, I}, Port, Gr) ->
    case gr_get_num(Gr, I) of
	{pid,Pid} ->
	    exit(Pid, interrupt),
	    retry;
	undefined ->
	    unknown_group(Port)
    end;
switch_cmd({k, I}, Port, Gr) ->
    case gr_get_num(Gr, I) of
	{pid,Pid} ->
	    exit(Pid, die),
	    case gr_get_info(Gr, Pid) of
		{_Ix,{}} ->			% no shell
		    retry;
		_ ->
                    receive {'EXIT',Pid,_} ->
                            {retry,gr_del_pid(Gr, Pid)}
                    after 1000 ->
                            {retry,Gr}
                    end
	    end;
	undefined ->
	    unknown_group(Port)
    end;
switch_cmd(j, Port, Gr) ->
    io_requests(gr_list(Gr), Port),
    retry;
switch_cmd({s, Shell}, _Port, Gr0) when is_atom(Shell) ->
    Pid = group:start(self(), {Shell,start,[]}),
    Gr = gr_add_cur(Gr0, Pid, {Shell,start,[]}),
    {retry, Gr};
switch_cmd(s, Port, Gr) ->
    switch_cmd({s, shell}, Port, Gr);
switch_cmd(r, Port, Gr0) ->
    case is_alive() of
	true ->
	    Node = pool:get_node(),
	    Pid = group:start(self(), {Node,shell,start,[]}),
	    Gr = gr_add_cur(Gr0, Pid, {Node,shell,start,[]}),
	    {retry, Gr};
	false ->
	    io_request({put_chars,unicode,"Not alive\n"}, Port),
            retry
    end;
switch_cmd({r, Node}, Port, Gr) when is_atom(Node)->
    switch_cmd({r, Node, shell}, Port, Gr);
switch_cmd({r,Node,Shell}, _Port, Gr0) when is_atom(Node),
                                            is_atom(Shell) ->
    Pid = group:start(self(), {Node,Shell,start,[]}),
    Gr = gr_add_cur(Gr0, Pid, {Node,Shell,start,[]}),
    {retry, Gr};
switch_cmd([{atom,_,q}], Port, _Gr) ->
    case erlang:system_info(break_ignored) of
	true ->					% noop
	    io_request({put_chars,unicode,"Unknown command\n"}, Port),
	    retry;
	false ->
	    halt()
    end;
switch_cmd(h, Port, _Gr) ->
    list_commands(Port),
    retry;
switch_cmd([], _Port, _Gr) ->
    retry;
switch_cmd(_Ts, Port, _Gr) ->
    io_request({put_chars,unicode,"Unknown command\n"}, Port),
    retry.

unknown_group(Port) ->
    io_request({put_chars,unicode,"Unknown job\n"}, Port),
    retry.


list_commands(Port) ->
    QuitReq = case erlang:system_info(break_ignored) of
		  true -> 
		      [];
		  false ->
		      [{put_chars, unicode,"  q                 - quit erlang\n"}]
	      end,
    io_requests([{put_chars, unicode,"  c [nn]            - connect to job\n"},
		 {put_chars, unicode,"  i [nn]            - interrupt job\n"},
		 {put_chars, unicode,"  k [nn]            - kill job\n"},
		 {put_chars, unicode,"  j                 - list all jobs\n"},
		 {put_chars, unicode,"  s [shell]         - start local shell\n"},
		 {put_chars, unicode,"  r [node [shell]]  - start remote shell\n"}] ++
		QuitReq ++
		[{put_chars, unicode,"  ? | h             - this message\n"}],
		Port).

% Let driver report window geometry,
% definitely outside of the common interface
get_tty_geometry(Port) ->
    case (catch port_control(Port,?CTRL_OP_GET_WINSIZE,[])) of
	List when length(List) =:= 8 -> 
	    <<W:32/native,H:32/native>> = list_to_binary(List),
	    {W,H};
	_ ->
	    error
    end.
get_unicode_state(Port) ->
    case (catch port_control(Port,?CTRL_OP_GET_UNICODE_STATE,[])) of
	[Int] when Int > 0 -> 
	    true;
	[Int] when Int =:= 0 ->
	    false;
	_ ->
	    error
    end.

set_unicode_state(Port, Bool) ->
    Data = case Bool of
	       true -> [1];
	       false -> [0]
	   end,
    case (catch port_control(Port,?CTRL_OP_SET_UNICODE_STATE,Data)) of
	[Int] when Int > 0 -> 
	    {unicode, utf8};
	[Int] when Int =:= 0 ->
	    {unicode, false};
	_ ->
	    error
    end.

%% io_request(Request, InPort, OutPort)
%% io_requests(Requests, InPort, OutPort)
%% Note: InPort is unused.
io_request({requests,Rs}, Port) ->
    io_requests(Rs, Port);
io_request(Request, Port) ->
    case io_command(Request) of
        {Data, Reply} ->
            true = port_command(Port, Data),
            Reply;
        unhandled ->
            ok
    end.

io_requests([R|Rs], Port) ->
    io_request(R, Port),
    io_requests(Rs, Port);
io_requests([], _Port) ->
    ok.

put_int16(N, Tail) ->
    [(N bsr 8)band 255,N band 255|Tail].

%% When a put_chars_sync command is used, user_drv guarantees that
%% the bytes have been put in the buffer of the port before an acknowledgement
%% is sent back to the process sending the request. This command was added in
%% OTP 18 to make sure that data sent from io:format is actually printed
%% to the console before the vm stops when calling erlang:halt(integer()).
-dialyzer({no_improper_lists, io_command/1}).
io_command({put_chars_sync, unicode,Cs,Reply}) ->
    {[?OP_PUTC_SYNC|unicode:characters_to_binary(Cs,utf8)], Reply};
io_command({put_chars, unicode,Cs}) ->
    {[?OP_PUTC|unicode:characters_to_binary(Cs,utf8)], ok};
io_command({move_rel,N}) ->
    {[?OP_MOVE|put_int16(N, [])], ok};
io_command({insert_chars,unicode,Cs}) ->
    {[?OP_INSC|unicode:characters_to_binary(Cs,utf8)], ok};
io_command({delete_chars,N}) ->
    {[?OP_DELC|put_int16(N, [])], ok};
io_command(beep) ->
    {[?OP_BEEP], ok};
io_command(_) ->
    unhandled.

%% gr_new()
%% gr_get_num(Group, Index)
%% gr_get_info(Group, Pid)
%% gr_add_cur(Group, Pid, Shell)
%% gr_set_cur(Group, Index)
%% gr_cur_pid(Group)
%% gr_cur_index(Group)
%% gr_del_pid(Group, Pid)
%%  Manage the group list. The group structure has the form:
%%	{NextIndex,CurrIndex,CurrPid,GroupList}
%%
%%  where each element in the group list is:
%%	{Index,GroupPid,Shell}
-record(group, { index, pid, shell }).
-record(gr, { next = 0, current = 0, pid = none, groups = []}).
gr_new() ->
    #gr{}.
gr_new_group(I, P, S) ->
    #group{ index = I, pid = P, shell = S }.

gr_get_num(#gr{ groups = Gs }, I) ->
    case lists:keyfind(I, #group.index, Gs) of
        false -> undefined;
        #group{ shell = {} } ->
            undefined;
        #group{ pid = Pid } ->
            {pid, Pid}
    end.

gr_get_info(#gr{ groups = Gs }, Pid) ->
    case lists:keyfind(Pid, #group.pid, Gs) of
        false -> undefined;
        #group{ index = I, shell = S } ->
            {I, S}
    end.

gr_add_cur(#gr{ next = Next, groups = Gs}, Pid, Shell) ->
    #gr{ next = Next + 1, current = Next, pid = Pid,
         groups = Gs ++ [gr_new_group(Next, Pid, Shell)]
       }.

gr_set_cur(Gr, I) ->
    case gr_get_num(Gr, I) of
	{pid,Pid} -> {ok, Gr#gr{ current = I, pid = Pid }};
	undefined -> undefined
    end.

gr_set_num(Gr = #gr{ groups = Groups }, I, Pid, Shell) ->
    NewGroups = lists:keystore(I, #group.index, Groups, gr_new_group(I,Pid,Shell)),
    Gr#gr{ groups = NewGroups }.


gr_del_pid(Gr = #gr{ groups = Groups }, Pid) ->
    Gr#gr{ groups = lists:keydelete(Pid, #group.pid, Groups) }.


gr_cur_pid(#gr{ pid = Pid }) ->
    Pid.
gr_cur_index(#gr{ current = Index }) ->
    Index.

gr_list(#gr{ current = Current, groups = Groups}) ->
    lists:flatmap(
      fun(#group{ shell = {} }) ->
              [];
         (#group{ index = I, shell = S }) ->
              Marker = ["*" || Current =:= I],
              [{put_chars, unicode,
                lists:flatten(io_lib:format("~4w~.1ts ~w\n", [I,Marker,S]))}]
      end, Groups).
