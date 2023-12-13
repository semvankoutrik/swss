-module(connections_server).

-export([start_link/0, init/1, handle_call/3]).
-export([send_msg/1, add/0, remove/0]).

start_link() ->
    gen_server:start_link({local, connections_server}, connections_server, [], []).

init(_Env) ->
    {ok, []}.

handle_call({send_msg, Msg}, _From, State) ->
    lists:foreach(fun(Pid) -> Pid ! {server_message, Msg} end, State),

    {reply, {ok}, State};
handle_call({add}, {Pid, _Ref}, State) ->
    {reply, {ok}, lists:append(State, [Pid])};
handle_call({remove}, {Pid, _Ref}, State) ->
    {reply, {ok}, lists:filter(fun(Conn) -> Conn =/= Pid end, State)}.

send_msg(Msg) ->
    gen_server:call(connections_server, {send_msg, Msg}).

add() ->
    gen_server:call(connections_server, {add}).

remove() ->
    gen_server:call(connections_server, {remove}).
