-module(channels_server).

-export([start_link/0, init/1, terminate/2, handle_call/3]).
-export([create_channel/1, subscribe/1, publish/2, get_channels/0, cancel_sub/1,
         delete_channel/1]).

% API functions
create_channel(Name) ->
    quickrand:seed(),

    Id = uuid:uuid_to_string(
             uuid:get_v4_urandom()),

    io:format("Created channel with ID ~p~n", [Id]),

    gen_server:call(channels_server, {create_channel, Id, Name}).

delete_channel(Id) ->
    io:format("Deleted channel with ID ~p~n", [Id]),

    gen_server:call(channels_server, {delete_channel, Id}).

subscribe(Id) ->
    gen_server:call(channels_server, {subscribe, Id}).

cancel_sub(Id) ->
    gen_server:call(channels_server, {cancel_sub, Id}).

publish(Id, Msg) ->
    gen_server:call(channels_server, {publish, {Id, Msg}}).

get_channels() ->
    gen_server:call(channels_server, {get_channels}).

% gen_server-functions
init(_Env) ->
    Table = ets:new('ChannelsTable', [ordered_set]),

    {ok, Table}.

start_link() ->
    io:format("Starting the \"channels\"-server ~n"),

    gen_server:start_link({local, channels_server}, channels_server, [], []).

handle_call({create_channel, Id, Name}, _From, State) ->
    ets:insert(State, {Id, Name, []}),

    connections_server:send_msg(
        sws_prot:create_new_channel(Id)),

    {reply, {ok, Id}, State};
handle_call({delete_channel, Id}, _From, State) ->
    ets:delete(State, Id),

    connections_server:send_msg(
        sws_prot:create_new_channel("delete")),

    {reply, {ok, Id}, State};
handle_call({subscribe, Id}, {Pid, _Ref}, State) ->
    io:format("Subscribing to ~p~n", [Id]),

    case ets:lookup(State, Id) of
        [{Id, Name, Connections}] ->
            % TODO: Check if subscriber exists and if so return error
            NewConnections = lists:append([Pid], Connections),

            ets:insert(State, {Id, Name, NewConnections}),

            {reply, ok, State};
        [] ->
            {reply, {error, {not_found, Id}}, State}
    end;
handle_call({cancel_sub, Id}, {Pid, _Ref}, State) ->
    io:format("Canceling subscription to ~p~n", [Id]),

    case ets:lookup(State, Id) of
        [{Id, Name, Connections}] ->
            NewConnections = lists:filter(fun(Sub) -> Sub =/= Pid end, Connections),

            ets:insert(State, {Id, Name, NewConnections}),

            {reply, ok, State};
        [] ->
            {reply, {error, {not_found, Id}}, State}
    end;
handle_call({publish, {Id, Msg}}, _From, State) ->
    io:format("Publishing \"" ++ Msg ++ "\" to: ~p~n", [Id]),

    {reply, publish_msg(State, Id, Msg), State};
handle_call({get_channels}, _From, State) ->
    Channels = ets:tab2list(State),

    {reply, {ok, Channels}, State}.

terminate(_Reason, _Env) ->
    io:format("Terminating the channel-server... ~n").

publish_msg(Table, Id, Msg) ->
    case ets:lookup(Table, Id) of
        [{Id, _Name, Connections}] ->
            lists:foreach(fun(Pid) -> Pid ! {server_message, sws_prot:create_pub(Id, Msg)} end,
                          Connections),

            {ok};
        [] ->
            {error, not_found}
    end.
