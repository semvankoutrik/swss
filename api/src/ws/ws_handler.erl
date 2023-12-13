-module(ws_handler).

-export([init/2]).
-export([terminate/3]).
-export([websocket_handle/2]).
-export([websocket_init/1]).
-export([websocket_info/2]).
-export([new_ping_timer/0]).

init(Req, _State) ->
    {cowboy_websocket,
     Req,
     #{id => undefined,
       name => undefined,
       subscriptions => []}}.

new_ping_timer() ->
    io:format("~p~n", [self()]),

    erlang:start_timer(5000, self(), ping).

websocket_init(State) ->
    io:format("Starting WebSocket-process at: ~p~n", [self()]),

    new_ping_timer(),

    connections_server:add(),

    {[{text, <<"Welcome to the SWS-server!">>}], State}.

websocket_handle({text, Binary}, State) ->
    Message = binary_to_list(Binary),

    io:format("Incoming message: ~p~n", [Message]),

    case sws_prot:get_payload(Message) of
        {set_name, Name} ->
            {ok, Id} = channels_server:create_channel(Name),

            self() ! {server_message, sws_prot:create_id(Id)},

            Subscriptions = maps:get(subscriptions, State),

            {ok,
             #{id => Id,
               name => Name,
               subscriptions => Subscriptions}};
        {subscribe, ChannelId} ->
            case channels_server:subscribe(ChannelId) of
                {error, {not_found, ChannelId}} ->
                    self() ! {server_message, sws_prot:create_error({not_found, ChannelId})},

                    {ok, State};
                _ ->
                    Id = maps:get(id, State),
                    Name = maps:get(name, State),
                    Subscriptions =
                        lists:append(
                            maps:get(subscriptions, State), [ChannelId]),

                    {ok,
                     #{id => Id,
                       name => Name,
                       subscriptions => Subscriptions}}
            end;
        {cancel_sub, ChannelId} ->
            case channels_server:cancel_sub(ChannelId) of
                {error, {not_found, ChannelId}} ->
                    self() ! {server_message, sws_prot:create_error({not_found, ChannelId})},

                    {ok, State};
                _ ->
                    Id = maps:get(id, State),
                    Name = maps:get(name, State),
                    Subscriptions =
                        lists:filter(fun(Sub) -> Sub =:= ChannelId end,
                                     maps:get(subscriptions, State)),

                    {ok,
                     #{id => Id,
                       name => Name,
                       subscriptions => Subscriptions}}
            end;
        {error, {invalid_action, Action}} ->
            self() ! {server_message, sws_prot:create_error({invalid_action, Action})},

            {ok, State}
    end;
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({timeout, _Ref, ping}, State) ->
    ws_handler:new_ping_timer(),

    {[ping], State};
websocket_info({server_message, Text}, State) ->
    {[{text, Text}], State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _Req, State) ->
    io:format("WebSocket with PID ~p terminated with Reason ~p~n", [self(), Reason]),

    Id = maps:get(id, State),
    Subscriptions = maps:get(subscriptions, State),

    connections_server:remove(),
    channels_server:delete_channel(Id),
    lists:foreach(fun(Sub) -> channels_server:cancel_sub(Sub) end, Subscriptions),

    {ok, State}.
