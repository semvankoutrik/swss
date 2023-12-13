-module(channels_endpoint).

-export([init/2]).

init(Request, State) ->
    case cowboy_req:method(Request) of
        <<"GET">> ->
            handle_get(Request, State);
        <<"POST">> ->
            handle_post(Request, State);
        _ ->
            io:format(
                cowboy_req:method(Request)),
            {ok, cowboy_req:reply(405, #{}, Request), State}
    end.

handle_get(Request, State) ->
    io:format("GET ~p~n", [binary_to_list(cowboy_req:path(Request))]),

    {ok, Channels} = channels_server:get_channels(),

    Strings =
        lists:map(fun({Id, Name, Connections}) ->
                     "{ \"id\": \""
                     ++ Id
                     ++ "\", \"name\": \""
                     ++ Name
                     ++ "\", \"subscribers\": "
                     ++ integer_to_list(length(Connections))
                     ++ "}"
                  end,
                  Channels),

    Json = "[" ++ lists:join(",", Strings) ++ "]",

    {ok, cowboy_req:reply(200, #{}, Json, Request), State}.

handle_post(Request, State) ->
    {ok, Data, ModRequest} = cowboy_req:read_body(Request),

    Name = binary_to_list(Data),

    case length(Name) > 3 of
        true ->
            {ok, Id} = channels_server:create_channel(Name),

            {ok, cowboy_req:reply(200, #{}, "\"" ++ Id ++ "\"", ModRequest), State};
        false ->
            {ok,
             cowboy_req:reply(400,
                              #{},
                              "\"Name can not be shorter than three characters.\"",
                              ModRequest)}
    end.
