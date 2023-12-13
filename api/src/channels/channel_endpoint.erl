-module(channel_endpoint).

-export([init/2]).

init(Request, State) ->
    case cowboy_req:method(Request) of
        <<"POST">> ->
            handle_post(Request, State);
        _ ->
            io:format(
                cowboy_req:method(Request)),
            {ok, cowboy_req:reply(405, #{}, Request), State}
    end.

handle_post(Request, State) ->
    Id = cowboy_req:binding(id, Request),
    {ok, Data, ModRequest} = cowboy_req:read_body(Request),

    case channels_server:publish(binary_to_list(Id), binary_to_list(Data)) of
        {ok} ->
            {ok, cowboy_req:reply(204, #{}, ModRequest), State};
        {error, not_found} ->
            {ok, cowboy_req:reply(404, #{}, ModRequest), State};
        Response ->
            io:format("No matching clause when trying to publish a message to ~p. Got response: ~p~n",
                      [Id, Response]),
            {ok, cowboy_req:reply(500, #{}, ModRequest), State}
    end.
