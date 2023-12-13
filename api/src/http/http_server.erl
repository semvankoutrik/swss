-module(http_server).

-export([start/0]).

start() ->
    Routes =
        [{"/ws", ws_handler, []},
         {"/channels", channels_endpoint, []},
         {"/channels/:id", channel_endpoint, []}],

    Dispatch = cowboy_router:compile([{'_', Routes}]),

    {ok, _} =
        cowboy:start_clear(http_server,
                           [{port, 8080}],
                           #{env => #{dispatch => Dispatch},
                             middlewares => [header_middleware, cowboy_router, cowboy_handler]}),

    {ok}.
