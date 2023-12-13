-module(header_middleware).

-export([execute/2]).

execute(Request, Env) ->
    ModRequest =
        cowboy_req:set_resp_headers(#{<<"Content-Type">> => <<"application/json">>,
                                      <<"Access-Control-Allow-Origin">> => <<"*">>,
                                      <<"Access-Control-Allow-Methods">> =>
                                          <<"GET, POST, OPTIONS">>},
                                    Request),
    {ok, ModRequest, Env}.
