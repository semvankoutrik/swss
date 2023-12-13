-module(sws_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags =
        #{strategy => one_for_one,
          intensity => 0,
          period => 1},

    Children =
        [#{id => channels_server,
           start => {channels_server, start_link, []},
           restart => permanent,
           shutdown => brutal_kill,
           type => worker,
           modules => [channels_server]},
         #{id => connections_server,
           start => {connections_server, start_link, []},
           restart => permanent,
           shutdown => brutal_kill,
           type => worker,
           modules => [connections_server]}],

    {ok, {SupFlags, Children}}.
