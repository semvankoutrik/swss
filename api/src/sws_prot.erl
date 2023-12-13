-module(sws_prot).

-export([create_pub/2, get_payload/1, create_error/1, create_new_channel/1, create_id/1]).

create_pub(ChannelId, Msg) ->
    "pub/" ++ ChannelId ++ ":" ++ Msg.

create_new_channel(ChannelId) ->
    "new_channel/" ++ ChannelId.

create_id(ChannelId) ->
    "id/" ++ ChannelId.

get_payload("cancel_sub/" ++ Payload) ->
    {cancel_sub, Payload};
get_payload("set_name/" ++ Payload) ->
    {set_name, Payload};
get_payload("sub/" ++ Payload) ->
    {subscribe, Payload};
get_payload(Action) ->
    {error, {invalid_action, Action}}.

create_error({invalid_action, Action}) ->
    "err/invalid_action:" ++ Action;
create_error({not_found, Action}) ->
    "err/not_found:" ++ Action.
