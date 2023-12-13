-module(sws_prot_SUITE).

-include_lib("eunit/include/eunit.hrl").

return_sub_msg_test() ->
    ?assertEqual({subscribe, "1"}, sws_prot:get_msg("sub/1")).

return_pub_msg_test() ->
    ?assertEqual({publish, {"1", "Bericht"}}, sws_prot:get_msg("pub/1:Bericht")).

error_invalid_pub_body_test() ->
    ?assertEqual({error, {missing_body}}, sws_prot:get_msg("pub/1:")).

error_invalid_pub_request_test() ->
    ?assertEqual({error, {invalid_payload, "1"}}, sws_prot:get_msg("pub/1")).

error_invalid_action_test() ->
    ?assertEqual({error, {invalid_action, "action/1"}}, sws_prot:get_msg("action/1")).
