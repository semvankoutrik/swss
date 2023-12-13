sws
=====

# Client WebSocket-messages
sub/<channel_id>
cancel_sub/<channel_id>
set_name/<name>

# Server WebSocket-messages
pub/<channel_id>:<msg>
id/<channel_id>
new_channel/<channel_id>
error/<code>:<action>

# Error-codes
- invalid_action
- not_found

# TODO
- Calls op channels_server asynchroon maken
- cancel_sub toevoegen
- new_channel toevoegen