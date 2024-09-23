-module(erlib).
-export([process_message_queue_len/0]).

process_message_queue_len() ->
    % Use process_info/2 to get the message_queue_len of the current process
    {message_queue_len, Len} = process_info(self(), message_queue_len),
    Len.
