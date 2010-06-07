-module(smtp).
-include_lib("nitrogen/include/wf.hrl").
-export([send/2]).
-compile(export_all).

send(Recipient, Message) ->
    spawn(fun () -> connect(Recipient, Message) end).

connect(Recipient, Message) ->
    {ok, Socket} = ssl:connect("smtp.gmail.com", 465, [{active, false}], 1000),
    recv(Socket),
    send_and_receive(Socket, "HELO localhost"),
    send_and_receive(Socket, "AUTH LOGIN"),
    send_and_receive(Socket, binary_to_list(base64:encode("invites@dayfindr.com"))),
    send_and_receive(Socket, binary_to_list(base64:encode("wXptvLA"))),
    send_and_receive(Socket, "MAIL FROM: <invites@dayfindr.com>"),
    send_and_receive(Socket, "RCPT TO:<" ++ Recipient ++ ">"),
    send_and_receive(Socket, "DATA"),
    send_no_receive(Socket, "From: <invites@dayfindr.com>"),
    send_no_receive(Socket, "To: <" ++ Recipient ++ ">"),
    send_no_receive(Socket, "Date: Tue, 15 Jan 2008 16:02:43 +0000"),
    send_no_receive(Socket, "Subject: Test message"),
    send_no_receive(Socket, ""),
    send_no_receive(Socket, Message),
    send_no_receive(Socket, ""),
    send_and_receive(Socket, "."),
    send_and_receive(Socket, "QUIT"),
    ssl:close(Socket),
    error_logger:info_msg("Sent invite to ~p", [Recipient]).

send_no_receive(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n").


send_and_receive(Socket, Data) ->
    ssl:send(Socket, Data ++ "\r\n"),
    recv(Socket).

recv(Socket) ->
    case ssl:recv(Socket, 0, 1000) of
	{ok, _} -> ok; %error_logger:info_msg("INFO: ~p~n", [Msg]);
	{error, Reason} -> error_logger:error_msg("ERROR: ~p~n", [Reason])
    end.
