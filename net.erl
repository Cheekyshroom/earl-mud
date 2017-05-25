-module(net).
-export([listenForClients/1]).
-include("configuration.hrl").

-define(TCP_OPTIONS, [binary, {active, false}, {reuseaddr, true}]).

listenForClients(World) ->
	{ok, Socket} = gen_tcp:listen(?SERVER_PORT, ?TCP_OPTIONS),
	accept(Socket, World).

accept(ServerSock, World) ->
	{ok, ClientSock} = gen_tcp:accept(ServerSock),
	NewClientPid = spawn(fun() -> clientLogin(ClientSock, World) end),
	gen_tcp:controlling_process(ClientSock, NewClientPid),
	accept(ServerSock, World).

clientLogin(Sock, World) ->
	gen_tcp:send(Sock, "Login: "),
	case gen_tcp:recv(Sock, 0) of
		{ok, RawUsername} ->
			Username = re:replace(RawUsername, "\\s+", "", [global,{return,list}]),
			World ! {newplayer, Username},
			gen_tcp:send(Sock, ?BANNER),
			clientLoop(Sock, Username, World);
		{error, closed} ->
			ok
	end.	

clientLoop(Sock, Username, World) ->
	gen_tcp:send(Sock, ["Instructions, ", Username, "? "]),
	inet:setopts(Sock, [{active, once}]),
	receive
		{tcp, Sock, <<"quit", _/binary>>} -> 
			gen_tcp:close(Sock),
			World ! {lostplayer, Username};
		{tcp_closed, Sock} ->
			World ! {lostplayer, Username};
		{tcp_error, Sock, Reason} ->
			io:format("Error on socket ~p reason: ~p~n", [Sock, Reason]),
			World ! {lostplayer, Username};
		{tcp, Sock, <<"help", _/binary>>} -> 
			gen_tcp:send(Sock, "The Lord helps those who help themselves.\n"),
			clientLoop(Sock, Username, World);
		{tcp, Sock, Msg} ->
			gen_tcp:send(Sock, ["I heard: '", strings:rstrip(Msg), "'\n"]),
			clientLoop(Sock, Username, World)
	end.
	%case gen_tcp:recv(Sock, 0) of
		%{ok, Command} ->
			%gen_tcp:send(Sock, ["I heard: '", strings:rstrip(Command), "'\n"]),
			%clientLoop(Sock, Username, World);
		%{error, closed} ->
			%World ! {lostplayer, Username},
			%ok
	%end.
	
