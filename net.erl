-module(net).
-export([listenForClients/1]).
-include("configuration.hrl").

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

listenForClients(World) ->
	{ok, Socket} = gen_tcp:listen(?SERVER_PORT, ?TCP_OPTIONS),
	accept(Socket, World).

accept(ServerSock, World) ->
	{ok, ClientSock} = gen_tcp:accept(ServerSock),
	spawn(fun() -> clientLogin(ClientSock, World) end),
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
	case gen_tcp:recv(Sock, 0) of
		{ok, Command} ->
			gen_tcp:send(Sock, ["I heard: '", strings:rstrip(Command), "'\n"]),
			clientLoop(Sock, Username, World);
		{error, closed} ->
			World ! {lostplayer, Username},
			ok
	end.
	
