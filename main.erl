-module(main).
-export([start/0]).
-include("room.hrl").

worldEventLoop() ->
	receive
		% {badarg,[{io,format,[<0.48.0>,"Player '~s' has arrived.~n",<<109,105,108,111,10>>],[]}
		{newplayer, PlayerName} -> io:format("Player '~s' has arrived.~n", [PlayerName]);
		{lostplayer, PlayerName} -> io:format("Player '~s' has departed.~n", [PlayerName])
	end,
	worldEventLoop().

start() ->
    R = #room{name = "A small room",
              description = "A really small room"},
    io:format("~s~n", [R#room.name]),
	io:format("World initialized.~n"),
	spawn(net, listenForClients, [self()]),
	io:format("Listening for clients -- server ready.~n"),
	worldEventLoop().
