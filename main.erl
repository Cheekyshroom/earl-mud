-module(main).
-export([start/0]).
-include("room_interface.hrl").

worldEventLoop() ->
	receive
    % {badarg,[{io,format,[<0.48.0>,"Player '~s' has arrived.~n",<<109,105,108,111,10>>],[]}
		{newplayer, PlayerName} -> io:format("Player '~s' has arrived.~n", [PlayerName]);
		{lostplayer, PlayerName} -> io:format("Player '~s' has departed.~n", [PlayerName])
	end,
	worldEventLoop().

start() ->
  {Worked, [R|_]} = room:load_map("map.dat"),
  case Worked of
    ok -> ok;
    _ -> throw("Map load failed")
  end,
  %% R ! {self(), enter, player1},
  %% R ! {self(), get, players},
  io:format("World initialized.~n"),
  spawn(net, listenForClients, [self()]),
	io:format("Listening for clients -- server ready.~n"),
  worldEventLoop().
