-module(main).
-export([start/0]).
-include("room_interface.hrl").

worldEventLoop() ->
	receive
		{newplayer, PlayerName} -> io:format("Player '~s' has arrived.~n", [PlayerName]);
		{lostplayer, PlayerName} -> io:format("Player '~s' has departed.~n", [PlayerName]);
		{travel, PlayerName, Direction, Callback} -> ok;
		{take, PlayerName, Item, Callback} -> ok;
		{getRoomDescription, PlayerName, Callback} -> Callback ! {ok, "You are in a dark and lonely room."}
	end,
	worldEventLoop().

start() ->
  {Rooms} = room:load_map("map.dat"),
  io:format("World initialized.~n"),
  spawn(net, listenForClients, [self()]),
	io:format("Listening for clients -- server ready.~n"),
  worldEventLoop().
