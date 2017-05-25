-module(main).
-export([start/0]).
-include("room_interface.hrl").
-include("player_interface.hrl").

-record(world_data,{rooms, players = #{}}).

addPlayer(World, Name, Client) ->
  AlreadyCreated = maps:is_key(Name, World#world_data.players),
  case AlreadyCreated of
    true ->
      io:format("Player '~s' has come back.~n", [Name]),
      Player = maps:get(Name, World#world_data.players),
      Player ! {login, Client};
    false ->
      io:format("Player '~s' has arrived.~n", [Name]),
      Pid = spawn(player, new, #player_data{
                                  name = Name,
                                  client = Client,
                                  room = maps:get("Starting Room", World#world_data.rooms)
                                 }),
      World#world_data{
        players = maps:put(Name, Pid, World#world_data.players)
       }
  end.

worldEventLoop(World) ->
  Result = receive
             {newplayer, PlayerName, Callback} -> io:format("fooo~n"), addPlayer(World, PlayerName, Callback);
             {lostplayer, PlayerName} -> io:format("Player '~s' has departed.~n", [PlayerName]);
             {travel, PlayerName, Direction, Callback} -> ok;
             {take, PlayerName, Item, Callback} -> ok;
             {getRoomDescription, PlayerName, Callback} ->
               Callback ! {ok, "You are in a dark and lonely room."}
           end,
  case Result of
    ok -> worldEventLoop(World);
    NewWorld -> worldEventLoop(NewWorld)
  end.

start() ->
  {Rooms} = room:load_map("map.dat"),
  io:format("World initialized.~n"),
  spawn(net, listenForClients, [self()]),
	io:format("Listening for clients -- server ready.~n"),
  worldEventLoop(#world_data{
                    rooms = Rooms
                   }).
