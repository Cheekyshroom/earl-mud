-module(main).
-export([start/0]).
-include("room_interface.hrl").
-include("player_interface.hrl").

-record(world_data,{rooms, players = #{}}).

getRoomByName(World, Name) ->
  Rooms = World#world_data.rooms,
  maps:fold(fun(RoomId, RoomData, Out) ->
                if Name == RoomData#room_data.name ->
                    RoomId;
                   true ->
                    Out
                end
            end,
            false, Rooms).

getPlayerByName(World, Name) ->
  maps:get(Name, World#world_data.players).

addPlayer(World, Name, Client) ->
  AlreadyCreated = maps:is_key(Name, World#world_data.players),
  case AlreadyCreated of
    true ->
      io:format("Player '~s' has come back.~n", [Name]),
      Player = getPlayerByName(World, Name),
      Player ! {self(), login, Client},
      World;
    false ->
      io:format("Player '~s' has arrived.~n", [Name]),
      Pid = spawn(player, new, [#player_data{
                                   name = Name,
                                   client = Client,
                                   room = getRoomByName(World, "Starting Room")
                                  }]),
      World#world_data{
        players = maps:put(Name, Pid, World#world_data.players)
       }
  end.

worldEventLoop(World) ->
  Result = receive
             {newplayer, PlayerName, Callback} ->
               {update_world, addPlayer(World, PlayerName, Callback)};
             {lostplayer, PlayerName} ->
               io:format("Player '~s' has departed.~n", [PlayerName]),
               getPlayerByName(World, PlayerName) ! {self(), logout};
             {player_command, PlayerName, Command, Callback} ->
               Player = getPlayerByName(World, PlayerName),
               Player ! {Callback, client_command, Command};
             {getRoomDescription, PlayerName, Callback} ->
               Player = getPlayerByName(World, PlayerName),
               Player ! {self(), describe_room},
               {room_description, Description} = receive A -> A end,
               Callback ! {ok, Description}
           end,
  case Result of
    {update_world, NewWorld} -> worldEventLoop(NewWorld);
    _ -> worldEventLoop(World)
  end.

start() ->
  {Rooms} = room:load_map("map.dat"),
  io:format("World initialized.~n"),
  spawn(net, listenForClients, [self()]),
	io:format("Listening for clients -- server ready.~n"),
  worldEventLoop(#world_data{rooms = Rooms}).
