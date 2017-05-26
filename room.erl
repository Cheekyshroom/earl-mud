-module(room).
-export([new/1,
         load_map/1,
         save_map/2
        ]).
-include("room_interface.hrl").

new(Room) ->
  Result = receive
             {Pid, broadcast, Message} ->
               io:format("Broadcasting ~p to ~p~n", [Message, Room#room_data.players]),
               lists:foreach(fun(Player) ->
                                 Player ! {Pid, output, Message}
                             end, Room#room_data.players);
             {Pid, get} -> Pid ! Room;
             {Pid, room_in_direction, Direction} ->
               Pid ! maps:find(Direction, Room#room_data.exits);
             {_, enter, Player} ->
               io:format("~p has entered ~p~n", [Player, self()]),
               NewPlayers = [Player | [P || P <- Room#room_data.players, P /= Player]],
               {update_room, Room#room_data{players = NewPlayers}};
             {_, exit, Player} ->
               NewPlayers = [P || P <- Room#room_data.players, P /= Player],
               {update_room, Room#room_data{players = NewPlayers}};
             {_, set_exits, Exits} ->
               {update_room, Room#room_data{exits = Exits}}
           end,
  case Result of
    {update_room, NewRoom} -> new(NewRoom);
    _ -> new(Room)
  end.

%%% Takes a filename and returns a map of [existing PID] => room_data
load_map(Filename) ->
  {ok, [Rooms | _]} = file:consult(Filename),
  OldPidToActor = maps:map(fun(_, RoomData) ->
                               spawn(room, new, [RoomData])
                           end,
                           Rooms),
  NewPidToActor = maps:fold(fun(OldPid, Actor, Out) ->
                                OldRoom = maps:get(OldPid, Rooms),
                                OldExits = OldRoom#room_data.exits,
                                NewExits = maps:map(fun(_, OldExitPid) ->
                                                         maps:get(OldExitPid, OldPidToActor)
                                                     end,
                                                     OldExits),
                                Actor ! {self(), set_exits, NewExits},
                                Actor ! {self(), get},
                                Data = receive
                                         Result -> Result
                                       end,
                                maps:put(Actor, Data, Out)
                            end,
                            #{}, OldPidToActor),
  {NewPidToActor}.

%%% Saves a map of [current PID] => room_data
save_map(Filename, Rooms) ->
  %% Make all the pids in the exits and Room object be strings instead of Pids
  ToString = fun(Datum) -> io_lib:format("\"~p\"", [Datum]) end,
  StringifiedRooms = maps:fold(fun(OldPid, RoomData, Out) ->
                                   NewExits = maps:map(fun(_, Pid) -> ToString(Pid) end, RoomData#room_data.exits),
                                   maps:put(ToString(OldPid),
                                            RoomData#room_data{
                                              exits = NewExits
                                             },
                                            Out)
                               end,
                               #{}, Rooms),
  file:write_file(Filename, io_lib:format("~p.~n", [StringifiedRooms])).
