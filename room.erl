-module(room).
-export([new/1,
         load_map/1,
         save_map/2
        ]).
-include("room_interface.hrl").

new(Room) ->
  Result = receive
             {Pid, get} -> Pid ! Room, ok;
             {Pid, room_in_direction, Direction} ->
               Pid ! maps:find(Direction, Room#room_data.exits), ok;
             {_, enter, Player} ->
               Room#room_data{players = [Player | Room#room_data.players]};
             {_, exit, Player} ->
               NewPlayers = lists:filter(fun(P) -> P /= Player end, Room#room_data.players),
               Room#room_data{players = NewPlayers};
             {_, set_exits, Exits} ->
               Room#room_data{exits = Exits}
           end,
  case Result of
    ok -> new(Room);
    NewRoom -> new(NewRoom)
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
