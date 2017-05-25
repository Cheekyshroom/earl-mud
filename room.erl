-module(room).
-export([new/1,
         load_map/1,
         save_map/2
        ]).
-include("room_interface.hrl").

new(Room) ->
    Result = receive
                 {Pid, get} ->
                     Pid ! {room_data, Room};
                 {_, enter, Player} ->
                     Room#room_data{players = [Player | Room#room_data.players]};
                 {_, exit, Player} ->
                     NewPlayers = lists:filter(fun(P) -> P /= Player end),
                     Room#room_data{players = NewPlayers}
             end,
    case Result of
        ok -> new(Room);
        NewRoom -> new(NewRoom)
    end.

load_map(Filename) ->
    file:consult(Filename).
        
save_map(Filename, RootRoom) ->
    file:write_file(Filename, io_lib:format("~p.~n", [RootRoom])).
