-module(room).
-export([new/1]).
-include("room_interface.hrl").

new(Room) ->
    Result = receive
                 {Pid, get, description} ->
                     Pid ! {description, Room#room_data.description};
                 {Pid, get, name} ->
                     Pid ! {name, Room#room_data.name};
                 {Pid, get, exits} ->
                     Pid ! {exits, Room#room_data.exits};
                 {Pid, get, players} ->
                     Pid ! {players, Room#room_data.players};
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
              
       
        
