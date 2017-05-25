-module(player).
-export([new/1]).
-include("room_interface.hrl").
-include("player_interface.hrl").

new(Player) ->
  Result = receive
             {Pid, describe_room} ->
               Player#player_data.room ! {self(), get},
               RoomData = receive A -> A end,
               Pid ! {room_description,
                      RoomData#room_data.description
                      ++ "\nWith exits leading: "
                      ++ string:join(maps:keys(RoomData#room_data.exits), ", ")
                     },
               ok;
             {Pid, get, name} ->
               Pid! {name, Player#player_data.name};
             {_, logout} ->
               Player#player_data{
                 client = false
                };
             {_, login, ClientPid} ->
               Player#player_data{
                 client = ClientPid
                };
             {Pid, message, Msg} ->
               Pid ! {self(), get, name},
               OtherName = receive
                        {name, Name} -> Name
                      end,
               case Player#player_data.client of
                 false -> ok;
                 Client -> Client! {message, OtherName, Msg}
               end;
             {Pid, travel, Direction} ->
               Room = Player#player_data.room,
               Room ! {self(), room_in_direction, Direction},
               receive
                 {ok, NextRoom} ->
                   Room ! {self(), exit, self()},
                   NextRoom ! {self(), enter, self()},
                   NextRoom ! {self(), get},
                   RoomData = receive A -> A end,
                   Pid ! {ok, RoomData#room_data.name},
                   Player#player_data{
                     room = NextRoom
                    };
                 error -> ok
               end
           end,
  case Result of
    ok -> new(Player);
    NewPlayer -> new(NewPlayer)
  end.
