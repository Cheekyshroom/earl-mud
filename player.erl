-module(player).
-export([new/1]).
-include("room_interface.hrl").
-include("player_interface.hrl").

describeRoom(Player) ->
  Player#player_data.room ! {self(), get},
  receive
    RoomData ->
      "You're in the: "
        ++ RoomData#room_data.name
        ++ "\nYou see:\n"
        ++ RoomData#room_data.description
        ++ "\nWith exits leading: "
        ++ string:join(maps:keys(RoomData#room_data.exits), ", ")
  end.

outputToClient(Player, Msg) ->
  case Player#player_data.client of
    false -> io:format("No client found for ~p~n", [self()]), ok;
    Client -> Client! {output, Msg}
  end.

new(Player) ->
  Result = receive
             {Pid, get, name} ->
               Pid ! {name, Player#player_data.name};
             {_, logout} ->
               {update_player, Player#player_data{client = false}};
             {_, login, ClientPid} ->
               Player#player_data.room ! {self(), enter, self()},
               {update_player, Player#player_data{client = ClientPid}};
             {_, output, Msg} ->
               outputToClient(Player, Msg);
             {Pid, client_command, look} ->
               Description = describeRoom(Player),
               outputToClient(Player, ["\n", Description, "\n"]);
             {_, client_command, {say, Message}} ->
               FullMessage = [Player#player_data.name, " says \"", Message, "\""],
               Player#player_data.room ! {self(), broadcast_others, FullMessage};
             {Pid, client_command, {travel, Direction}} ->
               Room = Player#player_data.room,
               Room ! {self(), room_in_direction, Direction},
               receive
                 {ok, NextRoom} ->
                   Room ! {self(), exit, self()},
                   NextRoom ! {self(), enter, self()},
                   NextRoom ! {self(), get},
                   receive
                     RoomData ->
                       Pid ! {ok, RoomData#room_data.name},
                       {update_player, Player#player_data{
                                         room = NextRoom
                                        }}
                   end;
                 error -> ok
               end;
             {_, client_command, {take, Item}} ->
               io:format("~p taking item: ~p~n", [self(), Item])
           end,
  case Result of
    {update_player, NewPlayer} -> new(NewPlayer);
    _ -> new(Player)
  end.
