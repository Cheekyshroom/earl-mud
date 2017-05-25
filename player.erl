-module(player).
-export([new/1]).
-include("player_interface.hrl").

new(Player) ->
  Result = receive
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
               end
           end,
  case Result of
    ok -> new(Player);
    NewPlayer -> new(NewPlayer)
  end.
