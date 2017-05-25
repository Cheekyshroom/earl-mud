-module(player).
-export([new/1]).

new(Player) ->
  Result = receive
             {Pid, get, name} ->
               Pid! {name, Player#player_data.name};
             {Pid, logout} ->
               Player#player_data{
                 client = false
                };
             {Pid, login, ClientPid} ->
               Player#player_data{
                 client = ClientPid
                };
             {Pid, message, Msg} ->
               Pid ! {self(), get, name},
               Name = receive
                        {name, Name} -> Name
                      end,
               case Player#player_data.client of
                 false -> ok;
                 Client -> Client! {message, Name, Msg}
               end
           end,
  case Result of
    ok -> new(Player);
    NewPlayer -> new(NewPlayer)
  end.
