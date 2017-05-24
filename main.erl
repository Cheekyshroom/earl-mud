-module(main).
-export([start/0]).
-include("room.hrl").

start() ->
    R = #room{name = "A small room",
              description = "A really small room"},
    io:format("~p~n", [R#room.name]).
