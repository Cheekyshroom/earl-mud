-module(main).
-export([start/0]).
-include("room_interface.hrl").

main_loop(Game_Data) ->
    A = receive
            Result -> Result
        end,
    io:format("A is ~p~n", [A]).

start() ->
    R = spawn(room, new, [#room_data{name = "A small room",
                                description = "A really small room"
                               }]),
    R ! {self(), enter, player1},
    R ! {self(), get, players},
    main_loop(10).
