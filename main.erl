-module(main).
-export([start/0]).
-include("room_interface.hrl").

main_loop(Game_Data) ->
    A = receive
            Result -> Result
        end,
    io:format("A is ~p~n", [A]).

start() ->
    R = #room_data{
           description = "Hey man",
           name = "Joe"
          },
    room:save_map("map.dat", R),
    {Worked, [R2|_]} = room:load_map("map.dat"),
    case Worked of
        ok ->
            io:format("room 1 is ~p~nroom 2 is ~p~n",
                      [R, R2]);
        Error ->
            io:format("Got ~p reading from save.~n",
                      [Error])
    end.
