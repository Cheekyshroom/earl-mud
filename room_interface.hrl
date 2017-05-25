%%% Room type.

-record(room_data, {
          name,
          description,
          exits = [],
          players = []
         }).
