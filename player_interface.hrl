%%% Player type

-record(player_data, {
          name,
          room,
          client = false,
          inventory = []
         }).
