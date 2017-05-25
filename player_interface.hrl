%%% Player type

-record(player_data, {
          name,
          room = false,
          client = false,
          inventory = []
         }).
