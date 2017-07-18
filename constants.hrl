-define(BANNER, "
      W e l c o m e      t o
    ______           __      __  ___          ____
   / ____/___ ______/ /     /  |/  /_  ______/ / /
  / __/ / __ `/ ___/ /_____/ /|_/ / / / / __  / / 
 / /___/ /_/ / /  / /_____/ /  / / /_/ / /_/ /_/  
/_____/\\__,_/_/  /_/     /_/  /_/\\__,_/\\__,_(_)

").

-define(INSTRUCTIONS, "Earl-Mud is a Multi-User-Dungeon, or MUD. You can communicate to the game using simple English sentences. Do not include punctuation. If the game does not understand your instructions, try rephrasing them.").

-record(client, {sock, username, world, user = none}).
