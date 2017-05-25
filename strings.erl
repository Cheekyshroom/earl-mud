-module(strings).
-export([rstrip/1]).

rstrip(String) -> re:replace(String, "\\s+$", "", [global,{return,list}]).
