-module(strings).
-export([rstrip/1, lcTokens/2]).

rstrip(String) -> re:replace(String, "\\s+$", "", [global,{return,list}]).

% Like "string:tokens", but also lower cases the strings
lcTokens(String, Separator) ->
	Tokens = string:tokens(String, Separator),
	lists:map(fun string:to_lower/1, Tokens).
