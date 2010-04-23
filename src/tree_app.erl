-module(tree_app).

-export([start/2,stop/1]).
start(_Type,_Args) ->
    tree_sup:start_link().

stop(_State) ->
    ok.
