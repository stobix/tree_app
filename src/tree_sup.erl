-module(tree_sup).
-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE,[]).

init(_) ->
    {ok, {{one_for_one, 3, 10},
        [{tree_srv, {tree_srv, start_link, []},
          permanent, 10, worker, [tree_srv]}]}}.

