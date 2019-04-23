-module(ws_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Strategy = {one_for_all, 10, 10},
    Childs = [

    ],
    {ok, {Strategy, Childs}}.
