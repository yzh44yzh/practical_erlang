-module(mcache_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    SupervisorSpecification =
        #{strategy => one_for_one, % one_for_one | one_for_all | rest_for_one
          intensity => 10, % max restarts
          period => 1000 % in period of time
         },

    ChildSpecifications =
        [
         #{id => mcache,
           start => {mcache, start_link, []},
           restart => permanent, % permanent | transient | temporary
           shutdown => 2000, % milliseconds | brutal_kill | infinity
           type => worker, % worker | supervisor
           modules => [mcache]
          },
         #{id => mcache_server,
           start => {mcache_server, start_link, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [mcache_server]
          }
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
