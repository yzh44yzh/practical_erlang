-module(strategy_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupervisorSpecification = #{
        strategy => one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecifications = [
        #{
            id => st_player_storage,
            start => {st_player_storage, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker,
            modules => [st_player_storage]
        },
        #{
            id => st_player_sup,
            start => {st_player_sup, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => supervisor,
            modules => [st_player_sup]
        }
    ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
