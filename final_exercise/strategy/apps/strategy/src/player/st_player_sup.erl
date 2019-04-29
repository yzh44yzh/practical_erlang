-module(st_player_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    SupervisorSpecification = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60},

    ChildSpecifications =
        [
            #{
                id => st_player_srv,
                start => {st_player_srv, start_link, []},
                restart => temporary,
                shutdown => 2000,
                type => worker,
                modules => [st_player_srv]
            }
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
