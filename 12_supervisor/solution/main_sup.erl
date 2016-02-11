-module(main_sup).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).


init(_Args) ->
    io:format("main_sup init ~p~n", [self()]),
    SupervisorSpecification = #{strategy => one_for_one,
                                intensity => 10,
                                period => 1000},
    ChildSup1 =  #{id => child_sup_1,
                   start => {sup_1, start_link, []},
                   restart => permanent,
                   shutdown => 2000,
                   type => supervisor,
                   modules => [sup_1]
                  },
    ChildSup2 =  #{id => child_sup_2,
                   start => {sup_2, start_link, []},
                   restart => permanent,
                   shutdown => 2000,
                   type => supervisor,
                   modules => [sup_2]
                  },
    {ok, {SupervisorSpecification, [ChildSup1, ChildSup2]}}.
