-module(sup_1).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_Args) ->
    io:format("sup_1 init ~p~n", [self()]),
    SupervisorSpecification = #{strategy => one_for_one,
                                intensity => 10,
                                period => 1000},
    Worker1 =  #{id => worker_1,
                 start => {worker, start_link, [worker_1]},
                 restart => permanent,
                 shutdown => 2000,
                 type => supervisor,
                 modules => [worker]
                },
    Worker2 =  #{id => worker_2,
                 start => {worker, start_link, [worker_2]},
                 restart => permanent,
                 shutdown => 2000,
                 type => supervisor,
                 modules => [worker]
                },
    {ok, {SupervisorSpecification, [Worker1, Worker2]}}.
