-module(try_cowboy).

-export([start/0]).

start() ->
    io:format("starting cowboy~n"),
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowlib),
    ok = application:start(cowboy),

    Port = 8080,
    Routes = cowboy_router:compile(routes()),
    cowboy:start_http(http, 100,
                      [{port, Port}],
                      [{env, [{dispatch, Routes}]}]),
    io:format("cowboy started at port ~p~n", [Port]),
    ok.

routes()->
    [{'_',
      [
       {"/", index_handler, []}
      ]
     }].
