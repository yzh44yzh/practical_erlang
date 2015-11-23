-module(raft).

-export([start/0]).


start() ->
    Self = node(),
    Cluster = [{Node, net_adm:ping(Node)} || Node <- cluster(), Node /= Self],
    io:format("self:~p~ncluster:~p~n", [Self, Cluster]),
    Cluster.


cluster() ->
    ['alpha@127.0.0.1',
     'bravo@127.0.0.1',
     'charlie@127.0.0.1',
     'delta@127.0.0.1',
     'echo@127.0.0.1'].
