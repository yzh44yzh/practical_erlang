-module(st_tcp_api).

%% API
-export([on_connect/0]).

on_connect() ->
    Socket = make_ref(),
    supervisor:start_child(st_player_sup, [Socket]).