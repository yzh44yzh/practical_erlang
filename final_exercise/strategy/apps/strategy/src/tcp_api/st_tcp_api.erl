%%
%% DEPRECATED
%%

-module(st_tcp_api).

%% API
-export([on_connect/0]).

on_connect() ->
    Socket = make_ref(),
    {ok, PlayerPid} = supervisor:start_child(st_player_sup, [Socket]),
    PlayerPid. % is already in storage