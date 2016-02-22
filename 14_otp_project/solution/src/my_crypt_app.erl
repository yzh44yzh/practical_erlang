-module(my_crypt_app).
-behaviour(application).

-export([start/0, start/2, stop/1]).


-spec(start() -> ok).
start() ->
    application:start(my_crypt),
    ok.


-spec(start(term(), term()) -> {ok, pid()}).
start(_StartType, _StartArgs) ->
    my_crypt_sup:start_link().


-spec(stop(term()) -> ok).
stop(_State) ->
    ok.
