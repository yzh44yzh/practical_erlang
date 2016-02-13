-module(mylib_worker).
-behavior(gen_server).

-export([start_link/0, get_version/0, get_modules/0, get_min_val/0, get_connection_timeout/0, all_apps/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


get_version() ->
    {ok, Version} = application:get_key(mylib, vsn), % need to define application name
    Version.


get_modules() ->
    gen_server:call(?MODULE, get_modules).


get_min_val() ->
    {ok, Val} = application:get_env(mylib, min_val), % need to define application name
    Val.


get_connection_timeout() ->
    gen_server:call(?MODULE, get_connection_timeout).


all_apps() ->
    lists:foldl(fun({App, Desc, Version}, Acc) ->
                        Acc#{App => #{description => Desc,
                                      version => Version}}
                end,
                maps:new(),
                application:which_applications()).


%%% gen_server API

init([]) ->
    {ok, no_state}.


handle_call(get_modules, _From, State) ->
    {ok, Modules} = application:get_key(modules), % no need to define application name
    {reply, Modules, State};

handle_call(get_connection_timeout, _From, State) ->
    {ok, Timeout} = application:get_env(connection_timeout), % no need to define application name
    {reply, Timeout, State};

handle_call(Any, _From, State) ->
    error_logger:error_msg("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_cast(Any, State) ->
    error_logger:error_msg("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


handle_info(Request, State) ->
    error_logger:error_msg("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.



%%% inner functions
