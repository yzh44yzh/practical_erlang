-module(strategy_worker).
-behavior(gen_server).

-export([start_link/2, hello/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
    user_id,
    user_name
}).

start_link(UserId, UserName) ->
    gen_server:start_link(?MODULE, {UserId, UserName}, []).

hello() ->
    42.

%%% gen_server API

init({UserId, UserName}) ->
    State = #state{user_id = UserId, user_name = UserName},
    lager:info("init ~p ~p", [self(), State]),
    lager:warning("this is warning"),
    lager:error("this is error"),
    {ok, State}.

handle_call(_Request, _From, #state{} = State) ->
    {reply, ok, State}.

handle_cast(_Request, #state{} = State) ->
    {noreply, State}.

handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
