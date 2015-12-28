-module(chat_user).
-behavior(gen_server).

-export([start_link/0, add_message/3, get_messages/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(name() :: binary()).
-type(message() :: {name(), binary()}).

-record(state, {
          messages = [] :: [message()]
         }).


%%% module API

start_link() ->
    gen_server:start_link(?MODULE, [], []).


-spec add_message(pid(), name(), binary()) -> ok.
add_message(Pid, UserName, Message) ->
    gen_server:cast(Pid, {add_message, {UserName, Message}}), ok.


-spec get_messages(pid()) -> [message()].
get_messages(Pid) ->
    gen_server:call(Pid, get_messages).


%%% gen_server API

init([]) ->
    {ok, #state{}}.


handle_call(get_messages, _From, #state{messages = Messages} = State) ->
    Reply = lists:reverse(Messages),
    {reply, Reply, State}.


handle_cast({add_message, Data}, #state{messages = Messages} = State) ->
    {noreply, State#state{messages = [Data | Messages]}}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
