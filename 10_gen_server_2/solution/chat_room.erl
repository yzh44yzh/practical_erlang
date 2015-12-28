-module(chat_room).
-behavior(gen_server).

-export([start_link/0, add_user/3, get_users/1, add_message/3, get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(name() :: binary()).
-type(user() :: {name(), pid()}).
-type(message() :: {name(), binary()}).

-record(state, {
          users = [] :: [user()],
          history = [] :: [message()]
         }).


%%% module API

start_link() ->
    gen_server:start_link(?MODULE, [], []).


-spec add_user(pid(), name(), pid()) -> ok.
add_user(RoomPid, UserName, UserPid) ->
    gen_server:cast(RoomPid, {add_user, {UserName, UserPid}}), ok.


-spec get_users(pid()) -> [user()].
get_users(RoomPid) ->
    gen_server:call(RoomPid, get_users).


-spec add_message(pid(), name(), binary()) -> ok.
add_message(RoomPid, UserName, Message) ->
    gen_server:cast(RoomPid, {add_message, {UserName, Message}}), ok.


-spec get_history(pid()) -> [message()].
get_history(RoomPid) ->
    gen_server:call(RoomPid, get_history).


%%% gen_server API

init([]) ->
    {ok, #state{}}.


handle_call(get_users, _From, #state{users = Users} = State) ->
    {reply, Users, State};

handle_call(get_history, _From, #state{history = Messages} = State) ->
    Reply = lists:reverse(Messages),
    {reply, Reply, State}.


handle_cast({add_user, User}, #state{users = Users} = State) ->
    {noreply, State#state{users = [User | Users]}};

handle_cast({add_message, {Name, Message}}, #state{users = Users, history = Messages} = State) ->
    lists:foreach(fun({_UserName, UserPid}) ->
                          chat_user:add_message(UserPid, Name, Message)
                  end, Users),
    {noreply, State#state{history = [{Name, Message} | Messages]}}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
