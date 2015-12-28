-module(chat_room_manager).
-behavior(gen_server).

-export([start_link/0,
         create_room/1, get_rooms/0,
         add_user/2, remove_user/2, get_users/1,
         send_message/3,  get_history/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-type(name() :: binary()).
-type(room() :: {name(), pid()}).
-type(user() :: {name(), pid()}).
-type(message() :: {name(), binary()}).

-record(state, {
          rooms :: map()
         }).


%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec create_room(name()) -> room().
create_room(RoomName) ->
    {ok, RoomPid} = chat_room:start_link(),
    Room = {RoomName, RoomPid},
    gen_server:cast(?MODULE, {add_room, Room}),
    Room.


-spec get_rooms() -> [room()].
get_rooms() ->
    gen_server:call(?MODULE, get_rooms).


-spec add_user(pid(), user()) -> ok | {error, room_not_found}.
add_user(RoomPid, User) ->
    gen_server:call(?MODULE, {add_user, {RoomPid, User}}).


-spec remove_user(pid(), user()) -> ok | {error, room_not_found} | {error, user_not_found}.
remove_user(RoomPid, UserPid) ->
    gen_server:call(?MODULE, {remove_user, RoomPid, UserPid}).


-spec get_users(pid()) -> {ok, [user()]} | {error, room_not_found}.
get_users(RoomPid) ->
    gen_server:call(?MODULE, {get_users, RoomPid}).


-spec send_message(pid(), name(), binary()) -> ok | {error, room_not_found}.
send_message(RoomPid, UserName, Message) ->
    gen_server:call(?MODULE, {add_message, {RoomPid, UserName, Message}}), ok.


-spec get_history(pid()) -> {ok, [message()]} | {error, room_not_found}.
get_history(RoomPid) ->
    gen_server:call(?MODULE, {get_history, RoomPid}).


%%% gen_server API

init([]) ->
    {ok, #state{rooms = maps:new()}}.


handle_call(get_room, _From, State) ->
    %% TODO [room()]
    Reply = [],
    {reply, Reply, State};

handle_call({add_user, {RoomPid, User}}, _From, State) ->
    %% TODO ok | {error, room_not_found}
    Reply = ok,
    {reply, Reply, State};

handle_call({remove_user, {RoomPid, UserPid}}, _From, State) ->
    %% TODO ok | {error, room_not_found} | {error, user_not_found}
    Reply = ok,
    {reply, Reply, State};

handle_call({get_users, RoomPid}, _From, State) ->
    %% TODO {ok, [user()]} | {error, room_not_found}
    Reply = ok,
    {reply, Reply, State};

handle_call({send_message, {RoomPid, UserName, Message}}, _From, State) ->
    %% TODO ok | {error, room_not_found}
    Reply = ok,
    {reply, Reply, State};

handle_call({get_history, RoomPid}, _From, State) ->
    %% TODO {ok, [message()]} | {error, room_not_found}
    Reply = ok,
    {reply, Reply, State}.


handle_cast({add_room, {RoomName, RoomPid}}, State) ->
    %% TODO
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.
