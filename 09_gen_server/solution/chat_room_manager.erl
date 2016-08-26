-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2]).
-export([loop/1, handle_call/2]).

-type(server() :: pid()).
-type(room_id() :: reference()).
-type(name() :: binary()).
-type(message() :: {name(), binary()}).

-record(room, {
          id :: room_id(),
          name :: name(),
          users = [] :: [name()],
          history = [] :: [message()]
         }).

-record(state, {
          max_rooms = 5 :: integer(),
          rooms = maps:new() :: map()
         }).


-spec start() -> server().
start() ->
    spawn(?MODULE, loop, [#state{}]).


-spec create_room(server(), name()) -> {ok, room_id()} | {error, term()}.
create_room(Server, RoomName) ->
    call(Server, {create_room, RoomName}).


-spec remove_room(server(), room_id()) -> ok | {error, term()}.
remove_room(Server, RoomId) ->
    call(Server, {remove_room, RoomId}).


-spec get_rooms(server()) -> [{room_id(), name()}].
get_rooms(Server) ->
    call(Server, get_rooms).


-spec add_user(server(), room_id(), name()) -> ok | {error, term()}.
add_user(Server, RoomId, UserName) ->
    call(Server, {add_user, RoomId, UserName}).


-spec remove_user(server(), room_id(), name()) -> ok | {error, term()}.
remove_user(Server, RoomId, UserName) ->
    call(Server, {remove_user, RoomId, UserName}).


-spec get_users_list(server(), room_id()) -> {ok, [name()]} | {error, term()}.
get_users_list(Server, RoomId) ->
    call(Server, {get_users_list, RoomId}).


-spec send_message(server(), room_id(), name(), binary()) -> ok.
send_message(Server, RoomId, UserName, Message) ->
    call(Server, {send_message, RoomId, UserName, Message}).


-spec get_messages_history(server(), room_id()) -> [message()].
get_messages_history(Server, RoomId) ->
    call(Server, {get_messages_history, RoomId}).


call(Server, Msg) ->
    MRef = erlang:monitor(process, Server),
    Server ! {Msg, self(), MRef},
    receive
        {reply, MRef, Reply} ->
            erlang:demonitor(MRef, [flush]),
            Reply;
        {'DOWN', MRef, _, _, Reason} ->
            {error, Reason}
    after 5000 ->
            erlang:demonitor(MRef, [flush]),
            no_reply
    end.


loop(State) ->
    receive
        {Msg, From, Ref} ->
            {Reply, State2} = ?MODULE:handle_call(Msg, State),
            From ! {reply, Ref, Reply},
            ?MODULE:loop(State2);
        stop -> ok;
        _Any -> ?MODULE:loop(State)
    end.


handle_call({create_room, RoomName}, #state{rooms = Rooms, max_rooms = MaxRooms} = State) ->
    case maps:size(Rooms) of
        S when S >= MaxRooms -> {{error, room_limit}, State};
        _ -> RoomId = make_ref(),
             NewRoom = #room{id = RoomId, name = RoomName},
             {{ok, RoomId}, State#state{rooms = Rooms#{RoomId => NewRoom}}}
    end;

handle_call({remove_room, RoomId}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, _Room} -> Rooms2 = maps:remove(RoomId, Rooms),
                       {ok, State#state{rooms = Rooms2}};
        error -> {{error, room_not_found}, State}
    end;

handle_call(get_rooms, #state{rooms = Rooms} = State) ->
    Reply = maps:fold(fun(RoomId, #room{name = Name}, Acc) ->
                              [{RoomId, Name} | Acc]
                      end, [], Rooms),
    {Reply, State};

handle_call({add_user, RoomId, UserName}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room{users = Users} = Room} ->
            case lists:member(UserName, Users) of
                false ->
                    Room2 = Room#room{users = [UserName | Users]},
                    {ok, State#state{rooms = Rooms#{RoomId => Room2}}};
                true -> {{error, user_is_in_room}, State}
            end;
        error -> {{error, room_not_found}, State}
    end;

handle_call({remove_user, RoomId, UserName}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room{users = Users} = Room} ->
            case lists:member(UserName, Users) of
                true ->
                    Users2 = lists:delete(UserName, Users),
                    Room2 = Room#room{users = Users2},
                    {ok, State#state{rooms = Rooms#{RoomId => Room2}}};
                false -> {{error, user_not_in_room}, State}
            end;
        error -> {{error, room_not_found}, State}
    end;

handle_call({get_users_list, RoomId}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room{users = Users}} ->
            {{ok, Users}, State};
        error -> {{error, room_not_found}, State}
    end;


handle_call({send_message, RoomId, UserName, Message}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room{history = History, users = Users} = Room} ->
            case lists:member(UserName, Users) of
                true ->
                    Msg = {UserName, Message},
                    Room2 = Room#room{history = [Msg | History]},
                    {ok, State#state{rooms = Rooms#{RoomId => Room2}}};
                false -> {{error, user_not_in_room}, State}
            end;
        error -> {{error, room_not_found}, State}
    end;


handle_call({get_messages_history, RoomId}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, #room{history = History}} ->
            {{ok, History}, State};
        error -> {{error, room_not_found}, State}
    end.
