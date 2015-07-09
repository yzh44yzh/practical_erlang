-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/2, remove_user/2,
         get_users_list/2, get_messages_history/2,
         send_message/4]).

-record(room, {
          id,
          name,
          users = [],
          history = []
         }).

-record(state, {
          max_rooms = 10,
          max_users_in_room = 10,
          rooms = maps:new()
         }).


-spec start() -> pid().
start() ->
    spawn(?MODULE, loop, [#state{}]).


-spec create_room(pid(), binary()) -> {ok, reference()} | {error, term()}.
create_room(Pid, RoomName) ->
    call(Pid, {create_room, RoomName}).


-spec remove_room(pid(), reference()) -> ok | {error, term()}.
remove_room(Pid, RoomId) ->
    call(Pid, {remove_room, RoomId}).


-spec get_rooms(pid()) -> [#room{}].
get_rooms(Pid) ->
    call(Pid, get_rooms).


add_user(Pid, UserName) ->
    ok.


remove_user(Pid, UserName) ->
    ok.


get_users_list(Pid, RoomId) ->
    ok.


get_messages_history(Pid, RoomId) ->
    ok.


send_message(Pid, RoomId, UserName, Message) ->
    ok.


call(Pid, Msg) ->
    MRef = erlang:monitor(process, Pid),
    Pid ! {Msg, self(), MRef},
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
        _Any -> ?MODULE:loop(State)
    end.


handle_call({create_room, RoomName}, #state{rooms = Rooms, max_rooms = MaxRooms} = State) ->
    case maps:size(Rooms) of
        S when S >= MaxRooms -> {{error, room_limit}, State};
        _ -> RoomId = make_ref(),
             Rooms2 = maps:put(RoomId, #room{id = RoomId, name = RoomName}, Rooms),
             {{ok, RoomId}, State#state{rooms = Rooms2}}
    end;

handle_call({remove_room, RoomId}, #state{rooms = Rooms} = State) ->
    case maps:find(RoomId, Rooms) of
        {ok, _Room} -> Rooms2 = maps:remove(RoomId, Rooms),
                       {ok, State#state{rooms = Rooms2}};
        error -> {{error, not_found}, State}
    end;

handle_call(get_rooms, #state{rooms = Rooms} = State) ->
    {maps:values(Rooms), State}.
