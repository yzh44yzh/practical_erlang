-module(chat_room_manager_test).

-include_lib("eunit/include/eunit.hrl").

-record(room, {id, name, users = [], history = []}).


start_test() ->
    Pid = chat_room_manager:start(),
    ?assert(is_pid(Pid)),
    ?assert(is_process_alive(Pid)),
    Pid ! stop,
    ok.


room_test() ->
    Server = chat_room_manager:start(),

    ?assertEqual([], chat_room_manager:get_rooms(Server)),

    {ok, Room1} = chat_room_manager:create_room(Server, <<"Room 1">>),
    ?assertEqual([#room{id = Room1, name = <<"Room 1">>}],
                 chat_room_manager:get_rooms(Server)),

    {ok, Room2} = chat_room_manager:create_room(Server, <<"Room 2">>),
    ?assertEqual([#room{id = Room1, name = <<"Room 1">>},
                  #room{id = Room2, name = <<"Room 2">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),

    {ok, Room3} = chat_room_manager:create_room(Server, <<"Nice Room">>),
    ?assertEqual([#room{id = Room1, name = <<"Room 1">>},
                  #room{id = Room2, name = <<"Room 2">>},
                  #room{id = Room3, name = <<"Nice Room">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),

    {ok, Room4} = chat_room_manager:create_room(Server, <<"Room 4">>),
    {ok, Room5} = chat_room_manager:create_room(Server, <<"Room 5">>),
    {error, room_limit} = chat_room_manager:create_room(Server, <<"Room 6">>),
    ?assertEqual([#room{id = Room1, name = <<"Room 1">>},
                  #room{id = Room2, name = <<"Room 2">>},
                  #room{id = Room3, name = <<"Nice Room">>},
                  #room{id = Room4, name = <<"Room 4">>},
                  #room{id = Room5, name = <<"Room 5">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),

    {error, room_not_found} = chat_room_manager:remove_room(Server, make_ref()),

    ok = chat_room_manager:remove_room(Server, Room2),
    ?assertEqual([#room{id = Room1, name = <<"Room 1">>},
                  #room{id = Room3, name = <<"Nice Room">>},
                  #room{id = Room4, name = <<"Room 4">>},
                  #room{id = Room5, name = <<"Room 5">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),
    {error, room_not_found} = chat_room_manager:remove_room(Server, Room2),

    ok = chat_room_manager:remove_room(Server, Room4),
    ?assertEqual([#room{id = Room1, name = <<"Room 1">>},
                  #room{id = Room3, name = <<"Nice Room">>},
                  #room{id = Room5, name = <<"Room 5">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),

    ok = chat_room_manager:remove_room(Server, Room1),
    ?assertEqual([#room{id = Room3, name = <<"Nice Room">>},
                  #room{id = Room5, name = <<"Room 5">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),

    ok = chat_room_manager:remove_room(Server, Room3),
    ?assertEqual([#room{id = Room5, name = <<"Room 5">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),

    ok = chat_room_manager:remove_room(Server, Room5),
    ?assertEqual([], chat_room_manager:get_rooms(Server)),

    Server ! stop,
    ok.


user_test() ->
    Server = chat_room_manager:start(),
    {ok, Room1} = chat_room_manager:create_room(Server, <<"Room 1">>),
    {ok, Room2} = chat_room_manager:create_room(Server, <<"Room 2">>),

    ?assertEqual({ok, []}, chat_room_manager:get_users_list(Server, Room1)),
    ?assertEqual({ok, []}, chat_room_manager:get_users_list(Server, Room2)),
    ?assertEqual({error, room_not_found}, chat_room_manager:get_users_list(Server, make_ref())),

    ok = chat_room_manager:add_user(Server, Room1, <<"Bob">>),
    ok = chat_room_manager:add_user(Server, Room2, <<"Bill">>),
    {error, room_not_found} = chat_room_manager:add_user(Server, make_ref(), <<"John">>),
    ?assertEqual({ok, [<<"Bob">>]}, chat_room_manager:get_users_list(Server, Room1)),
    ?assertEqual({ok, [<<"Bill">>]}, chat_room_manager:get_users_list(Server, Room2)),

    ok = chat_room_manager:add_user(Server, Room1, <<"Helen">>),
    ok = chat_room_manager:add_user(Server, Room2, <<"Kate">>),
    ?assertEqual({ok, [<<"Helen">>, <<"Bob">>]}, chat_room_manager:get_users_list(Server, Room1)),
    ?assertEqual({ok, [<<"Kate">>, <<"Bill">>]}, chat_room_manager:get_users_list(Server, Room2)),

    {error, room_not_found} = chat_room_manager:remove_user(Server, make_ref(), <<"Bob">>),
    {error, user_not_in_room} = chat_room_manager:remove_user(Server, Room2, <<"Bob">>),
    ok = chat_room_manager:remove_user(Server, Room1, <<"Bob">>),
    ?assertEqual({ok, [<<"Helen">>]}, chat_room_manager:get_users_list(Server, Room1)),
    ok = chat_room_manager:remove_user(Server, Room1, <<"Helen">>),
    ?assertEqual({ok, []}, chat_room_manager:get_users_list(Server, Room1)),

    Server ! stop,
    ok.
