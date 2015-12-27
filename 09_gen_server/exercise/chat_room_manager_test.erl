-module(chat_room_manager_test).

-include_lib("eunit/include/eunit.hrl").


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
    ?assertEqual([{Room1, <<"Room 1">>}],
                 chat_room_manager:get_rooms(Server)),

    {ok, Room2} = chat_room_manager:create_room(Server, <<"Room 2">>),
    ?assertEqual([{Room1, <<"Room 1">>},
                  {Room2, <<"Room 2">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),

    {ok, Room3} = chat_room_manager:create_room(Server, <<"Nice Room">>),
    ?assertEqual([{Room1, <<"Room 1">>},
                  {Room2, <<"Room 2">>},
                  {Room3, <<"Nice Room">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),

    {ok, Room4} = chat_room_manager:create_room(Server, <<"Room 4">>),
    {ok, Room5} = chat_room_manager:create_room(Server, <<"Room 5">>),
    {error, room_limit} = chat_room_manager:create_room(Server, <<"Room 6">>),
    ?assertEqual([{Room1, <<"Room 1">>},
                  {Room2, <<"Room 2">>},
                  {Room3, <<"Nice Room">>},
                  {Room4, <<"Room 4">>},
                  {Room5, <<"Room 5">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),

    {error, room_not_found} = chat_room_manager:remove_room(Server, make_ref()),

    ok = chat_room_manager:remove_room(Server, Room2),
    ?assertEqual([{Room1, <<"Room 1">>},
                  {Room3, <<"Nice Room">>},
                  {Room4, <<"Room 4">>},
                  {Room5, <<"Room 5">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),
    {error, room_not_found} = chat_room_manager:remove_room(Server, Room2),

    ok = chat_room_manager:remove_room(Server, Room4),
    ?assertEqual([{Room1, <<"Room 1">>},
                  {Room3, <<"Nice Room">>},
                  {Room5, <<"Room 5">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),

    ok = chat_room_manager:remove_room(Server, Room1),
    ?assertEqual([{Room3, <<"Nice Room">>},
                  {Room5, <<"Room 5">>}],
                 lists:sort(chat_room_manager:get_rooms(Server))),

    ok = chat_room_manager:remove_room(Server, Room3),
    ?assertEqual([{Room5, <<"Room 5">>}],
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


message_test() ->
    Server = chat_room_manager:start(),
    {ok, Room1} = chat_room_manager:create_room(Server, <<"Room 1">>),
    {ok, Room2} = chat_room_manager:create_room(Server, <<"Room 2">>),

    ?assertEqual({ok, []}, chat_room_manager:get_messages_history(Server, Room1)),
    ?assertEqual({ok, []}, chat_room_manager:get_messages_history(Server, Room2)),
    ?assertEqual({error, room_not_found}, chat_room_manager:get_messages_history(Server, make_ref())),

    ?assertEqual({error, user_not_in_room}, chat_room_manager:send_message(Server, Room1, <<"Bob">>, <<"Hello!">>)),
    ok = chat_room_manager:add_user(Server, Room1, <<"Bob">>),
    ok = chat_room_manager:send_message(Server, Room1, <<"Bob">>, <<"Hello!">>),

    ?assertEqual({error, user_not_in_room}, chat_room_manager:send_message(Server, Room1, <<"Bill">>, <<"Hi there!">>)),
    ok = chat_room_manager:add_user(Server, Room1, <<"Bill">>),
    ok = chat_room_manager:send_message(Server, Room1, <<"Bill">>, <<"Hi there!">>),

    ?assertEqual({ok, [{<<"Bill">>, <<"Hi there!">>}, {<<"Bob">>, <<"Hello!">>}]},
                 chat_room_manager:get_messages_history(Server, Room1)),
    ?assertEqual({ok, []}, chat_room_manager:get_messages_history(Server, Room2)),

    ok = chat_room_manager:add_user(Server, Room2, <<"Helen">>),
    ok = chat_room_manager:add_user(Server, Room2, <<"Kate">>),
    ok = chat_room_manager:send_message(Server, Room2, <<"Helen">>, <<"Hi!">>),
    ?assertEqual({ok, [{<<"Helen">>, <<"Hi!">>}]},
                 chat_room_manager:get_messages_history(Server, Room2)),
    ok = chat_room_manager:send_message(Server, Room2, <<"Kate">>, <<"ok">>),
    ?assertEqual({ok, [{<<"Kate">>, <<"ok">>}, {<<"Helen">>, <<"Hi!">>}]},
                 chat_room_manager:get_messages_history(Server, Room2)),

    Server ! stop,
    ok.
