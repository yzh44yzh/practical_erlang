-module(chat_room_manager_test).

-include_lib("eunit/include/eunit.hrl").


chat_user_test() ->
    {ok, Pid1} = chat_user:start_link(),
    {ok, Pid2} = chat_user:start_link(),
    ?assertEqual([], chat_user:get_messages(Pid1)),
    ?assertEqual([], chat_user:get_messages(Pid2)),

    chat_user:add_message(Pid1, <<"Bob">>, <<"Hello!">>),
    chat_user:add_message(Pid1, <<"Bill">>, <<"Hi there!">>),
    ?assertEqual([{<<"Bob">>, <<"Hello!">>}, {<<"Bill">>, <<"Hi there!">>}],
                 chat_user:get_messages(Pid1)),
    ?assertEqual([], chat_user:get_messages(Pid2)),

    chat_user:add_message(Pid2, <<"Helen">>, <<":D">>),
    chat_user:add_message(Pid2, <<"Kate">>, <<"awesome!">>),
    chat_user:add_message(Pid1, <<"John">>, <<":) :) :)">>),
    ?assertEqual([{<<"Bob">>, <<"Hello!">>}, {<<"Bill">>, <<"Hi there!">>}, {<<"John">>, <<":) :) :)">>}],
                 chat_user:get_messages(Pid1)),
    ?assertEqual([{<<"Helen">>, <<":D">>}, {<<"Kate">>, <<"awesome!">>}],
                 chat_user:get_messages(Pid2)),
    ok.


chat_room_test() ->
    {ok, RoomPid} = chat_room:start_link(),
    {ok, UserPid1} = chat_user:start_link(),
    {ok, UserPid2} = chat_user:start_link(),

    chat_room:add_user(RoomPid, <<"Bob">>, UserPid1),
    ?assertEqual([{<<"Bob">>, UserPid1}], chat_room:get_users(RoomPid)),
    chat_room:add_user(RoomPid, <<"Bill">>, UserPid2),
    ?assertEqual([{<<"Bill">>, UserPid2}, {<<"Bob">>, UserPid1}], lists:sort(chat_room:get_users(RoomPid))),

    chat_room:add_message(RoomPid, <<"Bob">>, <<"Hello">>),
    chat_room:add_message(RoomPid, <<"Bill">>, <<"Hi">>),

    Messages = [{<<"Bob">>, <<"Hello">>}, {<<"Bill">>, <<"Hi">>}],
    ?assertEqual(Messages, chat_room:get_history(RoomPid)),
    ?assertEqual(Messages, chat_user:get_messages(UserPid1)),
    ?assertEqual(Messages, chat_user:get_messages(UserPid2)),

    ?assertEqual(ok, chat_room:remove_user(RoomPid, UserPid1)),
    ?assertEqual({error, user_not_found}, chat_room:remove_user(RoomPid, UserPid1)),
    ?assertEqual(ok, chat_room:remove_user(RoomPid, UserPid2)),
    ?assertEqual({error, user_not_found}, chat_room:remove_user(RoomPid, UserPid2)),

    ok.


chat_room_manager_test() ->
    {ok, _} = chat_room_manager:start_link(),
    ?assertEqual([], chat_room_manager:get_rooms()),

    Room1 = chat_room_manager:create_room(<<"room 1">>),
    ?assertEqual([Room1], chat_room_manager:get_rooms()),

    Room2 = chat_room_manager:create_room(<<"room 2">>),
    ?assertEqual([Room1, Room2], lists:sort(chat_room_manager:get_rooms())),

    {<<"room 1">>, RoomPid1} = Room1,
    {<<"room 2">>, RoomPid2} = Room2,
    ?assertEqual({error, room_not_found}, chat_room_manager:get_users(self())),
    ?assertEqual({ok, []}, chat_room_manager:get_users(RoomPid1)),
    ?assertEqual({ok, []}, chat_room_manager:get_users(RoomPid2)),

    {ok, UserPid1} = chat_user:start_link(),
    {ok, UserPid2} = chat_user:start_link(),
    {ok, UserPid3} = chat_user:start_link(),
    ok = chat_room_manager:add_user(RoomPid1, <<"Bob">>, UserPid1),
    ?assertEqual({error, room_not_found}, chat_room_manager:add_user(self(), <<"Bob">>, UserPid1)),
    ok = chat_room_manager:add_user(RoomPid1, <<"Bill">>, UserPid2),
    ok = chat_room_manager:add_user(RoomPid2, <<"Helen">>, UserPid3),

    {ok, Users1} = chat_room_manager:get_users(RoomPid1),
    ?assertEqual([{<<"Bill">>, UserPid2}, {<<"Bob">>, UserPid1}], lists:sort(Users1)),
    {ok, Users2} = chat_room_manager:get_users(RoomPid2),
    ?assertEqual([{<<"Helen">>, UserPid3}], Users2),

    ok = chat_room_manager:remove_user(RoomPid1, UserPid1),
    ?assertEqual({error, user_not_found}, chat_room_manager:remove_user(RoomPid1, UserPid1)),
    ?assertEqual({error, room_not_found}, chat_room_manager:remove_user(self(), UserPid1)),
    ?assertEqual({ok, [{<<"Bill">>, UserPid2}]}, chat_room_manager:get_users(RoomPid1)),
    ok = chat_room_manager:remove_user(RoomPid1, UserPid2),
    ?assertEqual({ok, []}, chat_room_manager:get_users(RoomPid1)),

    ?assertEqual({error, room_not_found}, chat_room_manager:send_message(self(), <<"Bob">>, <<"Hello">>)),
    ?assertEqual(ok, chat_room_manager:send_message(RoomPid2, <<"Bob">>, <<"Hello">>)),
    ?assertEqual(ok, chat_room_manager:send_message(RoomPid2, <<"Helen">>, <<"Hi">>)),

    ?assertEqual({error, room_not_found}, chat_room_manager:get_history(self())),
    ?assertEqual({ok, [{<<"Bob">>, <<"Hello">>}, {<<"Helen">>, <<"Hi">>}]},
                 chat_room_manager:get_history(RoomPid2)),
    ?assertEqual([{<<"Bob">>, <<"Hello">>}, {<<"Helen">>, <<"Hi">>}],
                 chat_room:get_history(RoomPid2)),
    ?assertEqual([{<<"Bob">>, <<"Hello">>}, {<<"Helen">>, <<"Hi">>}],
                 chat_user:get_messages(UserPid3)),

    ok.
