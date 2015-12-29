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
