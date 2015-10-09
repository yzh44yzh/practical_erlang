-module(chat_room_manager).

-export([start/0,
         create_room/2, remove_room/2, get_rooms/1,
         add_user/3, remove_user/3, get_users_list/2,
         send_message/4,  get_messages_history/2]).


start() ->
    ok.


create_room(Server, RoomName) ->
    ok.


remove_room(Server, RoomId) ->
    ok.


get_rooms(Server) ->
    ok.


add_user(Server, RoomId, UserName) ->
    ok.


remove_user(Server, RoomId, UserName) ->
    ok.


get_users_list(Server, RoomId) ->
    ok.


send_message(Server, RoomId, UserName, Message) ->
    ok.


get_messages_history(Server, RoomId) ->
    ok.
