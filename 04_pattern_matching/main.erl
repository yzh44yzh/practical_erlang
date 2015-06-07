-module(main).

-export([is_user_owner_of_room/2, area/1, test/1]).

-record(room, {room_id, owner}).

is_user_owner_of_room(UserId, RoomId) ->
    case rooms:find_room(RoomId) of
        {ok, #room{owner = UserId}} -> true;
        _ -> false
    end.


area({rect, Width, Height}) -> Width * Height;
area({square, Size}) -> Size * Size;
area({circle, Radius}) -> math:pi() * Radius * Radius.


check_user({user, _, Gender, Age})
  when Gender =:= female, Age < 14;
       Gender =:= male, Age < 14
       -> child;
check_user({user, _, Gender, Age})
  when Gender =:= male, Age >= 21;
       Gender =:= male, Age >= 21
       -> adult;


check_user({user, _, Gender, Age})
  when (Gender =:= female andalso Age < 14) orelse
       (Gender =:= male andalso Age < 14)
       -> child;
check_user({user, _, Gender, Age})
  when (Gender =:= male andalso Age >= 21) orelse
       (Gender =:= male andalso Age >= 21)
       -> adult.


test(Val) when (5 / Val > 2) -> Val.


stop_room(UserId, RoomId) ->
    case rooms:find_room(RoomId) of
        {ok, #room{owner = UserId}} ->
            case rooms:stop(RoomId) of
                ok -> room_stopped;
                {error, Reason} -> {error, Reason}
            end;
        {ok, #room{}} -> {error, not_room_owner};
        {error, not_found} -> {error, room_not_found}
    end.
