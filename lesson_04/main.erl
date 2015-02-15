-module(main).

-export([is_user_owner_of_room/2, area/1]).

-record(room, {room_id, owner}).

is_user_owner_of_room(UserId, RoomId) ->
    case rooms:find_room(RoomId) of
        {ok, #room{owner = UserId}} -> true;
        _ -> false
    end.


area({rect, Width, Height}) -> Width * Height;
area({square, Size}) -> Size * Size;
area({circle, Radius}) -> math:pi() * Radius * Radius.
