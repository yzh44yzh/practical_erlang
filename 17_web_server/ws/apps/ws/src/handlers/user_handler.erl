-module(user_handler).

-export([init/2]).

init(Req0, State) ->
    Bindings = cowboy_req:bindings(Req0),
    UserId = maps:get(user_id, Bindings, <<"0">>),
    Info = get_user_info(UserId),

    Tpl = tpl_manager:get_template("user"),
    Body = bbmustache:render(Tpl, Info),

    Headers = #{
        <<"content-type">> => <<"text/html">>
    },

    Req = cowboy_req:reply(200, Headers, Body, Req0),
    {ok, Req, State}.


get_user_info(UserId) ->
    #{
        "user_id" => UserId,
        "username" => <<"Bob">>,
        "attributes" => [
            #{"attr_name" => "height", "attr_value" => 182},
            #{"attr_name" => "weight", "attr_value" => 70}
        ]
    }.