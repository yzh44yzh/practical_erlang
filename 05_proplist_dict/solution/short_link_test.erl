-module(short_link_test).

-include_lib("eunit/include/eunit.hrl").


%%% module API

create_short_test() ->
    State1 = short_link:init(),
    {Short1, State2} = short_link:create_short("http://hexlet.io", State1),
    {Short2, State3} = short_link:create_short("http://lenta.ru", State2),
    {Short3, State4} = short_link:create_short("http://ya.ru", State3),
    {Short4, State5} = short_link:create_short("http://facebook.com", State4),
    ?assertMatch({Short1, _}, short_link:create_short("http://hexlet.io", State5)),
    ?assertMatch({Short2, _}, short_link:create_short("http://lenta.ru", State5)),
    ?assertMatch({Short3, _}, short_link:create_short("http://ya.ru", State5)),
    ?assertMatch({Short4, _}, short_link:create_short("http://facebook.com", State5)),
    ok.


get_long_test() ->
    State0 = short_link:init(),
    ?assertEqual({error, not_found}, short_link:get_long("foobar", State0)),

    {Short1, State1} = short_link:create_short("http://hexlet.io", State0),
    ?assertEqual({ok, "http://hexlet.io"}, short_link:get_long(Short1, State1)),

    {Short2, State2} = short_link:create_short("http://lenta.ru", State1),
    ?assertEqual({ok, "http://lenta.ru"}, short_link:get_long(Short2, State2)),

    {Short3, State3} = short_link:create_short("http://ya.ru", State2),
    ?assertEqual({ok, "http://ya.ru"}, short_link:get_long(Short3, State3)),

    {Short4, State4} = short_link:create_short("http://facebook.com", State3),
    ?assertEqual({ok, "http://facebook.com"}, short_link:get_long(Short4, State4)),

    ?assertEqual({ok, "http://hexlet.io"}, short_link:get_long(Short1, State4)),
    ?assertEqual({ok, "http://lenta.ru"}, short_link:get_long(Short2, State4)),
    ?assertEqual({ok, "http://ya.ru"}, short_link:get_long(Short3, State4)),
    ?assertEqual({ok, "http://facebook.com"}, short_link:get_long(Short4, State4)),
    ?assertEqual({error, not_found}, short_link:get_long("bla-bla-bla", State4)),

    ok.
