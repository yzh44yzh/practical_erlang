-module(main_test).

-include_lib("eunit/include/eunit.hrl").


app_test() ->
    ok = application:start(mylib),
    {ok, [{application, mylib, Resource}]} = file:consult("ebin/mylib.app"),

    Version = proplists:get_value(vsn, Resource),
    ?assertEqual("0.1", Version),
    Modules = proplists:get_value(modules, Resource),
    ?assertEqual([mylib_app, mylib_sup, mylib_worker], lists:sort(Modules)),

    Env = proplists:get_value(env, Resource),
    MinVal = proplists:get_value(min_val, Env),
    ?assertEqual(2, MinVal),
    MaxVal = proplists:get_value(max_val, Env),
    ?assertEqual(10, MaxVal),
    ConnectionTimeout = proplists:get_value(connection_timeout, Env),
    ?assertEqual(10000, ConnectionTimeout),

    ?assertEqual(Version, mylib_worker:get_version()),
    ?assertEqual(Modules, mylib_worker:get_modules()),
    ?assertEqual(MinVal, mylib_worker:get_min_val()),
    ?assertEqual(ConnectionTimeout, mylib_worker:get_connection_timeout()),

    AllApps = mylib_worker:all_apps(),
    ?assertMatch(#{kernel := #{description := _, version := _},
                   stdlib := #{description := _, version := _},
                   mylib := #{description := _, version := "0.1"}
                  },
                 AllApps),
    ok.
