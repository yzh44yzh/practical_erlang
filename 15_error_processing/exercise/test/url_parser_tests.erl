-module(url_parser_tests).
-include_lib("eunit/include/eunit.hrl").

parse_test_() ->
    Tests = [
             {<<"https://erlangcentral.org/books/">>,
              {ok, #{protocol => <<"https">>,
                     domain => <<"erlangcentral.org">>,
                     path => [<<"books">>],
                     query => <<>>,
                     date => undefined
                    }}},
             %%
             {<<"https://www.youtube.com/user/ErlangSolutions/playlists">>,
              {ok, #{protocol => <<"https">>,
                     domain => <<"www.youtube.com">>,
                     path => [<<"user">>, <<"ErlangSolutions">>, <<"playlists">>],
                     query => <<>>,
                     date => undefined
                    }}},
             %%
             {<<"https://www.youtube.com/playlist?list=PLWbHc_FXPo2jN8cLhwLg7frCx6fJ-GPwM">>,
              {ok, #{protocol => <<"https">>,
                     domain => <<"www.youtube.com">>,
                     path => [<<"playlist">>],
                     query => <<"list=PLWbHc_FXPo2jN8cLhwLg7frCx6fJ-GPwM">>,
                     date => undefined
                    }}},
             %%
             {<<"http://mostlyerlang.com/2015/05/12/067-rebar-3">>,
              {ok, #{protocol => <<"http">>,
                     domain => <<"mostlyerlang.com">>,
                     path => [<<"2015">>, <<"05">>, <<"12">>, <<"067-rebar-3">>],
                     query => <<>>,
                     date => {2015, 5, 12}
                    }}},
             %%
             {<<"http://mostlyerlang.com/2015/05/21/interview-with-joe-armstrong/">>,
              {ok, #{protocol => <<"http">>,
                     domain => <<"mostlyerlang.com">>,
                     path => [<<"2015">>, <<"05">>, <<"21">>, <<"interview-with-joe-armstrong">>],
                     query => <<>>,
                     date => {2015, 5, 21}
                    }}},
             %%
             {<<"http://mostlyerlang.com/2015/05/07/interview-with-guido-van-rossum/">>,
              {ok, #{protocol => <<"http">>,
                     domain => <<"mostlyerlang.com">>,
                     path => [<<"2015">>, <<"05">>, <<"07">>, <<"interview-with-guido-van-rossum">>],
                     query => <<>>,
                     date => {2015, 5, 7}
                    }}},
             %%
             {<<"http://dieswaytoofast.blogspot.com.by/2012/12/erlang-binaries-and-garbage-collection.html">>,
              {ok, #{protocol => <<"http">>,
                     domain => <<"dieswaytoofast.blogspot.com.by">>,
                     path => [<<"2012">>, <<"12">>, <<"erlang-binaries-and-garbage-collection.html">>],
                     query => <<>>,
                     date => undefined
                    }}},
             %%
             {<<"http://www.erlangpatterns.org/patterns.html">>,
              {ok, #{protocol => <<"http">>,
                     domain => <<"www.erlangpatterns.org">>,
                     path => [<<"patterns.html">>],
                     query => <<>>,
                     date => undefined
                    }}},
             %%
             {<<"www.erlang.org">>,
              {error, invalid_protocol}},
             %%
             {<<"http://">>,
              {error, invalid_domain}},
             %%
             {<<"http://mostlyerlang.com/2015/45/07/interview-with-guido-van-rossum/">>,
              {ok, #{protocol => <<"http">>,
                     domain => <<"mostlyerlang.com">>,
                     path => [<<"2015">>, <<"45">>, <<"07">>, <<"interview-with-guido-van-rossum">>],
                     query => <<>>,
                     date => undefined
                    }}}
            ],
    {generator, fun() -> lists:map(
                           fun({Url, Res}) -> ?_assertEqual(Res, url_parser:parse(Url)) end, Tests)
                end}.
