-module(try_json).

-export([init/0]).

init() ->
    {
      [{<<"doc">>, <<"list of users">>},
       {<<"version">>, 12},
       {<<"users">>,
        [
         {[{<<"id">>, 1},
           {<<"name">>, <<"Bob">>},
           {<<"gender">>, male},
           {<<"age">>, 22}]},
         {[{<<"id">>, 2},
           {<<"name">>, <<"Helen">>},
           {<<"gender">>, female},
           {<<"age">>, 21}]}
        ]}
     ]
    }.
