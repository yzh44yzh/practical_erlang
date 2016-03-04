-module(my_crypt_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    application:ensure_all_started(my_crypt),
    ok.

application_start_test() ->
    setup(),
    Apps = application:which_applications(),
    Res0 = lists:keyfind(my_crypt, 1, Apps),
    ?assertMatch({my_crypt, _, _}, Res0),

    Res1 = application:get_env(my_crypt, crypt_key),
    ?assertMatch({ok, _}, Res1),
    Res2 = application:get_env(my_crypt, hash_size),
    ?assertMatch({ok, _}, Res2),
    ok.

encode_test() ->
    setup(),
    Bin1 = <<"Hello">>,
    Res1 = my_crypt:encode(Bin1),
    ?assertNotEqual(Res1,Bin1),
    ?assertEqual(byte_size(Bin1), byte_size(Res1)),
    ?assertEqual(Bin1, my_crypt:encode(Res1)),

    Bin2 = <<"Some string long enough to be longer than encode key">>,
    Res2 = my_crypt:encode(Bin2),
    ?assertNotEqual(Res2, Bin2),
    ?assertEqual(byte_size(Bin2), byte_size(Res2)),
    ?assertEqual(Bin2, my_crypt:encode(Res2)),
    ok.


get_set_key_test() ->
    setup(),
    Res = application:get_env(my_crypt, crypt_key),
    Key = my_crypt:get_key(),
    ?assertEqual({ok, Key}, Res),

    Key1 = <<"MyNewEncryptKey">>,
    my_crypt:set_key(Key1),
    ?assertEqual(Key1, my_crypt:get_key()),

    Key2 = <<"OtherEncryptKey">>,
    my_crypt:set_key(Key2),
    ?assertEqual(Key2, my_crypt:get_key()),
    ok.


hash_test() ->
    setup(),
    Bins = [
        <<"The Zen of Erlang">>,
        <<"http://ferd.ca/the-zen-of-erlang.html">>,
        <<"Let it crash?">>,
        <<"Blow it up">>,
        <<"Fight Fire with Fire">>,
        <<"Preemptive Scheduling">>,
        <<"Supervision Trees">>,
        <<"I should be able to sleep at night, right? Hopefully yes.">>
    ],
    lists:foldl(
        fun(Bin, Acc) ->
            Hash = my_crypt:hash(Bin),
            ?assertEqual(Hash, my_crypt:hash(Bin)), % same hash for same bin
            ?assertEqual(false, lists:member(Hash, Acc)), % hash not the same for prev bins
            [Hash | Acc]
        end, [], Bins),
    ok.


hash_size_test() ->
    setup(),
    Bin = <<"Hello World!">>,

    {ok, HashSize1} = application:get_env(my_crypt, hash_size),
    Hash1 = my_crypt:hash(Bin),
    ?assertEqual(HashSize1, byte_size(Hash1)),

    HashSize2 = 32,
    application:set_env(my_crypt, hash_size, HashSize2),
    Hash2 = my_crypt:hash(Bin),
    ?assertEqual(HashSize2, byte_size(Hash2)),

    HashSize3 = 128,
    application:set_env(my_crypt, hash_size, HashSize3),
    Hash3 = my_crypt:hash(Bin),
    ?assertEqual(HashSize3, byte_size(Hash3)),
    ok.
