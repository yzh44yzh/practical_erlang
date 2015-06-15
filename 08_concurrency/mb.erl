-module(mb).
-export([test/0]).

test() ->
    test_messages("test1, ящик пустой", []),

    test_messages("test2, одно сообщение, матчится",
                  [{msg, 1}]),

    test_messages("test3, одно сообщение, не матчится",
                  [msg1]),

    test_messages("test4, 3 сообщения, все матчатся",
                  [{msg, 1}, {msg, 2}, {msg, 3}]),

    test_messages("test5, 3 сообщения, все не матчатся",
                  [msg1, msg2, msg3]),

    test_messages("test6, 4 сообщения, часть матчится, часть не матчится",
                  [{msg, 1}, msg2, {msg, 3}, msg4]),

    test_messages("test7, 4 сообщения, часть матчится, часть не матчится",
                  [msg1, {msg, 2}, msg3, {msg, 4}]),

    ok.

test_messages(TestName, Messages) ->
    io:format("~n### ~ts~ntest_messages: ~p~n", [TestName, Messages]),
    flush(),
    [self() ! Msg || Msg <- Messages],

    io:format("call receive~n"),
    Res = receive
              {msg, M} -> {msg, M}
          after 100 -> timeout
          end,
    io:format("after receive got: ~p~n", [Res]),
    [{messages, Left}] = process_info(self(), [messages]),
    io:format("left in mailbox: ~p~n", [Left]),
    ok.

flush() ->
    receive
        _ -> flush()
    after 100 -> ok
    end.
