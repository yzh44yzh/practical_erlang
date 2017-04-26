# Functional Programming in Erlang
https://www.futurelearn.com/courses/functional-programming-erlang/

They were wanting to build
complex combinations of hardware and software
that constituted telecom switchest.

So what they were doing was building systems
that existed in a highly concurrent environment.
Hundreds, thousands, tens of thousands, hundreds of thousands of calls might be handled simultaneously by one of these boxes

they needed lightweight fine-grained concurrency.

And that is because side effects and lazy evaluation fit very badly together. Laziness forces you to be pure. That's because you don't know when particular parts of an expression are going to be evaluated. So you don't know when that side effect, if it was there, would happen.

================

хороший пример pattern matching:
% exclusive OR
exOr(true,false) -> true;
exOr(false,true) -> true;
exOr(_,_) -> false.

рефакторинг:
exOr(X,X) -> false;
exOr(_,_) -> true.

еще вариант:
exOr(true, X) -> not X;
exOr(false, X) -> X.


================

You can't put general user-defined functions in guards.
The designers of Erlang didn't want pattern matching not to terminate. They would like pattern matching always to terminate.

================

перед тем, как давать рекурсию с аккумуляторами, стоит сперва показать direct recursion

sum([]) -> 0;
sum([H|T]) -> H + sum(T).


================

nub -- удаление дубликатов из списка -- хороший учебный пример

с сохранением первого элемента
nub([]) -> []
nub([H | T]) ->
    [H | nub(lists:delete(H, T))].

с сохранением последнего элемента
nub([]) -> []
nub([H | T]) ->
    case lists:member(H, T) of
        true -> nub(T);
        false -> [H | nub(T)]
    end.

================

palindrom с игнорированием регистра и пунктуации -- тоже хорошее упражнение.

palindrom("Madam I\'m Adam.").

пригодится filtermap, чтобы в один проход и отфильтровать лишние элементы, и преобразовать в lower case

Strategy is to solve a simplified problem and then either adapt that
simplified solution to the simplified problem for the larger one, or
to reuse it as a component of the overall solution.

================

permutations -- хорошее упражнение )

perms([1,2,3]) = [[1,2,3],[2,3,1],[3,1,2],[2,1,3],[1,3,2],[3,2,1]]


================
Merge sort
Quicksort
Insertion sort

тоже хорошее упражнение


================
HOF

Functions as arguments: map, filter, fold
Functions as results: curried functions
Functions as arguments and results: compose(F1,F2)->F3, iterage(F, NumTimes) -> F2

The other thing I wanted to talk about was the iteration function. And
let's see, what does iteration do? Well, iterate takes a number and
then it returns a function whose role is to take a function and
compose it with itself N times. So iterate zero will take any function
you like and return the identity function. Because doing F zero times
means doing nothing to the argument. Otherwise, what do we do? We want
to apply F N times. One thing we can do is use ordinary recursion of
the numbers and iterate F (N-1) times, and then apply it one more
time. Now, one way you can do that is to compose together the iterate
and F.

================

Как отличить map/filter/fold

Map сохраняет длинну списка, filter уменьшает список
map и filter работают только с текущим элементом, fold имеет состояние (где могут быть другие элементы).

Например:
удалить дубликаты из строки -- fold, т.к. зависит не только от текущего элемента.

================

FRANCESCO CESARINI:
три трудности в изучении эрланга:
- pattern matching
- recursion, tail recursion
- thinking concurrently

FRANCESCO CESARINI: I found Haskell and Erlang, at least when taught
in university, is very complementary of each other as well. And my
opinion is that any student taking a computer science degree should
learn both.

SIMON THOMPSON:
teaching Erlang, you're teaching both new ideas in functional programming and new ideas in concurrency

One thing I mentioned was the type-driven development thing. It would
be nice if, for example, Dialyzer were better integrated with Erlang.

FRANCESCO CESARINI: we use Erlang as the glue to handle the whole
orchestration. And then we use Python, C, Julia. It is actually
language intended to act as a hub towards other languages.

JOE ARMSTRONG: Because it's funny, when I teach it, the students start
asking, they say, where's the debugger? And I say, well, there isn't a
debugger. Or there is a debugger, but no Erlang programmers use
it. And they say, well, why's that? And I say, well, because you don't
have variables that are changing under your feet that it's difficult
to reason about it. Once they've got a value, they've got the value
forever, and they acquire it in one place, and therefore, you don't
need to track it.

FRANCESCO CESARINI: Massive concurrency. Orchestration. The fact that
your systems behave in a predictable way under extreme heavy load,
extreme heavy load under an extended period, or under extremely high
peak load as well. You won't get any degradation of throughput in your
system.


# Concurrent Programming in Erlang
https://www.futurelearn.com/courses/concurrent-programming-erlang/


actor model
Carl Hewitt
- everything is an actor
- actors communicate by messages
- actors can create other actors
- change behaviour according to messages

================


отправить сообщение по несуществующему pid -- это ок
отправить сообщение по несуществующему имени -- это ошибка

```
2> pid(0,200,0) ! hello.
hello
3> some ! hello.
** exception error: bad argument
     in operator  !/2
        called as some ! hello
```

================

rpc(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    receive
        {Ref, Response} -> Response
    end.

можно разбить на две части:

promise(Pid, Request) ->
    Ref = make_ref(),
    Pid ! {self(), Ref, Request},
    Ref.

yield(Ref) ->
    receive
        {Ref, Response} -> Response
    end.

это похоже на Feature в других языках.

================

Parallel map

pmap(ListOfFuns) ->
    Pids = [do(self, Fun) || Fun <- ListOfFuns],
    [receive {Pid, Val} -> Val end || Pid <- Pids].

do(Parent, Fun) ->
    spawn(fun() ->
        Parent ! {self(), Fun()}
    end).


================

We make sure that a second computer observes the first computer, and if the first computer crashes, it's detected by the second computer, and we must arrange that the second computer can take over whatever the first computer was going to do. Of course, we can we can do it in a more symmetric sort of way, that we can have pairs of computers that observe each other. So if the first computer crashes, the second computer will observe that error and take corrective action, and if the second computer crashes, then the first computer will take over.

Это объясняет, почему 2х сторонний link, а не односторонний monitor.

link можно сделать между процессами на разных нодах.


================

Супервизор использует link, а не монитор.
Для того чтобы, если супервизор упадет, то упадут все его воркеры. А не останутся бесхозными зомби )

Mike came up with the idea of links, yes. And it was-- actually, it
was to solve a different problem. That was the problem of zombie
processes when things go wrong.

There are two mechanisms-- links and monitors. And they're actually used for different purposes.

Monitors should be used for an asymmetric.
You don't want to crash the server because it will affect all the clients.
But if the server crashes, of course all the clients you do want to crash.

But if you've got three or four processes linked together without a client,
you know, in a peer to peer system, then you may want to crash that little graph of processes.

================

Work Stealing

Каждое ядро имеет свой планировщик и run queue.
Если очередь пуста, планировщик может забрать поток у другого планировщика (ядра).

For a long-lived process, it can move between cores a number of times, and we've observed that in the Percept2 tool.
We can see that longer processes will move perhaps 20 times during our execution.
That will typically be in a rather underloaded system where there's quite a lot of work stealing going on.

================

A node started with the short name *ant* on a host *baz*
will be identified by the atom 'ant@baz'

Short names communicate on the local network.
Long named communicate globally, and it's possible to use DNS to resolve names.
But in a single system we can either use all short names or all long names, we can't mix the naming conventions up.

And so, for example, we could have a three node system here, ant and
bee are two nodes that live on the host baz, and we also have a node
called ant that lives on a different host. That's perfectly OK because
one will be ant@baz and one will be ant@bar, but you can't use the
same name more than once on a single host.
