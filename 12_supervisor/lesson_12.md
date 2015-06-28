# supervisor

http://www.erlang.org/doc/man/supervisor.html

## Теория

мы рассмотрели link и monitor

обработка ошибок основывается на том, что один поток может быть связан с другим потоком
и если с другим потоком что-то случится, то получить событие и обработать эту ситуацию:
перезапустить поток в известном стабильном состоянии

Считается, что это позволяет пережить значительную часть ошибок. (На сей счет есть
научные работы, в т.ч. докторская диссертация Джо Армстронга). Значительная часть,
но не все, конечно. Поэтому нужны разные стратегии обработки ошибок.

дерево воркеров и супервизоров (картинка нужна)


## Настройка супервизора

структура child spec
{ok, {{RestartStrategy, MaxRestart, MaxTime},[ChildSpecs]}}.

    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one.

    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,

{ChildId, StartFunc, Restart, Shutdown, Type, Modules}.

**ChildId**
идентификатор потока. Pid не используется в этой роли, т.к. он будет меняться при рестарте.

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity
    Type = worker | supervisor %

**Type**
This will be important when upgrading applications with more advanced
OTP features, but you do not really need to care about this at the
moment

**Modules**
name of the callback module used by the child behavior.
важно для горячего обновления

TODO
Пример с парой воркеров и одним дочерним супервизором
наблюдать через observer

## Динамическое создание воркеров

Тут два варианта:
- вызовы start\_child/2, terminate\_child/2, restart\_child/2, delete\_child/2
- simple\_one\_for\_one стратегия

**terminate\_child/2**
The process, if there is one, is terminated and, unless it is a
temporary child, the child specification is kept by the
supervisor. The child process may later be restarted by the
supervisor.

**restart\_child/2**
restart a child process corresponding to the child specification
identified by Id. The child specification must exist and the
corresponding child process must not be running.

**delete\_child/2**
Tells the supervisor SupRef to delete the child specification
identified by Id. The corresponding child process must not be running,
use terminate_child/2 to terminate it.


### start\_child/2, delete\_child/2

Годится для небольшого количества детей

TODO пример кода и попробовать в консоли
наблюдать через observer


### simple\_one\_for\_one

Годится для большого количества детей

он умеет создавать воркеров только динамически, и только одинаковых
если кроме этого нужны и другие воркеры, то нужно 2 супервизора

a simple\_one\_for\_one supervisor just sits around there, and it knows
it can produce one kind of child only. Whenever you want a new one,
you ask for it and you get it

TODO пример кода и попробовать в консоли
наблюдать через observer
