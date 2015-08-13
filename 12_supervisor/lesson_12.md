# Supervisor

## Немного теории

На прошлом уроке мы выяснили, что стратегия эрланг -- разделить потоки
на рабочие (worker) и системные (supervisor), и поручить системным
потокам обрабатывать падения рабочих потоков.

Существуют научные работы, которые доказывают, что значительная часть
ошибок в серверных системах вызваны временными условиями, и перегрузка
части системы в известное стабильное состояние позволяет с ними
справиться. Среди таких работ [докторская диссертация](http://www.sics.se/~joe/thesis/armstrong_thesis_2003.pdf)
Джо Армстронга, одного из создателей эрланг.

Систему на эрланг рекомендуется строить так, чтобы любой поток был под
наблюдением супервизора, а сами супервизоры были организованы в
дерево.

![supervision_tree](http://yzh44yzh.github.io/img/practical_erlang/supervision_tree.png)

На картинке нарисовано такое дерево. Узлы в нем -- супервизоры, а
листья -- рабочие процессы.  Падение любого потока и любой части
системы не останется незамеченным.

Дерево супервизоров разворачивается на старте системы. Каждый
супервизор отвечает за то, чтобы запустить своих потомков, наблюдать
за их состоянием, рестартовать и корректно завершать, если надо.

В эрланг есть [стандартная реализация супервизора](http://www.erlang.org/doc/man/supervisor.html).
Она работает аналогично gen_server: вы должны написать кастомный
модуль, реализующий поведение supervisor, куда входит одна функция
обратного вызова **init/1**.  С одной стороны это просто -- всего один
callback. С другой стороны **init** должен вернуть довольно не простую
структуру данных, с которой нужно как следует разобраться.


## Запуск супервизора

![supervision_tree](http://yzh44yzh.github.io/img/practical_erlang/supervisor_init.png)

TODO

Схема инициализации, как была для gen_server

supervisor:start_link(ServerName, CallBackModule, Arguments)
supervisor:start_link(CallBackModule, Arguments)

ServerName
Is the name to be registered for the supervisor, and is a tuple of the format {local,
Name} or {global, Name} .

CallbackModule
Is the name of the module in which the init/1 callback function is placed.

Arguments
Is a valid Erlang term that is passed to the init/1 callback function when it is called.

The start
and start_link functions will spawn a new process that calls the init/1 callback function.



## Настройка супервизора

{ok, {SupervisorSpecification, ChildSpecificationList}}
The supervisor specification is a tuple containing information on how to handle process
crashes and restarts. The child specification list specifies which children the supervisor
has to start and monitor, together with information on how to terminate and restart
them.

структура child spec
{ok, {{RestartStrategy, MaxRestart, MaxTime},[ChildSpecs]}}.

    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one | simple_one_for_one.

    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,

maximum restart intensity: intensity, period

What will happen if your process gets into a cyclic restart? It crashes and is restarted,
only to come across the same corrupted data, and as a result, it crashes again. This can’t
go on forever! This is where AllowedRestarts comes in, by specifying the maximum
number of abnormal terminations the supervisor is allowed to handle in MaxSeconds
seconds. If more abnormal terminations occur than are allowed, it is assumed that the
supervisor has not been able to resolve the problem, and it terminates. The supervisor’s
supervisor receives the exit signal and, based on its configuration, decides how to
proceed.

Finding reasonable values for AllowedRestarts and MaxSeconds is not easy, as they will
be application-dependent. In production, we’ve used anything from ten restarts per
second to one per hour. Your choice will have to depend on what your child processes
do, how many of them you expect the supervisor to monitor, and how you’ve set up
your supervision strategy.

В 18 эрланг используется map
    #{strategy => strategy(),
      intensity => non_neg_integer(),
      period => pos_integer()}
вместо котрежа
    {RestartStrategy, MaxRestart, MaxTime}

### child specifications

{ChildId, StartFunc, Restart, Shutdown, Type, Modules}.

**ChildId**
идентификатор потока. Pid не используется в этой роли, т.к. он будет меняться при рестарте.

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000,     % brutal_kill | int() >= 0 | infinity
    Type = worker | supervisor %

**Shutdown**

TODO:
Specifies how many milliseconds a behavior that is trapping exits is allowed to
execute in its terminate callback function after receiving the shutdown signal from
its supervisor, either because the supervisor has reached its maximum number of
allowed child restarts or because of a rest_for_one or one_for_all restart strategy.

TODO:
Вот этого я не понял. Что, terminate вызывается, только если у воркера стоит trap_exit=true?
Очень странно. Надо проверить.

If the child process has not terminated by this time, the supervisor will kill it un-
conditionally.

Shutdown will also take the atom infinity , a value which should
always be chosen if the process is a supervisor, or the atom brutal_kill , if the
process is to be killed unconditionally.

**Type**
This will be important when upgrading applications with more advanced
OTP features, but you do not really need to care about this at the
moment

**Modules**
name of the callback module used by the child behavior.
важно для горячего обновления

Is a list of the modules that implement the process. The release handler uses it to
determine which processes it should suspend during a software upgrade. As a rule
of thumb, always include the behavior callback module.

В 18 эрланг используется map
    #{id => child_id(),       % mandatory
     start => mfargs(),      % mandatory
     restart => restart(),   % optional
     shutdown => shutdown(), % optional
     type => worker(),       % optional
     modules => modules()}   % optional

TODO
Пример с парой воркеров и одним дочерним супервизором
наблюдать через observer

## Динамическое создание воркеров

Тут два варианта:
- вызовы start\_child/2, terminate\_child/2, restart\_child/2, delete\_child/2
- simple\_one\_for\_one стратегия

### start_child

**start_child**
**supervisor:start_child(SupervisorName, ChildSpec)

SupervisorName
Is either the process identifier of the supervisor or its registered name
ChildSpec
Is a single child specification tuple

**terminate\_child/2**

supervisor:terminate_child(SupervisorName, Id)
Id Is the unique child identifier defined in the ChildSpec

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


## stop

it does not export a stop function.
As supervisors are never meant to be stopped by anyone other than their
parent supervisors, this function was not implemented.

You can easily add your own stop function by including the following
code in your supervisor callback module. However, this will work only
if stop is called by the parent:

stop() -> exit(whereis(?MODULE), shutdown).

If your supervisor is not registered, use its pid.
