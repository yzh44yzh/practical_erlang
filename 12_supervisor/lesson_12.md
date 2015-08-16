TODO: Клонировать erl-proj-tpl и проверить:
- Что, terminate вызывается, только если у воркера стоит trap_exit=true?
  Очень странно. Надо проверить.
- убедиться, что версия для эрланг 18 нормально работает

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

Запуск supervisor похож на запуск gen_server.
Вот картинка, аналогичная той, что мы видели в 10-м уроке:

![supervision_tree](http://yzh44yzh.github.io/img/practical_erlang/supervisor_init.png)

Напомню, что два левых квадрата (верхний и нижний), соответствуют
нашему модулю.  Два правых квадрата соответствуют коду OTP. Два
верхних квадрата выполняются в потоке родителя, два нижних квадрата
выполняются в потоке потомка.

Начинаем с функции **start\_link/0**:

```erlang
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
```

Здесь мы просим supervisor запустить новый поток.

Первый аргумент **{local, ?MODULE}** -- это имя, под которым нужно
зарегистрировать поток. Есть вариант **supervisor:start_link/2** на случай,
если мы не хотим регистрировать поток.

Второй аргумент **?MODULE** -- это имя модуля, callback-функции
которого будет вызывать supervisor.

Третий аргумент -- это набор параметров, которые нужны при
инициализации.

Дальше происходит некая магия в недрах OTP, в результате
которой создается дочерний поток, и вызывается callback **init/1**.

Из **init/1** нужно вернуть структуру данных, содержащую всю
необходимую информацию для работы супервизора.


## Настройка супервизора

Разберем подробнее:
```erlang
{ok, {SupervisorSpecification, ChildSpecifications}}
```

Нам нужно описать спецификацию самого супервизора, и дочерних
процессов, за которыми он будет наблюдать.

Спецификация супервизора -- это кортеж из трех значений:
```erlang
{RestartStrategy, Intensity, Period}
```

Restartstrategy описывает политику перезапуска дочерних потоков.
Есть 4 варианта стратегии:

**one_for_one** -- при падении одного потока перезапускается только
этот поток, остальные продолжают работать.

**one_for_all** -- при падении одного потока перезапускаются все
дочерние потоки.

**rest_for_one** -- промежуточный вариант между двумя первыми
стратегиями. Суть в том, что изначально потоки запущены один за одним,
в определенной последовательности. И при падении одного потока,
перезапускается он, и те потоки, которые были запущены позже него. Те,
которые были запущены раньше, продолжают работать.

**simple_one_for_one** -- это особый вариант, будет рассмотрен ниже.

Многие проблемы можно решить рестартом, но не все. Супервизор должен
как-то справляться с ситуацией, когда рестарт не помогает.  Для этого
есть еще две настройки: **Intensity** -- максимальное количество
рестартов, и **Period** -- за промежуток времени.

Например, если Intensity = 10, а Period = 1000, это значит, что
разрешено не более 10 рестартов за 1000 милисекунд. Если за это время
поток падает 11-й раз, то супервизор понимает, что он не может
справится с ситуацией.  И тогда супервизор завершается сам. И проблему
пытается решить его родитель -- супервизор уровнем выше.

В 18-й версии эрланг вместо кортежа:
```erlang
{RestartStrategy, Intensity, Period}
```
используется map:

```erlang
 #{strategy => strategy(),
   intensity => non_neg_integer(),
   period => pos_integer()}
```

Но и кортеж поддерживается для обратной совместимости.


### child specifications

Теперь разберем, как описываются дочерние потоки.
Каждый из них описывается кортежем из 6-ти элементов:

```erlang
{ChildId, Start, Restart, Shutdown, Type, Modules}.
```

**ChildId** -- идентификатор потока. Тут может быть любое значение.
Супервизор не использует Pid дочернего потока, потому что Pid будет
меняться при рестарте.


**Start** -- кортеж {Module, Function, Args}, описывает, с какой
функции стартует новый поток.


**Restart** -- атом, указывающий необходимость рестарта дочернего потока.
Возможны 3 варианта:
- permanent -- поток нужно рестартовать всегда.
- transient -- поток нужно рестартовать, если он завершился аварийно. При нормальном завершении рестартовать не нужно.
- temporary -- поток не нужно рестартовать.


**Shutdown** -- определяет, сколько времени супервизор дает дочернему
потоку на нормальное завершение работы.

Когда супервизор хочет остановить дочерний поток, он шлет сигнал
shutdown, и ждет заданное время.  Если за это время дочерний поток не
завершился, супервизор останавливает его сигналом kill.

Shutdown может быть указан как время в милисекунах, либо атомами:
- brutal_kill -- не давать время, завершать принудительно сразу же.
- infinity -- не ограничивать время, пусть дочерний поток завершается сколько, сколько ему нужно.

Обычно для worker-потоков указывают время в милисекундах, а для supervisor-потоков указывают infinity.


**Type** -- тип дочернего потока. Может быть либо worker, либо supervisor.


**Modules** -- модули, в которых будет выполнятся очерний поток. Обычно это один модуль,
и он совпадает с указанным в коржете Start.

Пример child specitication:
```erlang
{some_worker,
 {some_worker, start_link, []},
 permanent,
 2000,
 worker,
 [some_worker]},
```

В 18-й версии эрланг используется map:
```erlang
 #{id => some_worker
   start => {some_worker, start_link, []},
   restart => permanent,
   shutdown => 2000,
   type => worker,
   modules => [some_worker]
  }
```

Пример функции init:

```erlang
init(_Args) ->
    RestartStrategy = one_for_one, % one_for_one | one_for_all | rest_for_one
    Intensity = 10, %% max restarts
    Period = 60, %% in period of time
    SupervisorSpecification = {RestartStrategy, Intensity, Period},

    Restart = permanent, % permanent | transient | temporary
    Shutdown = 2000, % milliseconds | brutal_kill | infinity

    ChildSpecifications =
        [
         {some_worker,
          {some_worker, start_link, []},
          Restart,
          Shutdown,
          worker,
          [some_worker]},
         {other_worker,
          {other_worker, start_link, []},
          Restart,
          Shutdown,
          worker,
          [other_worker]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
```

То же самое для 18-й версии эрланг:

```erlang
init(_Args) ->
    SupervisorSpecification = #{
        strategy => one_for_one, % one_for_one | one_for_all | rest_for_one
        intensity => 10
        period => 60},

    ChildSpecifications =
        [#{id => some_worker
           start => {some_worker, start_link, []},
           restart => permanent, % permanent | transient | temporary
           shutdown => 2000, % milliseconds | brutal_kill | infinity
           type => worker,
           modules => [some_worker]},
         #{id => other_worker
           start => {other_worker, start_link, []},
           restart => permanent,
           shutdown => 2000,
           type => worker,
           modules => [other_worker]}
        ],
    {ok, {SupervisorSpecification, ChildSpecifications}}.
```

С map это все выглядит понятнее и лаконичнее.


## Динамическое создание воркеров

TODO:

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
