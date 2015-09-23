# Способы обработки ошибок. Let it crash.

## Defensive Programming vs Let it crash

Когда вся программа выполняется в одном потоке, аварийное завершение
этого потока означает аварийное завершение программы. И если это
случилось в месте, где явно не предусмотрена обработка ошибок, то
завершившаяся программа оставляет минимум информации для диагностики
проблемы.

Поэтому программисты стараются предусмотреть обработку всех возможных
ошибок во всех возможных местах. Такой стиль программирования
называется **Defensive Programming**. И он не редко он приводит к
тому, что в программе больше кода для обработки ошибок, чем кода,
выполняющего основную задачу. Конечно, это усложняет и написание кода,
и поддержку.

Эрланг предлагает другой подход: реализовать только основную логику
(**happy path**) и не писать код для обработки ошибок. Благодаря
многопоточности и разделению потоков на рабочие и супервизоры,
любая ошибка всегда будет замечена и записана в лог. А система
в целом продолжит работу. Этот подход называется **Let it crash**.

Между тем, все инструменты для Defensive Programming есть.
И полностью от этого подхода никто не отказывается.
На практике каждый разработчик ищет свой баланс между
Defensive Programming и Let it crash.

В этом уроке рассмотрим, как оба подхода применяются в эрланг.


## Типы данных Maybe и Error

Когда на 5-м уроке мы рассматривали Key-Value типы данных, мы
заметили, что почти все они имеют по два варианта функций,
возвращающих или обновляющих значение по ключу.

TODO примеры

Функции по-разному ведут себя в случае, когда ключ не
найден. Одни функции в этом случае бросают исключение, другие
возвращают специальное значение.

Вот эти специальные значения мы сейчас и рассмотрим. Только придется
обратиться к другим языкам программирования -- к Haskell и OCaml.
Там эта концепция реализована четко, в отличие от эрланг.




Функция **dict:fetch/2** возвращает значение, если ключ найден. Или бросает
исключение, если такого ключа нет.  Функция **dict:find/2** возвращает
кортеж {ok, Val}, если ключ найден, или атом error, если ключа нет.

И тут примеры из модулей dict, proplists, maps, ets и т.д.


```
{ok, Res} = do_something(Arg),
```

```
case do_something(Arg) of
    {ok, Res} -> code;
    {error, Reason} -> code
end.
```


the two basic approaches for reporting errors in OCaml: error-
aware return types and exceptions.

The best way in OCaml to signal an error is to include that error in your return value.

монада Maybe (Option)
{ok, Value} | None

монада Error (Result) (TODO уточнить название в Haskell)
{ok, Value} | {error, Reason}



Подчеркнуть бардак -- самые разные варианты maybe/error
к сожалению нет такого, чтобы везде одинаково

Including errors in the return values of your functions requires the caller to handle the
error explicitly, allowing the caller to make the choice of whether to recover from the
error or propagate it onward.


Самые частые исключения:

- no function/case clause matching
- badmatch
- bad argument
- undefined function
- badarith


## try..catch

Exceptions in OCaml are not that different from exceptions in many other languages,
like Java, C#, and Python. Exceptions are a way to terminate a computation and report
an error, while providing a mechanism to catch and handle (and possibly recover from)
exceptions that are triggered by subcomputations.

three kinds of exceptions in Erlang: throws, errors and exits

throw(Exception) -- подразумевается перехват и восстановление работы
erlang:error(Reason) -- восстановление не подразумевается, поток должен упасть
exit(Reason) -- системный, использовать обычно не нужно. Это логика на уровне супервизора.

A throw is a class of exceptions used for cases that the programmer can be expected to handle. In comparison with exits and errors, they don't really carry any 'crash that process!' intent behind them, but rather control flow.

erlang:error/1 returns a stack trace and exit/1 doesn't.

```
try
    Expression1,
    Expression2,
    Expression3
catch
    TypeOfError:ExceptionPattern1 -> Expression3;
    TypeOfError:ExceptionPattern2 -> Expression4
end.
```

```
try Expression of
    SuccessfulPattern1 [Guards] -> Expression1;
    SuccessfulPattern2 [Guards] -> Expression2
catch
    TypeOfError:ExceptionPattern1 -> Expression3;
    TypeOfError:ExceptionPattern2 -> Expression4
end.
```

как выглядит матчинг и стек трейс во всех этих случаях

Для чего применять:
- нужна обработка отличная от дефолтной
- нужны много точек выхода из функции
  (например валидация входящих данных)
- более понятное сообщение об ошибке
  не эрланговский стэк-трейс, а сообщение в терминах бизнес-логики, и содержащее контекст (данные)

It is important to know that the protected part of an exception can't be tail recursive.
The VM must always keep a reference there in case there's an exception popping up.


## stack trace

erlang:get_stacktrace().

Get the call stack back-trace (stacktrace) of the last exception in
the calling process as a list of {Module,Function,Arity,Location}
tuples.

The stack trace contains information about where the current function (which
crashed) would have returned to had it succeeded. The individual tuples in
the stack trace are of the form {Mod,Func,Arity,Info} . Mod , Func , and Arity denote a
function, and Info contains the filename and line number of the item in the
stack trace.

TODO: пример какой-нибудь



## catch all

клозы функций

клозы case и if

gen\_server:handle\_*

не нужен, если мы делаем let it crush
нужен, если мы кастомно обрабатываем ошибку

для gen_server:handle лучше сделать
чтобы правильно залогировать ошибку
иначе поток упадет, но будет мало информации для диагностики
TODO: посмотреть, какая тут будет информация


## Выбор способа обработки ошибок

функция/библиотека может:
- вернуть ok | error (Maybe or Result ok|error)
- бросить исключение
вызывающий код может
- матч на ok
- case на ok|error
- ловить исключение
- игнорировать исключение


Given that OCaml supports both exceptions and error-aware return types, how do you
choose between them? The key is to think about the trade-off between concision and
explicitness.

Exceptions are more concise because they allow you to defer the job of error handling
to some larger scope, and because they don’t clutter up your types. But this concision
comes at a cost: exceptions are all too easy to ignore. Error-aware return types, on the
other hand, are fully manifest in your type definitions, making the errors that your code
might generate explicit and impossible to ignore.

Это больше актуально для языков со статической типизацией -- Haskell, OCaml
В Erlang что угодно можно проигнорировать )

The maxim of “use ex‐
ceptions for exceptional conditions” applies. If an error occurs sufficiently rarely, then
throwing an exception is often the right behavior.

In short, for errors that are a foreseeable and ordinary part of the execution of your
production code and that are not omnipresent, error-aware return types are typically
the right solution.


## supervisor

в однопоточной программе краш потока -- это краш всей программы, поэтому
sequential languages have concentrated on the prevention of failure and an
emphasis on defensive programming.

Instead of handling an error in the process
where the error occurs, we let the process die and correct the error in some
other process.

Some studies proved that the main sources of downtime in large scale
software systems are intermittent or transient bugs (source). Then,
there's a principle that says that errors which corrupt data should
cause the faulty part of the system to die as fast as possible in
order to avoid propagating errors and bad data to the rest of the
system.  (stidues -- диссертация Джо Армстронга, и другие)

Это уже было в 12-м уроке про супервизор:

Существуют научные работы, которые доказывают, что значительная часть
ошибок в серверных системах вызваны временными условиями, и перегрузка
части системы в известное стабильное состояние позволяет с ними
справиться. Среди таких работ [докторская диссертация Джо Армстронга](http://www.sics.se/~joe/thesis/armstrong_thesis_2003.pdf),
одного из создателей эрланг.


## распределенность

the next problem you get is hardware failures.  to have your program
 running on more than one computer at once, something that was needed
 for scaling anyway

To build really fault-tolerant systems, we need more than one computer; after
all, the entire computer might crash. So, the idea of detecting failure and
resuming the computation elsewhere has to be extended to networked com-
puters.

TODO: у Фреда написано
распределенное приложение
takeover
failover

распределенный приложения -- отдельная большая тема
