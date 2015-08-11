- источники инфы:
  - Real World OCaml, где-то там было про обработку ошибок
    исключения vs option/result types
  - notes.org
  - erlang-school
  - официальные доки
  - Армстронг
  - Цезарини
  - Хеберт
  - erlang in anger
  - OTP in action
  - yzh44yzh.by


## Let it crash

http://learnyousomeerlang.com/errors-and-processes

The first writers of Erlang always kept in mind that failure is
common. You can try to prevent bugs all you want, but most of the time
some of them will still happen. In the eventuality bugs don't happen,
nothing can stop hardware failures all the time. The idea is thus to
find good ways to handle errors and problems rather than trying to
prevent them all.

In C
we are taught to write defensive code. Programs should check their arguments
and not crash. There is a very good reason for this in C: writing multiprocess
code is extremely difficult and most applications have only one process, so if
this process crashes the entire application, you’re in big trouble. Unfortunately,
this leads to large quantities of error checking code, which is intertwined with
the non-error-checking code.

In Erlang we do exactly the opposite. We build our applications in two parts:
a part that solves the problem and a part that corrects errors if they have
occurred.

The part that solves the problem is written with as little defensive code as
possible; we assume that all arguments to functions are correct and the
programs will execute without errors.

The part that corrects errors is often generic, so the same error-correcting
code can be used for many different applications.

We write code that solves problems
and code that fixes problems, but the two are not intertwined.

```
{ok, Res} = do_something(Arg),
```

```
case do_something(Arg) of
    {ok, Res} -> code;
    {error, Reason} -> code
end.
```

Самые частые исключения:

- no function/case clause matching
- badmatch
- bad argument
- undefined function
- badarith

как читать стэктрейс?


## try..catch

three kinds of exceptions in Erlang: throws, errors and exits

throw(Exception)
erlang:error(Reason)
exit(Reason)

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

erlang:get_stacktrace().

Get the call stack back-trace (stacktrace) of the last exception in
the calling process as a list of {Module,Function,Arity,Location}
tuples.

Для чего применять:
- много точек выхода с разными ошибками
  (валидация входящих данных)
- более понятное сообщение об ошибке
  не эрланговский стэк-трейс, а сообщение в терминах бизнес-логики, и содержащее контекст (данные)

It is important to know that the protected part of an exception can't be tail recursive.
The VM must always keep a reference there in case there's an exception popping up.

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


## распределенность

the next problem you get is hardware failures.  to have your program
 running on more than one computer at once, something that was needed
 for scaling anyway



функция/библиотека может:
- вернуть ok | error (Maybe or Result ok|error)
- бросить исключение
вызывающий код может
- матч на ok
- case на ok|error
- ловить исключение
- игнорировать исключение
