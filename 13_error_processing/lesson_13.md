## Let it crash

http://learnyousomeerlang.com/errors-and-processes

The first writers of Erlang always kept in mind that failure is
common. You can try to prevent bugs all you want, but most of the time
some of them will still happen. In the eventuality bugs don't happen,
nothing can stop hardware failures all the time. The idea is thus to
find good ways to handle errors and problems rather than trying to
prevent them all.


```
{ok, Res} = do_something(Arg),
```

```

```
case do_something(Arg) of
    {ok, Res} -> code;
    {error, Reason} -> code
end.
```

Самые частые исключения:
badmatch
unknown function
badarith
что еще?

как читать стэктрейс?


## try..catch

много точек выхода с разными ошибками
(валидация входящих данных)

генерация исключений: throw, exit
разница между throw, error и exit
как выглядит матчинг и стек трейс во всех этих случаях

более понятное сообщение об ошибке
не эрланговский стэк-трейс, а сообщение в терминах бизнес-логики, и содержащее контекст (данные)


## supervisor

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
