# Application

Пришло время создать и запустить Application.

У нас будет приложение **mylib** версии "0.1",
включающее 3 модуля: mylib\_app, mylib\_sup, mylib\_worker.
Из которых главный модуль -- mylib\_app.

У приложения будет несколько настроек:

```
min_val = 2
max_val = 10
connection_timeout = 10000
query_timeout = 10000
```

Все это должно быть описано в файле ресурсов **ebin/mylib.app**.

Главный модуль **src/mylib_app.erl** реализует behaviour(application) и запускает супервизор.
Супервизор **src/mylib_sup.erl** запускает рабочий процесс **src/mylib_worker.erl**.

Ну и mylib\_worker реализует несколько функций:

**get_version/0** -- возвращает версию приложения:

```
2> mylib_worker:get_version().
"0.1"
```


**get_modules/0** -- возвращает список модулей, входящих в приложение:

```
3> mylib_worker:get_modules().
[mylib_app,mylib_sup,mylib_worker]
```

**get_min_val()** и **get_connection_timeout()** возвращают значения соответствующих настроек:

```
4> mylib_worker:get_min_val().
2
5> mylib_worker:get_connection_timeout().
10000
```

И, наконец, **all_apps()** возвращает список всех запущенных приложений в виде структуры данных:

```
 #{
     app1 => #{
         description => "app1 description",
         version => "1.0"
     },
     appN => #{
         description => "appN description",
         version => "1.0"
     }
 }
```

например:

```
6> mylib_worker:all_apps().
 #{kernel => #{description => "ERTS  CXC 138 10",version => "4.1"},
   mylib => #{description => "My Cool Lib",version => "0.1"},
   stdlib => #{description => "ERTS  CXC 138 10",version => "2.6"}}
```

Не стесняйтесь заглянуть в **test/main_test.erl**, чтобы понять,
какие тесты должно пройти ваше приложение.
