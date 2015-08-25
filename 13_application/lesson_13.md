## Application

На уровне синтаксиса языка код структурируется в функции и модули.
На уровне потоков код структурируется в дерево супервизоров.
Эти две структуры существуют независимо друг от друга.
Но есть **Application**, которое связывает их вместе.

Во многих языка мы привыкли, что после функций и модулей, следующим
уровнем идут пакеты.  В эрланг нет пакетов, но Application отчасти
выполняет эту роль -- группирует несколько модулей в одну сущность.
_(К сожалению, Application не создает пространства имен.  Имена всех
модулей находятся в одной области видимости, и конфликты имен иногда
случаются.)_

С другой стороны, Application контролирует часть дерева супервизоров и
группирует потоки подобно тому, как пакет группирует модули. Эта
группа (поддерево) может быть запущена и остановлена как единое целое.

Ну и Application следует рассматривать как некий компонент,
предназначенный для повторного использования в разных проектах.
Причем, для повторного использования предназначены обе структуры: и
структура кода (функции-модули), и структура потоков (поддерево
супервизоров).

Проект на эрланг обычно состоит из нескольких приложений:

Во-первых, это приложения, которые пишут разработчики --
непосредственно код проекта.

Во-вторых, это используемые библиотеки. Обычно каждая библиотека
оформляется как Application. Например, библиотека для логирования
[lager](https://github.com/basho/lager), библиотека для сериализации
JSON [jiffy](https://github.com/davisp/jiffy), драйвер для работы с
PostgreSQL [epgsql](https://github.com/epgsql/epgsql) и другие.

В-третьих, это приложения, входящие в состав OTP. Например, приложение
для работы с сетью **inets**, приложения, отвечающие за шифрование
**crypto** и **ssl**, приложение для модульного тестирования **eunit**
и другие.

Application состоит, как минимум, из главного модуля, реализующего
**behaviour(application)**, нескольких других модулей и специального
файла с метаинформацией. (Полную структуру мы рассмотрим на следующем уроке).


### Application Resource File

Начнем с файла, описывающего метаинформацию о приложении.
Он должен называться по имени приложения и иметь расширение **app**.
Например, **my_cool_component.app**.

Внутри он содержит кортеж из трех элементов:

```erlang
{application, ApplicationName, Properties}.
```

**ApplicationName** -- имя приложения в виде атома. Например, **my_cool_component**.

**Properties** -- свойства приложения в виде proplist, где, обычно, присутствуют такие элементы:
- **description** -- краткое описание приложения одной строкой;
- **vsn** -- версия приложения, обычно в формате "major.minor.patch";
- **modules** -- список всех модулей, входящих в состав приложения;
- **registered** -- список всех имен под которыми регистрируются потоки;
- **env** -- настройки приложения в виде вложенного proplist;
- **applications** -- список других приложений, от которых зависит данное приложение;
- **mod** -- основной модуль приложения, реализующий behaviour(application).

Все опции считаются необязательными, но лучше указывать их явно.
Большинство из них важны для сборки релиза.  Инструменты, собирающие
релиз, проверяют наличие указанных модулей, определяют очередность
загрузки приложенией, выявляют конфликты имен потоков. _(Сборка релиза
не входит в данный курс, разные команды делают это по-разному.)_

Пример ресурс файла, взят из cowboy 1.0.1:

```erlang
{application, cowboy, [
	{description, "Small, fast, modular HTTP server."},
	{vsn, "1.0.1"},
	{id, "git"},
	{modules, []},
	{registered, [cowboy_clock, cowboy_sup]},
	{applications, [
		kernel,
		stdlib,
		ranch,
		cowlib,
		crypto
	]},
	{mod, {cowboy_app, []}},
	{env, []}
]}.
```

Из этого файла видно следующее:

Ключ **id** не документирован, это авторы cowboy сами что-то
придумали :)

Список модулей оставлен пустым. Его трудно поддерживать вручную,
обычно он генерируется автоматически при сборке проекта.

Cowboy регистритует 2 потока с именами **cowboy_clock** и
**cowboy_sup**.

Cowboy зависит от 5-ти других приложений. kernel, stdlib и crypto --
это часть OTP, ranch и cowlib -- это еще 2 приложения от тех же
авторов.

Главный модуль -- **cowboy_app**.

Настроек тут нет, cowboy конфигурируется другим способом.


### запуск

When an Erlang runtime system is started, a number of processes are
started as part of the Kernel application. One of these processes is
the application controller process, registered as
**application_controller**.

It starts all other applications and sits on top of most of them. In
fact, you could say the application controller acts a bit like a
supervisor for all applications. (есть исключения, kernel, например)

All operations on applications are coordinated by the application
controller. It is interacted through the functions in the module
application, see the application(3) manual page in Kernel. In
particular, applications can be loaded, unloaded, started, and
stopped.

The application controller then creates an **application master** for the
application. The application master is the group leader of all the
processes in the application. The application master starts the
application by calling the application callback function start/2 in
the module, and with the start argument, defined by the mod key in the
.app file.

The application master is in fact two processes taking charge of each
individual application: they set it up and act like a middleman in
between your application's top supervisor and the application
controller.

observer:start() тут как раз и видно, что каждое приложение начинается с двух безымянных процессов,
и под ними корневой супервизор приложения.

The application master stops the application by telling the top
supervisor to shut down. The top supervisor tells all its child
processes to shut down, and so on; the entire tree is terminated in
reversed start order. The application master then calls the
application callback function stop/1 in the module defined by the mod
key.


#### application:start(my_app).

The application controller checks the value of the application specification key applications, to ensure that all applications that should be started before this application are running. If not, {error,{not_started,App}} is returned, where App is the name of the missing application.

ensure_started(Application)
Equivalent to application:start/1,2 except it returns ok for already started applications.

ensure\_all\_started(Application)
Equivalent to calling application:start/1,2 repeatedly on all dependencies that have not yet been started for an application.

temporary
transient
permanent

If a permanent application terminates, all other applications and the runtime system are also terminated.
If a transient application terminates with reason normal, this is reported but no other applications are terminated. If a transient application terminates abnormally, that is with any other reason than normal, all other applications and the runtime system are also terminated.
If a temporary application terminates, this is reported but no other applications are terminated.

An application can always be stopped explicitly by calling application:stop/1. Regardless of the mode, no other applications are affected.

The transient mode is of little practical use, since when a supervision tree terminates, the reason is set to shutdown, not normal.

#### Module:start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State} | {error, Reason}

This function is called whenever an application is started using application:start/1,2, and should start the processes of the application. If the application is structured according to the OTP design principles as a supervision tree, this means starting the top supervisor of the tree.

#### stop(Application) -> ok | {error, Reason}

The application master calls Module:prep_stop/1, if such a function is defined,
and then tells the top supervisor of the application to shutdown
This means that the entire supervision tree, including included applications, is terminated in reversed start order.
After the shutdown, the application master calls Module:stop/1.
Last, the application master itself terminates


#### which_applications() -> [{Application, Description, Vsn}]

Returns a list with information about the applications which are currently running. Application is the application name. Description and Vsn are the values of its description and vsn application specification keys, respectively.


Распределенное приложение -- за рамками курса. Есть глава у Фреда.


### настройки

application:get\_env(Name,Tag)

If the application argument is omitted, it defaults to the application of the calling process.

If the specified application is not loaded, or the specification key
does not exist, or if the process executing the call does not belong
to any application, the function returns undefined.

внимание, тут разница в возвращаемом значении:
    get_env(Par) -> undefined | {ok, Val}
    get_env(Application, Par, Def) -> Val

не редакая ошибка такой код

    {ok, Val} = application:get_env(my_app, my_key)

заменить на такой

    {ok, Val} = application:get_env(my_app, my_key, DefaultValue)

и тут будет badmatch, т.к. нужно еще заменить {ok, Val} на Val.


application:get\_all\_env(Name).

из app file
или из sys.config

The values in the .app file can be overridden by values in a system configuration file.

set_env(Application, Par, Val) - пожалуй об этом лучше не писать,
а то придется описывать приоритеты настроек при рестарте приложения
