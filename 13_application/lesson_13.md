## application

http://www.erlang.org/doc/design_principles/applications.html
http://www.erlang.org/doc/man/application.html

component that can be started and stopped as a unit, and which can also be reused in other systems.

An Erlang system will consist of a set of loosely coupled applications. Some are devel-
oped by the programmer or the open source community, and others will be part of the
OTP distribution.

Supervision trees are packaged into a behavior called an application. OTP applications
not only are the building blocks of Erlang systems, but also are a way to package reus-
able components. Industrial-grade systems consist of a set of loosely coupled, possibly
distributed applications. These applications are part of the standard Erlang distribution
or are specific applications developed by you, the programmer.

to package Erlang modules into reusable components.
An Erlang system will consist of a set of loosely coupled applications.

a component implementing some specific functionality, that can be
started and stopped as a unit, and which can be re-used in other
systems as well.


### Application Resource File

application specification

http://www.erlang.org/doc/man/app.html (не уверен, что нужно давать эту ссылку)

ebin/some.app
application resource file, also known as the app file

A resource file associated
with every application not only describes it, but also specifies its modules, registered
processes, and other configuration data.

{application, ApplicationName, Properties}.

ApplicationName, an atom, is the name of the application. The file must be named ApplicationName.app.


All keys are optional

{description, "Some description of your application"}
A one-line description of the application.

{vsn, "1.2.3"}
It's usually a good idea to stick to a scheme of the form <major>.<minor>.<patch>

{modules, ModuleList}
Contains a list of all the modules that your application introduces to the system.
поддерживать это вручную неудобно, поэтому
src/some.app.src
rebar генерирует ebin/some.app

The purpose of
listing them is twofold. The first is to ensure that all of them are present when building
the system and that there are no name clashes with any other applications. The second
is to be able to load them either at startup or when loading the application.

{registered, AtomList}
All names of registered processes in the application. systools uses this list to detect name clashes between applications.
entirely based on trusting the developers to give good data

{env, [{Key, Val}]}
Key is to be an atom. Val is any term.

{applications, AtomList}
All applications that must be started before this application is started
all applications have dependencies to at least Kernel and STDLIB.

description, vsn, modules, registered, and application -- важны для сборки релизов

есть некоторое дублирование зависимостей
в rebar.config описаны зависимые библиотеки
здесь описаны зависимые app, которые должны быть запущены, чтобы могло нормально работать это app
важно поддерживать эти зависимости, чтобы правильно собирался релиз


{mod, {CallbackMod, Args}}
The key mod defines the callback module and start argument of the application
какое значение по дефолту? А нету значения по дефолту. И нужно обязательно указывать для normal app,
можно не указывать для library app.


### Application Callback Module

start(StartType, StartArgs) -> {ok, Pid} | {ok, Pid, State}
stop(State)

 start_type() = normal
             | {takeover, Node :: node()}
             | {failover, Node :: node()}

restart_type() = permanent | transient | temporary

to create the supervision tree by starting the top supervisor.

stop/1 is called after the application has been stopped and is to do any necessary cleaning up

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

## OTP

gen_fsm, gen_event пару слов про них
