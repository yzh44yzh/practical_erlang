## application

http://www.erlang.org/doc/man/application.html

to package Erlang modules into reusable components.
An Erlang system will consist of a set of loosely coupled applications.

a component implementing some specific functionality, that can be
started and stopped as a unit, and which can be re-used in other
systems as well.

**normal applications**
will start the supervision tree and all of the relevant static workers.

**Library applications**
contain library modules but do not start the supervision tree.


### app file

ebin/some.app
application resource file, also known as the app file

A resource file associated
with every application not only describes it, but also specifies its modules, registered
processes, and other configuration data.

{application, ApplicationName, Properties}.

{description, "Some description of your application"}

{vsn, "1.2.3"}

{modules, ModuleList}
Contains a list of all the modules that your application introduces to the system.
поддерживать это вручную неудобно, поэтому
src/some.app.src
rebar генерирует ebin/some.app

{registered, AtomList}

{env, [{Key, Val}]}

{applications, AtomList}
A list of applications on which yours depends.

есть некоторое дублирование зависимостей
в rebar.config описаны зависимые библиотеки
здесь описаны зависимые app, которые должны быть запущены, чтобы могло нормально работать это app
важно поддерживать эти зависимости, чтобы правильно собирался релиз


{mod, {CallbackMod, Args}}
Defines a callback module for the application


### callbacks

 start_type() = normal
             | {takeover, Node :: node()}
             | {failover, Node :: node()}

restart_type() = permanent | transient | temporary


### настройки

application:get\_env(Name,Tag)
application:get\_all\_env(Name).

из app file
или из sys.config


### запуск

application:start(ApplicationName).

ensure_started(Application)
Equivalent to application:start/1,2 except it returns ok for already started applications.

ensure\_all\_started(Application)
Equivalent to calling application:start/1,2 repeatedly on all dependencies that have not yet been started for an application.

temporary
transient
permanent

application:stop(ApplicationName).
