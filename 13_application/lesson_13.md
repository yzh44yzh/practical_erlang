- источники инфы:
  + http://www.erlang.org/doc/design_principles/applications.html
  - http://www.erlang.org/doc/man/application.html
  - Хеберт
  - Армстронг
  - Цезарини
  - OTP in action

## application

http://www.erlang.org/doc/design_principles/applications.html
http://www.erlang.org/doc/man/application.html

component that can be started and stopped as a unit, and which can also be reused in other systems.

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

**normal applications**
will start the supervision tree and all of the relevant static workers.

**Library applications**
contain library modules but do not start the supervision tree.


### Application Resource File

ebin/some.app
application resource file, also known as the app file

A resource file associated
with every application not only describes it, but also specifies its modules, registered
processes, and other configuration data.

{application, ApplicationName, Properties}.

ApplicationName, an atom, is the name of the application. The file must be named ApplicationName.app.


All keys are optional

{description, "Some description of your application"}

{vsn, "1.2.3"}

{modules, ModuleList}
Contains a list of all the modules that your application introduces to the system.
поддерживать это вручную неудобно, поэтому
src/some.app.src
rebar генерирует ebin/some.app

{registered, AtomList}
All names of registered processes in the application. systools uses this list to detect name clashes between applications.

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
application:get\_all\_env(Name).

из app file
или из sys.config

The values in the .app file can be overridden by values in a system configuration file.


### запуск

When an Erlang runtime system is started, a number of processes are started as part of the Kernel application. One of these processes is the application controller process, registered as application_controller.

All operations on applications are coordinated by the application controller. It is interacted through the functions in the module application, see the application(3) manual page in Kernel. In particular, applications can be loaded, unloaded, started, and stopped.

The application controller then creates an application master for the application. The application master is the group leader of all the processes in the application. The application master starts the application by calling the application callback function start/2 in the module, and with the start argument, defined by the mod key in the .app file.

The application master stops the application by telling the top supervisor to shut down. The top supervisor tells all its child processes to shut down, and so on; the entire tree is terminated in reversed start order. The application master then calls the application callback function stop/1 in the module defined by the mod key.

application:load(my_app).
application:unload(my_app).
application:loaded_applications().

application:start(my_app).
application:stop(my_app).
application:which_applications().

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

## OTP

gen_fsm, gen_event пару слов про них
