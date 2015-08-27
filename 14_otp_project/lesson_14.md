# OTP фреймворк

OTP stands for the Open Telecom Platform.
название устарело, т.к. это фреймворк общего назначения, для любых проектов, а не только Telecom

единственный фреймворк в эрланг, стандарт де-факто, и без него проекты не делаются

It’s an application
operating system and a set of libraries and procedures used for building large-
scale, fault-tolerant, distributed applications.

The entire purpose of the Erlang/ OTP ecosystem is building stable, fault-tolerant systems

set of libraries that have been carefully engineered and battle-hardened over years

The OTP framework is also a set of modules and standards

Включает:
- поведения (behaviour)
  gen\_server, supervisor, application -- сердце OTP, используются везде
  gen\_fsm, gen\_event -- используются редко
- рекомендуемая структура проекта
- системные библиотеки и приложения TODO примеры

The power of OTP comes from the fact that properties such as fault tolerance,
scalability, dynamic-code upgrade, and so on, can be provided by the behavior
itself. In other words, the writer of the callback does not have to worry about
things such as fault tolerance because this is provided by the behavior.


## gen_fsm

Finite-State Machine (FSM)

усложненный вариант gen\_server, с несколькими состояниями и разными обработчиками для разных состояний
(gen\_server можно рассматривать как вырожденный случай gen\_fms, у которого только одно состояние).

TODO у Фреда есть инфа

## gen_event

In OTP, an event manager is a named object to which events can be sent. An event can be, for example, an error, an alarm, or some information that is to be logged.

In the event manager, zero, one, or many event handlers are installed. When the event manager is notified about an event, the event is processed by all the installed event handlers. For example, an event manager for handling errors can by default have a handler installed, which writes error messages to the terminal. If the error messages during a certain period is to be saved to a file as well, the user adds another event handler that does this. When logging to the file is no longer necessary, this event handler is deleted.

An event manager is implemented as a process and each event handler is implemented as a callback module.

- start event manager
- add/delete an Event Handler
- Notifying about Events

TODO у Фреда есть инфа


## Типичная структура OTP проекта

Well-behaved OTP applications usually have the files belonging to different
parts of the application in well-defined places

С этой структурой изначально работают тулы, собирающие релизы (TODO какие?).
Позже добавился rebar

с дочерними приложениями и зависимыми библиотеками
назначение каждой папки

They give a directory structure,
a way to handle configurations,
a way to handle dependencies,
create environment variables and configuration,
ways to start and stop applications,

http://www.erlang.org/doc/design_principles/applications.html
7.4  Directory Structure
    src - Contains the Erlang source code.
    ebin - Contains the Erlang object code, the beam files. The .app file is also placed here.
    priv - Used for application specific files. For example, C executables are placed here. The function code:priv_dir/1 is to be used to access this directory. (necessary scripts, graphics, configuration files, or other non-Erlang-related resources.)
    include - Used for include files.
(Contains all the Erlang header files ( hrl ) intended for use outside the application)

при использовании rebar к этому еще добавляется
**deps**
**test**

**.eunit** - автоматически создаются

**docs** whenever you have EDoc documentation to add to your application.

**logs** - это не стандарт, это добавляю уже я.
Удобно локально, для разработки. Для прода логи обычно бывают где-то в другом месте. Например, в /var/log/your_project


Виды проектов (по Фреду).
Примеры проектов каждого вида.

**normal applications**
will start the supervision tree and all of the relevant static workers.

**Library applications**
contain library modules but do not start the supervision tree.
This is not
to say that the code may not contain processes or supervision trees. It just means they
are started as part of a supervision tree belonging to another application.


## rebar

rebar.config

modules - Contains a list of all the modules that your application introduces to the system.
поддерживать это вручную неудобно, поэтому
src/some.app.src
rebar генерирует ebin/some.app
