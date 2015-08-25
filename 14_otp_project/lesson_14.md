- источники инфы:
  - notes.org
  - erlang-school
  - официальные доки
  - Армстронг
  - Цезарини
  - Хеберт
  - erlang in anger
  - OTP in action
  - yzh44yzh.by

# OTP фреймворк

The entire purpose of the Erlang/ OTP ecosystem is building stable, fault-tolerant systems

## Типичная структура OTP проекта

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


## gen_fsm, gen_event
пару слов про них
