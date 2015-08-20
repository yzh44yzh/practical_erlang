# OTP фреймворк

The entire purpose of the Erlang/ OTP ecosystem is building stable, fault-tolerant systems

## Типичная структура OTP проекта

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
    priv - Used for application specific files. For example, C executables are placed here. The function code:priv_dir/1 is to be used to access this directory.
    include - Used for include files.

при использовании rebar к этому еще добавляется
deps
test

.eunit - автоматически создаются

logs - это не стандарт, это добавляю уже я.
Удобно локально, для разработки. Для прода логи обычно бывают где-то в другом месте. Например, в /var/log/your_project

Виды проектов (по Фреду).
Примеры проектов каждого вида.

## rebar

rebar.config
