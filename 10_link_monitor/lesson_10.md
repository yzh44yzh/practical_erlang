

## Обработка ошибок на низком уровне

link, spawn_link
{'EXIT', Pid, Reason}

exit(Pid, Reason)

**System processes** are basically normal processes, except they can
convert exit signals to regular messages. This is done by calling
process_flag(trap_exit, true) in a running process.

сигнал 'EXIT' превращает в сообщение 'EXIT', которое можно обработать.

|--------------+--------------------------------+-------------------------------|
| Reason       | trap_exit=true                 | trap_exit=false               |
|--------------+--------------------------------+-------------------------------|
| exit(normal) | Receives {'EXIT', Pid, normal} | Nothing happens               |
|--------------+--------------------------------+-------------------------------|
| exit(kill)   | Terminates with reason killed  | Terminates with reason killed |
|--------------+--------------------------------+-------------------------------|
| exit(Other)  | Receives {'EXIT', Pid, Other}  | Terminates with reason Other  |
|--------------+--------------------------------+-------------------------------|

monitor
{'DOWN', Reference, process, Pid, Reason}
