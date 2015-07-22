- источники инфы:
  - Цезарини
  - OTP in action
  - официальные доки


# Обработка ошибок на низком уровне

Одна из главных фич эрланг -- устойчивость к ошибкам (fault
tolerance).  Считается, что система, сделанная на эрланг, может легко
переживать ошибки в коде и в данных, аппаратные сбои, сбои в сети, и
продолжать обслуживать клиентов.

Это не значит, что любой код на эрланг сразу обладает такими
свойствами.  Об этом должен позаботиться программист. А эрланг только
дает средства, которыми программист может обеспечить устойчивость.
Давайте посмотрим, что это за средства.

Устойчивость к ошибкам построена на способности потоков наблюдать
друг за другом.

**Armstrong**

If somebody dies, other people will notice.

If I’m in a room and suddenly keel over and die, somebody will probably
notice (well, at least I hope so). Erlang processes are just like people—
they can on occasion die. Unlike people, when they die, they shout out
in their last breath exactly what they have died from.

Imagine a room full of people. Suddenly one person keels over and dies.
Just as they die, they say “I’m dying of a heart attack” or “I’m dying of an
exploded gastric wobbledgog.” That’s what Erlang processes do. One
process might die saying “I’m dying because I was asked to divide by zero.”
Another might say, “I’m dying because I was asked what the last element
in an empty list was.”

Now in our room full of people, we might imagine there are specially
assigned people whose job it is to clear away the bodies. Let’s imagine
two people, Jane and John. If Jane dies, then John will fix any problems
associated with Jane’s death. If John dies, then Jane will fix the problems.
Jane and John are linked with an invisible agreement that says that if
one of them dies, the other will fix up any problems caused by the death.

That’s how error detection in Erlang works. Processes can be linked. If
one of the processes dies, the other process gets an error message saying
why the first process dies.

Pairs of processes can be linked. If one of the processes in a linked pair
dies, the other process in the pair will be sent a message containing the
reason why the first process died.


## link

A link is a specific kind of relationship that can be created between
two processes. When that relationship is set up and one of the
processes dies from an unexpected throw, error or exit (see Errors and
Exceptions), the other linked process also dies.

failing as soon as possible to stop errors

if the process that has an error crashes but those that depend on it
don't, then all these depending processes now have to deal with a
dependency disappearing. Letting them die and then restarting the
whole group is usually an acceptable alternative.

When
processes collaborate to solve a problem and something goes wrong, we can
sometimes recover, but if we can’t recover, we just want to stop everything
we were doing. This is rather like the notion of a transaction: either the pro-
cesses do what they were supposed to do or they are all killed.

When one of the linked processes crashes, a special kind of message is
sent, with information relative to what happened. No such message is
sent if the process dies of natural causes

**link/1**, which takes a Pid as an argument. When called, the
  function will create a link between the current process and the one
  identified by Pid.

**unlink/1**

-spec spawn_link(Fun) -> Pid
-spec spawn_link(Mod, Fnc, Args) -> Pid

-spec exit(Reason) -> none()
-spec exit(Pid, Reason) -> none()

TODO: пример кода

Links can not be stacked. If you call link/1 15 times for the same two
processes, only one link will still exist between them and a single
call to unlink/1 will be enough to tear it down.

Error propagation across processes is done through a process similar
to message passing, but with a special type of message called
signals.Exit signals are 'secret' messages that automatically act on
processes, killing them in the action.


## system processes

In order to restart a process, we need a way to first know that it
died. This can be done by adding a layer on top of links (the
delicious frosting on the cake) with a concept called system
processes. System processes are basically normal processes, except
they can convert exit signals to regular messages. This is done by
calling process_flag(trap_exit, true) in a running process.

By writing programs using system processes, it is easy to create a
process whose only role is to check if something dies and then restart
it whenever it fails.

There are two types of processes, normal processes and system processes.
spawn creates a normal process. A normal process can become a system
process by evaluating the BIF process_flag(trap_exit, true).

**System processes** are basically normal processes, except they can
convert exit signals to regular messages. This is done by calling
process_flag(trap_exit, true) in a running process.

-spec process_flag(trap_exit, true)

сигнал 'EXIT' превращает в сообщение 'EXIT', которое можно обработать.

{'EXIT', Pid, Reason}

TODO: пример кода


## kill

While you can trap most exit reasons, there are situations where you
might want to brutally murder a process: maybe one of them is trapping
exits but is also stuck in an infinite loop, never reading any
message. The kill reason acts as a special signal that can't be
trapped. This ensures any process you terminate with it will really be
dead. Usually, kill is a bit of a last resort, when everything else
has failed.

As the kill reason can never be trapped, it needs to be changed to
killed when other processes receive the message. If it weren't changed
in that manner, every other process linked to it would in turn die for
the same kill reason and would in turn kill its neighbors, and so
on. A death cascade would ensue.

When a system process receives a kill signal, it terminates. Kill signals
are generated by calling exit(Pid, kill) . This signal bypasses the normal error
signal processing mechanism and is not converted into a message. The
exit kill signal should be reserved for rogue processes that refuse to die
using any of the other error handling mechanisms.

Таблицу лучше представить в виде серии картинок

|--------------+--------------------------------+-------------------------------|
| Reason       | trap_exit=true                 | trap_exit=false               |
|--------------+--------------------------------+-------------------------------|
| exit(normal) | Receives {'EXIT', Pid, normal} | Nothing happens               |
|--------------+--------------------------------+-------------------------------|
| exit(kill)   | Terminates with reason killed  | Terminates with reason killed |
|--------------+--------------------------------+-------------------------------|
| exit(Other)  | Receives {'EXIT', Pid, Other}  | Terminates with reason Other  |
|--------------+--------------------------------+-------------------------------|

## monitor

More seriously, monitors are a special type of link with two differences:
- they are unidirectional;
- they can be stacked.

Monitors are what you want when a process wants to know what's going
on with a second process, but neither of them really are vital to each
other.

Reference = erlang:monitor(process, Pid)

{'DOWN', Reference, process, Pid, Reason}

erlang:demonitor(Ref, [flush, info]).

The info option tells you if a monitor existed or not when you tried to remove it.
demonitor returned false.
Using flush as an option will remove the DOWN message from the mailbox if it existed

monitoring process
does not have to become a system process in order to handle errors.

## Заключение

Низкоуровневые функции **link/unlink** почти не используются на
практике. Но базе **link** и **trap_exit** построен супервизор --
высокоуровневое средство для обработки ошибок.  Это тема следующего
урока.

TODO: криво сформулировано
**monitor** на практике используется, когда нам нужна специфическая
обработка завершения потока. Супервизор просто рестартует поток,
но не предоставляет возможности для такой обработки.
