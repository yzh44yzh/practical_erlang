

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


##

http://learnyousomeerlang.com/errors-and-processes

The first writers of Erlang always kept in mind that failure is common. You can try to prevent bugs all you want, but most of the time some of them will still happen. In the eventuality bugs don't happen, nothing can stop hardware failures all the time. The idea is thus to find good ways to handle errors and problems rather than trying to prevent them all.

Some studies proved that the main sources of downtime in large scale software systems are intermittent or transient bugs (source). Then, there's a principle that says that errors which corrupt data should cause the faulty part of the system to die as fast as possible in order to avoid propagating errors and bad data to the rest of the system.
(stidues -- диссертация Джо Армстронга, и другие)

the next problem you get is hardware failures.
 to have your program running on more than one computer at once, something that was needed for scaling anyway

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

link, spawn_link
{'EXIT', Pid, Reason}

monitor
{'DOWN', Reference, process, Pid, Reason}

exit, trap exit

process_flag(trap_exit, true).
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
