##

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
