# Live Coding. Выбор мастера с помощью Raft Consensus Algorithm.

Известно, что эрланг предназначен для разработки распределенных
систем. Это сложная область программирования, и о ней можно много
говорить.  Но зачем говорить, когда можно просто взять, и сделать
что-нибудь интересное?

Я проведу сессию программирования на эрланг и реализую механизм выбора
мастера для кластера из эрланговских нод с помощью Raft Consensus
Algorithm.


## Что это и зачем

https://ru.wikipedia.org/wiki/%D0%90%D0%BB%D0%B3%D0%BE%D1%80%D0%B8%D1%82%D0%BC_Raft

Raft — алгоритм для решения задач консенсуса в сети надёжных вычислений

Консе́нсус (лат. consensus — согласие) — общее согласие по спорному или
обсуждаемому вопросу, достигнутое в результате дискуссии без процедуры
голосования[1]. Результат разрешения конфликтов при принятии решений
после устранения принципиальных возражений у всех заинтересованных
участников, либо после исключения из участия в обсуждении немногих
несогласных участников.

Consensus algorithms allow a collection of machines
to work as a coherent group that can survive the fail-
ures of some of its members.

play a key role in building reliable large-scale software systems.

For example, large-scale systems that
have a single cluster leader, typically use a separate replicated state machine
to manage leader election
and store configuration information that must survive leader crashes.
Examples: Chubby and ZooKeeper.

https://zookeeper.apache.org/
highly reliable distributed coordination
centralized service for:
- maintaining configuration information, -- это ясно
- naming, -- это хз
- providing distributed synchronization, -- это слишком абстрактно
- and providing group services -- это хз
Consensus, group management, and presence protocol


безопасную и эффективную реализацию машины состояний поверх кластерной вычислительной системы.

Raft строится поверх кластера, на каждой из нод которого работает некая машина состояний.
Raft обеспечивает надёжную доставку сигналов на все ноды в заданном порядке.
Таким образом обеспечивается переход всех машин состояний по одним и тем же последовательностям состояний.
Таким образом, каждая нода гарантированно приходит в согласие с другими нодами.

*Чёткое разделение фаз*
декомпозицию задачи управления кластером на несколько, слабо связанных, подзадач:


**выбор лидера (голосование)**

Если обычная нода долго не получает сообщений от лидера, то она переходит в состояние «кандидат»
и посылает другим нодам запрос на голосование.

Другие ноды голосуют за того кандидата, от которого они получили первый запрос.

Если кандидат получает сообщение от лидера, то он снимает свою кандидатуру и возвращается в обычное состояние.

Если кандидат получает большинство голосов, то он становится лидером.

Если же он не получил большинства (это случай, когда на кластере возникли сразу несколько кандидатов и голоса разделились),
то кандидат ждёт случайное время и инициирует новую процедуру госования.

Процедура голосования повторяется, пока не будет выбран лидер.


**репликация протоколов**

Лидер полностью отвечает за правильную репликацию протоколов.

Он отправляет все нодам кластера запрос на добавление новой записи
и считает транзакцию успешной только после того, как большинство нод подтвердило, что данные были применены и результат сохранён


**Явно выделенный лидер**

на кластере всегда существует явно выделенный лидер. Только этот лидер отправляет новые записи на другие ноды кластера.

остальные ноды следуют за лидером и не взаимодействуют между собой (за исключением фазы голосования).

Если внешний клиент подключается к кластеру через обычную ноду, то все его запросы перенаправляются лидеру


**Записи добавляются строго последовательно, без пропусков**

Raft строго нумерует все записи в протоколе работы. Записи должны идти строго последовательно.
Эти номера играют важную роль в работе алгоритма. По ним определяется степень актуальности состояния ноды.

Эти же номера используются для нумерации сессий голосования. На каждый запрос на голосование нода может проголосовать лишь единожды.


**Изменение размера кластера**

Raft позволяет легко менять конфигурацию кластера, не останавливая работы: добавлять или удалять ноды.

Raft’s mechanism for
changing the set of servers in the cluster uses a new
joint consensus approach where the majorities of
two different configurations overlap during transi-
tions.
непонятно

**safety** (never returning an incorrect result) under all conditions, including
network delays, partitions, and packet loss, duplication, and reordering.

**available** as long as any majority of the servers are operational and can com-
municate with each other and with clients.
cluster of five servers can tolerate the failure of any two servers
they may later recover and rejoin the cluster.


## Интерактивная модель

https://raft.github.io/

продумать разные сценарии и показать их на этой модели:
- падение реплики
- падение мастера
- net split
- конфликт двух мастеров
- старт ноды (и всего кластера)
- что еще?

## Реализация

At any given time each server is in one of three states:
**leader**, **follower**, or **candidate**.

In normal operation there is exactly one leader and all of the other servers are followers.

**Followers** are passive: they issue no requests on
their own but simply respond to requests from leaders
and candidates.

The **leader** handles all client requests (if
a client contacts a follower, the follower redirects it to the
leader).

The third state, **candidate**, is used to elect a new
leader

Raft divides time into **terms**
Terms are numbered with consecutive integers.
Each term begins with an election
If a candidate wins the election, then it serves as leader for the rest of the term.

In some situations
an election will result in a split vote. In this case the term
will end with no leader; a new term (with a new election)
will begin shortly.

Current terms are exchanged
whenever servers communicate; if one server’s current
term is smaller than the other’s, then it updates its current
term to the larger value.
If a candidate or leader discovers
that its term is out of date, it immediately reverts to fol-
lower state.
If a server receives a request with a stale term
number, it rejects the request.

**схема конечного автомата для ноды**
raft.pdf page 5
нужно перерисовать более крупно для презентации
и картика с term тоже нужна в презентации

[Leader]
- discover other leader with higher term -> [Candidat]

[Candidat]
- receive votes from majority of servers -> [Leader]
- timeout, new election -> [Candidat]
- discover current leader or new term -> [Follower]

[Follower]
- timeout, start election -> [Candidat]


basic consensus algorithm requires only
**two types of RPCs**:

RequestVote - initiated by candidates during elections
Append-Entries - initiated by leaders to replicate log entries and to provide a form of heartbeat

Additionally: third RPC for transferring snapshots between servers

Servers retry RPCs if they do not receive a response in a timely manner


### Leader election

When servers start up, they begin as followers
A server remains in follower state as long as it receives valid RPCs from a leader or candidate

Leaders send periodic heartbeats to all followers
(AppendEntries RPCs that carry no log entries)

If a follower receives no communication over a period of time
called the **election timeout**, then it assumes there is no viable leader
and begins an election to choose a new leader.

To begin an election, a follower increments its current term
and transitions to candidate state.

It then votes for itself
and issues RequestVote RPCs in parallel to each of the other servers in the cluster.

A candidate continues in this state until one of three things happens:
- it wins the election,
- another server establishes itself as leader
- a period of time goes by with no winner.


**wins the election**

it receives votes from a majority of the servers in the full cluster for the same term.

Each server will vote for at most one candidate in a given term,
on a first-come-first-served basis

Once a candidate wins an election, it becomes leader.
It then sends heartbeat messages to all of the other servers
to establish its authority and prevent new elections.


**another server establishes itself as leader**

While waiting for votes, a candidate may receive an AppendEntries RPC
from another server claiming to be leader.

If the leader’s term >= candidate’s current term,
candidate recognizes the leader
and returns to follower state.

If the term in the RPC < candidate’s current term,
then the candidate rejects the RPC and continues in candidate state.


**a period of time goes by with no winner**

if many followers become candidates at the same time,
votes could be split so that no candidate obtains a majority.

each candidate will time out
and start a new election by incrementing its term
and initiating another round of Request-Vote RPCs.

Raft uses randomized election timeouts to ensure that
split votes are rare and that they are resolved quickly.

election timeouts are
chosen randomly from a fixed interval (e.g., 150–300ms)

in most cases only a single server will time out
it wins the election and sends heartbeats before any other servers time out


### Log replication

ну это не очень актуально, так что кратенько
