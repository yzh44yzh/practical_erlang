%% Реализовать group_by

%% Первый этап -- решение частной задачи

%% У нас есть список пользователей вида:
[
 {user, "Bob", 21, male},
 {user, "Bill", 23, male},
 {user, "Helen", 17, female},
 {user, "Kate", 25, female},
 {user, "John", 20, male}
].

%% Нужно реализовать функцию group_by(Users) -> Groups
%% которая сгруппирует пользователей по полу, и вернет map
%% где ключами будет пол, а значениям будет список пользователей этого пола.

%% Например:
#{
  male => [{user, "Bob", 21, male}, {user, "Bill", 23, male}, {user, "John", 20, male}],
  female => [{user, "Kate", 25, female}, {user, "Helen", 17, female}]
}.


%% Второй этап -- обобщенное решение

%% Нужно реализовать функцию, которая может сгруппировать любой список по любому критерию,
%% даже не зная, что за элементы находятся внутри списка.

%% Она должна принимать два аргумента:
%% - функцию, которая для данного элемента списка может сказать, к какому критерию отностися этот элемент
%% - и сам список
%% group_by(CriteriaFun, List) -> Groups

%% Применяя разные CriteriaFun, можно делать разные группировки списка одной и той же функцией group_by.

%% Например, мы хотим сгруппировать пользователей по возрасту:
%% CriteriaFun = fun(User) -> ??? end.
%% group_by(CriteriaFun, Users) ->
%%     #{21 => [{user, "Bob", 21, male}], 23 => [{user, "Bill", 23, male}} ...

%% Или мы можем определить некие диапазоны возрастов:
%% 0-12 - child
%% 13-18 - teeneage
%% 18-25 - young
%% 26-60 - adult
%% > 10 - old
%% и сгруппировать пользователей по этим диапазонам:
%% CriteriaFun = fun(User) -> ??? end.
%% group_by(CriteriaFun, Users) ->
%%    #{teenage => [{user, "Helen", 17, female}], young => [{user, "Bob", 21, male}, {user, "Bill", 23, male} ...

%% Третий пример:
%% У нас есть распределенная система, кластер из нескольких узлов.
%% В этой системе есть клиентские соединения разных типов, подключенные к разным узлам:
[
 {session, type_a, node_1, SocketId1},
 {session, type_b, node_1, SocketId2},
 {session, type_a, node_2, SocketId3},
 {session, type_b, node_2, SocketId4}
 ]
%% Мы хотим сгруппировать эти сессии по узлу:
#{
  node_1 => [{session, type_a, node_1, SocketId1}, {session, type_a, node_2, SocketId3}],
  node_2 => [{session, type_a, node_2, SocketId3}, {session, type_b, node_2, SocketId4}]
 }
%% А потом мы хотим сгруппировать их по типу:
#{
   type_a => [{session, type_a, node_1, SocketId1}, {session, type_a, node_2, SocketId3}],
   type_b => [{session, type_b, node_1, SocketId2}, {session, type_b, node_2, SocketId4}]
}
%% И во всех случаях мы применяем одну и ту же функцию group_by,
%% передавая ей разные CriteriaFun и разные списки.
