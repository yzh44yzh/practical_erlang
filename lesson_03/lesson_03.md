## Функции высшего порядка

Если вы разобрались с предыдущим уроком, сделали упражнение, то этот урок
будет для вас простым. Простым, но необходимым :)

Functions that return funs, or functions that can accept funs as their
arguments, are called higher-order functions.

we can regard each operation on the entire
list as a single conceptual step in our program

What makes a true functional programming lan-
guage is the fact that functions can be handled just like any other sort of data. Functional
data types in Erlang are called funs. They can be passed as arguments to other functions
and they can be stored in data structures like tuples and records or sent as messages to
other processes. Most of all, they can be the results of other functions so that the func-
tions passed around as data can be created dynamically within the program, and are
not just pointers to statically defined functions.

TODO описания функций:
map, filter
filtermap
all, any
zip, zipwith


TODO
реализовать те же функции, что и на прошлом уроке, только теперь через map и filter
реализовано, взять код из ./main.erl

TODO пример на filtermap
сперва сделать в два прохода, потом в один

пример, где функция возвращает функцию.
такое используется не часто

1. Генераторы в юнит тестах. Нужен пример кода.

2. Таким образом можно задать callback. (Еще callback можно задать как {M, F, A}, и вызывать через erlang:apply/3)

3. Можно реализовать ленивые вычисления
(примеров не будет, см Цезарини, 9-я глава)

Erlang also provides ways to use functions that are already defined as arguments to
other functions, or as values of variables. Within a module M , a local function F with
arity n can be denoted by fun F/n . Outside the module, it will be denoted by
fun M:F/n


## Свертка

foldl, foldr

foldl(Fun, Accumulator, List)
Takes a two-argument function, Fun . These arguments will be an Element from the
List and an Accumulator value. The fun returns the new Accumulator , which is also
the return value of the call once the list has been traversed. Unlike its sister function,
lists:foldr/3 , lists:foldl/3 traverses the list from left to right in a tail-recursive
way

все, что можно реализовать через рекурсивные функции с аккумуляторами, можно реализовать и через свертку

так что можно взять примеры из прошлого урока, и реализовать их тут заново через свертку
и упражнения предложить соответствующие


## Конструкторы списков

lists comprehention

List comprehension is another powerful construct whose roots lie in functional pro-
gramming. List comprehension constructs allow you to generate lists, merge them to-
gether, and filter the results based on a set of predicates. The result is a list of elements
derived from the generators for which the predicate evaluated to true.

Синтаксис взять из математики

```
{x | x ∈ N, x > 0}
```
all values x such that x comes from the
natural numbers (denoted by N), and x is greater than zero

```
[X || X <- ListOfIntegers, X > 0]
```

```Erlang
L = [1,2,3,4,5].
lists:map(fun(X) -> 2*X end, L).
[2*X || X <- L ].
```

[ Expression || Generators, Guards, Generators, ... ]
Generators
A generator has the form Pattern <- List , where Pattern is a pattern that is
matched with elements from the List expression. You can read the symbol <- as
“comes from”; it’s also like the mathematical symbol ∈, meaning “is an element of.”
Guards
Guards are just like guards in function definitions, giving a true or false result.
The variables in the guards are those which appear in generators to the left of the
guard (and any other variables defined at the outer level).
Expression
The expression specifies what the elements of the result will look like.

the left side of the <- arrow doesn’t have to be a variable—it can be any pattern

```
[{area, H * W} || {rectangle, H, W} <- Shapes, H * W >= 10]
```

заменяют map и filter, но умеют работать по 2-м и более спискам одновременно

```Erlang
10> [ {X,Y} || X <- lists:seq(1,3), Y <- lists:seq(X,3) ].
[{1,1},{1,2},{1,3},{2,2},{2,3},{3,3}]
11> [ {X,Y} || X <- lists:seq(1,4), X rem 2 == 0, Y <- lists:seq(X,4) ].
[{2,2},{2,3},{2,4},{4,4}]
12> [ {X,Y} || X <- lists:seq(1,4), X rem 2 == 0, Y <- lists:seq(X,4), X+Y>4 ].
[{2,3},{2,4},{4,4}]
```

Pythagorean Triplets, armstrong, page 75
Pythagorean triplets are sets of integers {A,B,C} such that A 2 + B 2 = C 2 .
The function pythag(N) generates a list of all integers {A,B,C} such that
A 2 + B 2 = C 2 and where the sum of the sides is less than or equal to N :

```Erlang
pythag(N) ->
    [ {A,B,C} ||
    A <- lists:seq(1,N),
    B <- lists:seq(1,N),
    C <- lists:seq(1,N),
    A + B + C =< N,
    A * A + B * B =:= C*C
].
```

```Erlang
1> lib_misc:pythag(16).
[{3,4,5},{4,3,5}]
2> lib_misc:pythag(30).
[{3,4,5},{4,3,5},{5,12,13},{6,8,10},{8,6,10},{12,5,13}]
```

Block Expressions, armstrong, page 129
