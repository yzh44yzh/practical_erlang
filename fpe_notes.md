Functional Programming in Erlang
https://www.futurelearn.com/courses/functional-programming-erlang/

They were wanting to build
complex combinations of hardware and software
that constituted telecom switchest.

So what they were doing was building systems
that existed in a highly concurrent environment.
Hundreds, thousands, tens of thousands, hundreds of thousands of calls might be handled simultaneously by one of these boxes

they needed lightweight fine-grained concurrency.

And that is because side effects and lazy evaluation fit very badly together. Laziness forces you to be pure. That's because you don't know when particular parts of an expression are going to be evaluated. So you don't know when that side effect, if it was there, would happen.

================

хороший пример pattern matching:
% exclusive OR
exOr(true,false) -> true;
exOr(false,true) -> true;
exOr(_,_) -> false.

рефакторинг:
exOr(X,X) -> false;
exOr(_,_) -> true.

еще вариант:
exOr(true, X) -> not X;
exOr(false, X) -> X.


================

You can't put general user-defined functions in guards.
The designers of Erlang didn't want pattern matching not to terminate. They would like pattern matching always to terminate.

================

перед тем, как давать рекурсию с аккумуляторами, стоит сперва показать direct recursion

sum([]) -> 0;
sum([H|T]) -> H + sum(T).


================

nub -- удаление дубликатов из списка -- хороший учебный пример

с сохранением первого элемента
nub([]) -> []
nub([H | T]) ->
    [H | nub(lists:delete(H, T))].

с сохранением последнего элемента
nub([]) -> []
nub([H | T]) ->
    case lists:member(H, T) of
        true -> nub(T);
        false -> [H | nub(T)]
    end.

================

palindrom с игнорированием регистра и пунктуации -- тоже хорошее упражнение.

palindrom("Madam I\'m Adam.").

пригодится filtermap, чтобы в один проход и отфильтровать лишние элементы, и преобразовать в lower case

Strategy is to solve a simplified problem and then either adapt that
simplified solution to the simplified problem for the larger one, or
to reuse it as a component of the overall solution.

================

permutations -- хорошее упражнение )

perms([1,2,3]) = [[1,2,3],[2,3,1],[3,1,2],[2,1,3],[1,3,2],[3,2,1]]


================
Merge sort
Quicksort
Insertion sort

тоже хорошее упражнение


================
HOF

Functions as arguments: map, filter, fold
Functions as results: curried functions
Functions as arguments and results: compose(F1,F2)->F3, iterage(F, NumTimes) -> F2
