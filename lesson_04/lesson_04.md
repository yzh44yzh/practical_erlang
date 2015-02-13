# Сопоставление с образцом

Когда я впервые увидел конструкцию **сопоставления с образцом**
(**pattern matching**), я сразу в нее влюблися.  И, может быть, это
главная причина, почему я полюбил эрланг. (На то время я не знал
других функциональных языков). В императивных языках такой конструкции
либо нет вообще, либо есть некое бледное подобие.

Поскольку мы уже пользовались сопоставлением с образцом на предыдущих
уроках, то у вас, наверняка, есть понимание (или интуитивное ощущение)
как это работает. Но теперь пришло время четко разобраться во всех
нюансах.


in Erlang is used to:
 - Assign values to variables
   оператора присваивания нет, есть оператор сопоставления с образцом :)
 - Control the execution flow of programs
 - Extract values from compound data types

Pattern matching occurs when:
 - evaluating a function call,
 - case- receive- try- expressions
 - match operator (=) expressions

left-hand side pattern is matched against a right-hand side term.
If the matching succeeds, any unbound variables in the pattern become bound. If the matching fails, a run-time error occurs.
несвязанная (unbound) - связанная (bound) переменная

Expr1 = Expr2
Matches Expr1, a pattern, against Expr2. If the matching succeeds, any unbound variable in the pattern becomes bound and the value of Expr2 is returned.

If the matching fails, a badmatch run-time error will occur.

Unbound variables are only allowed in patterns.
Variables are bound to values using pattern matching. Erlang uses single assignment, a variable can only be bound once.

The anonymous variable is denoted by underscore (_) and can be used when a variable is required but its value can be ignored.

Note that since variables starting with an underscore are not anonymous, this will match:
{_,_} = {1,2}
But this will fail:
{_N,_N} = {1,2}


Вот интересный пример:

```
%% check table owner leave table in waiting state
check_room_owner(RoomId, OwnerId) ->
    case  personal_table:get_table_for_room(RoomId) of
        {ok, #ptable{id = TableId, owner = OwnerId}} ->
            bingo_room_manager:close_table_and_room(TableId, RoomId);
        _ -> do_nothing
    end
end
```
Находим стол, и сразу матчингом UserId проверяем владельца.


## clause

klôz

a unit of grammatical organization next below the sentence in rank and
in traditional grammar said to consist of a subject and predicate.
"In each sentence above, two clauses are linked by clause-chaining without conjunctions."

синонимы: section, paragraph, article, subsection

TODO: немного познакомиться с прологом, в самых общих чертах

f({connect,From,To,Number,Options}, To) ->
    Signal = {connect,From,To,Number,Options},
can instead be written as
f({connect,_,To,_,_} = Signal, To) ->

следить за правильной очередностью
компилятор предупредит, но не всегда


## guards

A guard sequence is a sequence of guards, separated by semicolon (;). The guard sequence is true if at least one of the guards is true. (The remaining guards, if any, will not be evaluated.)
Guard1;...;GuardK

A guard is a sequence of guard expressions, separated by comma (,). The guard is true if all guard expressions evaluate to true.
GuardExpr1,...,GuardExprN

Guards are an extension of pattern
matching

If an arithmetic expression, a boolean expression, a short-circuit expression, or a call to a guard BIF fails (because of invalid arguments), the entire guard fails. If the guard was part of a guard sequence, the next guard in the sequence (that is, the guard following the next semicolon) will be evaluated.

guards cannot call user-defined functions, since we want to
guarantee that they are side effect free and terminate.

The set of valid guard expressions (sometimes called guard tests) is a subset of the set of valid Erlang expressions. The reason for restricting the set of valid expressions is that evaluation of a guard expression must be guaranteed to be free of side effects.

подробнее, какие именно выражения и функции можно использовать, см в доке
http://erlang.org/doc/reference_manual/expressions.html#id81911


f(X) when (X == 0) or (1/X > 2) ->
...
g(X) when (X == 0) orelse (1/X > 2) ->
...
The guard in f(X) fails when X is zero but succeeds in g(X).
In practice, few programs use complex guards, and simple ( , ) guards
suffice for most programs.

TODO проверить все это:
and, or - вычисляют оба аргумента
andalso, orelse - вычисляют минимум аргументов
comma is andalso, semicolon is orelse


TODO проверить это:
Robert Virding:
Note that there is one very significant difference between ';' and or/orelse and that is how they behave with errors. ';' will just fail that one guard sequence and attempt the next one while or/orelse will fail the guard sequence they are in. So if they are used as an alternative to ';' you will get different behaviour.
That errors in guards cause the guard to fail and not generate an exception was an intentional design decision, it saved a lot of explicit type tests and they were implicit in the operations. For example you could do tuple_size(T) without first having to check if T is a tuple.


## конструкция case

case Expr of
    Pattern1 [when GuardSeq1] ->
        Body1;
    ...;
    PatternN [when GuardSeqN] ->
        BodyN
end

The expression Expr is evaluated and the patterns Pattern are sequentially matched against the result. If a match succeeds and the optional guard sequence GuardSeq is true, the corresponding Body is evaluated.

If there is no matching pattern with a true guard sequence, a case_clause run-time error will occur.

The scope for a variable is its function clause. Variables bound in a branch of an if, case, or receive expression must be bound in all branches to have a value outside the expression, otherwise they will be regarded as 'unsafe' outside the expression.


## конструкция if

if
    GuardSeq1 ->
        Body1;
    ...;
    GuardSeqN ->
        BodyN
end

The branches of an if-expression are scanned sequentially until a guard sequence GuardSeq which evaluates to true is found. Then the corresponding Body (sequence of expressions separated by ',') is evaluated.

If no guard sequence is true, an if_clause run-time error will occur. If necessary, the guard expression true can be used in the last branch, as that guard sequence is always true.
