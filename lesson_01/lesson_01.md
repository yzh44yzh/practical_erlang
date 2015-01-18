# Эрланг на практике, урок №1.

TODO:
- перелистать первые главы
  выбрать интересные нюансы
  + Армстронга
  - Цезарини
  - OTP in action
  - Хеберта

- подготовить презентацию
  - последний слайд: summary, о чем рассказывалось в уроке
- записать видео
- подготовить quiz

- подготовить упраженения
  - есть упражнения в книге Цезарини
  - этюды для эрланг
  - книги по хаскелю

- в syllabus.org поправить название и описание урока
- выложить все на hexlet.io


## Вводная про курс
все-таки какую-то вводную по языку нужно дать: легковесные потоки, обмен сообщениями, устойчивость к ошибкам, горячее обновление кода

TODO возьму инфу отсюда:
http://yzh44yzh.by/post/killer_features.html

TODO рекомендовать книги, дать ссылки

Все-таки последовательность изложения в книге Армстронга хромает. Но написана простым и понятны языком.
не идет от простого к сложному, и как-то прыгает с темы на тему.

У Цезарини более логичная последовательность, но написало более сухо, скучно.

У Фреда книжка отличная во всех отношениях. Но очень большая, изучать ее придется долго :)

другие книжки тож: OTP in Action и т.д.

Можете начинать с любой из этих 4х, все они хорошие )

а вообще книг много, поиск по amazon.com находит их около десятка.

## Окружение erlang
Версии. Куда ставиться. Что входит в поставку.

TODO что там внутри /usr/local/lib/erlang
показать на видео, дать пару слов в теории, можно и quiz


## Инструменты
TODO
emacs, intellij idea
hexlet web ide


## Работа в erlang-shell

TODO: В видео можно показать. В теории буквально 2 слова. Quiz можно сделать.

Запуск с разными аргументами, какие актуальны
Остановка разными способами

запуск erl выход C-G q help()

b() – display all variable bindings f() – forget all variable bindings f(X) – forget the binding of variable X c(File) – compile and load code in <File> l(Module) – load or reload module flush() – flush any messages sent to the shell q() – quit - shorthand for init:stop()

i() – information about the system i(X,Y,Z) – information about pid <X,Y,Z>Еще m() – which modules are loaded m(Mod) – information about module <Mod> memory() – memory allocation information regs() – information about registered processes

C-G User switch command –> h c [nn] - connect to job i [nn] - interrupt job k [nn] - kill job j - list all jobs s [shell] - start local shell r [node [shell]] - start remote shell q - quit erlang ? | h - this message


## типы данных
TODO пару слов, какие вообще типы есть

### integer
TODO
small int 1 байт int 4 байта small bignum 1 байт заголовок large bignum 4 байта заголовок

the number of digits that can be represented
in an integer is limited only by available memory.

1> 2#101010.
42
2> 8#0677.
447
3> 16#AE.
174

### float
TODO

IEEE 754
http://en.wikipedia.org/wiki/IEEE_floating_point

TODO показать в консоли пару примеров с ошибками округления

Erlang uses 64-bit IEEE 754-1985 floats

1.0 3.14159 -2.3e+6 23.56E-27

http://habrahabr.ru/post/112953/
Что нужно знать про арифметику с плавающей запятой

### atom
TODO
константное значение ‘Atom in single quote’

Аналог в Java – enum enum Season { WINTER, SPRING, SUMMER, AUTUMN }

Аналог в C – define define WIDTH 80

делают код более понятным:

{ok, Value}
{error, Reason}
{user, "Bob", 22}
-type(my_bool() :: true | false | undefined).

глобальная область видимости (в пределах ноды)
хранятся в таблице атомов, занимают 4 или 8 байт, эта память не освобождается (4 bytes/atom in a 32-bit system, 8 bytes/atom in a 64-bit system).

boolean() = true | false.
но из функций можно возвращать и более мнемоничные атомы
ok | error
Value | not_found
и т.д.

### tuple
TODO

group a fixed number of items into a single entity
размер известен на стадии компиляции и не меняется
аналог struct в C (и внутри виртуальной машины именно так и представлен)

Элементы обычно извлекаются паттерн матчингом, но есть функция element, иногда удобно использовать ее.

Кортеж – обычная структура данных для функциональных языков, но почти не встречается в языках императивных. Подобно списку, это набор значений, доступных по их позиции. Но кортеж имеет фиксированную длинну, не больше и не меньше. Такая структура может показаться странной, но она очень удобна в pattern matching. И поэтому используется повсемесно, так же как и сам pattern matching.

В императивных языках обычно используют словари (в Java, наример, варианты Map), там где в функциональных используют кортежи. Кортежи проигрывают в ясности значений, зато сильно выигрывают в лаконичности кода. Ну а ясность значений можно подчеркнуть там, где это необходимо

Для этого первым элементом обычно ставят атом, указывающий на суть данных:

{point, 10, 15}
{rect, {point, 10, 10}, {point, 20, 20}}
{user, 22, "Bob"}
{direction, up}
{error, "user not found"}

Это называется тэгированый кортеж.

Но все-таки кортежи сами по себе хороши для небольших объектов, на 2-4 поля. Если нужно больше полей, то тут на помощь приходит record

###  list
TODO

рабочая лошадка всех функциональных языков. Основная структура данных. А в некоторых языках (в Lisp) – вообще единственная структура данных. Представляет собой однонаправленный связанный список.

Стоимость операций:

    добавить в начало списка - 1
    добавить в конец списка - O(n)
    получить N-й элемент - O(n)
    конкатернация двух списков - O(n)

Поэтому всегда работают с началом списка. А конкатернации можно и нужно избегать, используя вместо нее io_list.

Могут быть какой угодно вложенности

[1,2,[1,2,3],3]

Могут быть правильные (proper) и неправильные (nonproper)

[1,2,3]
[1 | [2,3]]
[1,2 | [3]]
[1,2 | 3]

### array
TODO

Это не настоящие массивы, как в императивных языках. В них нет главной фишки массивов: доступа к любому элементу за константное время, и изменения любого элемента за константное время.

Более того любая модификация потребует полного копирования массива и, соответственно, времени O(n).

Поэтому эти массивы нигде не используются :)

### record
TODO

Представляет собой синтаксический сахар, добавленный в язык позже. На этапе компиляции превращается в обычный кортеж и имена полей теряются. Зато принципы работы с ними вполне аналогичны работе со словорями в других языках.

-record(user, {id, name, age}). % {user, undefined, undefined, undefined}
-record(user, {id = 0, name = "", age = 0}). % {user, 0, "", 0}
-record(user, {
    id = 0 :: user_id(),
    name = "" :: string(),
    age = 0 :: age()
    }).

User = #user{}
User = #user{id = 5}
User = #user{id = 5, name = "Bob", age = 24}
UserId = User#user.id
UserName = User#user.name
User2 = User#user{id = 7}.

Функции для работы с записями из консоли: rd(R,D) – define a record rl() – display all record information rr(File) – read record information from File (wildcards allowed)

2> rd(user, {id, name, age}).
user
3> rl().
-record(user,{id,name,age}).
ok
4> {user, 5, "Bob", 23}.
#user{id = 5,name = "Bob",age = 23}
5> Bob = #user{id = 6, name = "Bob"}.
#user{id = 6,name = "Bob",age = undefined}
6> Bill = Bob#user{name = "Bill"}.
#user{id = 6,name = "Bill",age = undefined}
7> Bob#user.id.

### прочие типы
TODO
fun, ref, port и т.д.

number < atom < reference < fun < port < pid < tuple (and record) < map < list < binary
