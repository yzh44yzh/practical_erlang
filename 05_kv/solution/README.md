## Сервис сокращения ссылок

А сегодня мы создадим сервис сокращения ссылок.

Этот сервис получает какую-нибудь длинную ссылку, например
https://hexlet.io/tracks/fundamental/lesson_sets/sicp и генерирует для
нее короткую ссылку, например http://hexlet.io/EUjk9J7J И, наоборот,
короткую ссылку он умеет заменять на длинную.

Вам нужно будет реализовать модуль **short_link** с 3-мя функциями:
**init/0**, **create_short/2**, **get_long/2**.

**init/0** создает и возвращает структуру данных, которая будет
хранить всю информацию о коротких и длинных ссылках. Какая структура
тут лучше всего подходит, это решать вам :). Ниже будем называть эту
структуру **State**.

**create_short/2** получает на вход длинную ссылку и State, на выходе
отдает короткую ссылку и измененный State.  Для одной и той же длинной
ссылки всегда должна отдаваться одна и та же короткая
ссылка. Например:

```
3> {HexletLink, State2} = short_link:create_short("http://hexlet.io", State1).
{"http://hexlet.io/EUjk9J7J", ...}
4> {HexletLink, State3} = short_link:create_short("http://hexlet.io", State2).
{"http://hexlet.io/EUjk9J7J", ...}
```

**get_long/2** на вход принимает короткую ссылку и State, на выходе
отдает либо кортеж {ok, LongLink}, если ссылка найдена в State, либо
{error, not_found}, если ссылка не найдена. Например:

```
7> short_link:get_long("http://hexlet.io/EUjk9J7J", State4).
{ok,"http://hexlet.io"}
8> short_link:get_long("bla-bla-bla", State4).
{error,not_found}
```

В целом работа пользователя с модулем **short_link** должна выглядеть
примерно так:

```
2> State1 = short_link:init().
3> {HexletLink, State2} = short_link:create_short("http://hexlet.io", State1).
{"http://hexlet.io/EUjk9J7J", ...}
4> {HexletLink, State3} = short_link:create_short("http://hexlet.io", State2).
{"http://hexlet.io/EUjk9J7J", ...}
5> {CourseraLink, State4} = short_link:create_short("http://coursera.org", State3).
{"http://hexlet.io/WqLURDm9", ...}
6> short_link:get_long("http://hexlet.io/WqLURDm9", State4).
{ok,"http://coursera.org"}
7> short_link:get_long("http://hexlet.io/EUjk9J7J", State4).
{ok,"http://hexlet.io"}
8> short_link:get_long("bla-bla-bla", State4).
{error,not_found}
```

Еще вам понадобится функция **rand_str/1** для генерации случайных
строк заданной длинны.  Она уже реализована в модуле, и ей можно
пользоваться:

```
3> short_link:rand_str(8).
"NpGJukjT"
4> short_link:rand_str(8).
"PVBkEUxs"
5> short_link:rand_str(8).
"9YKlO0cx"
6> short_link:rand_str(20).
"hm2UFnEOPXCGJl4yPds2"
7> short_link:rand_str(20).
"19JkysY6CBm0KspAWK2N"
```
