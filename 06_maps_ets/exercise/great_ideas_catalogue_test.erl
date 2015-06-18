-module(great_ideas_catalogue_test).

-include_lib("eunit/include/eunit.hrl").


setup() ->
    great_ideas_catalogue:init().


teardown() ->
    ets:delete(great_ideas_table).


add_idea_test() ->
    setup(),
    ?assertEqual(10, ets:info(great_ideas_table, size)),
    great_ideas_catalogue:add_idea(11, "some idea", "Bob", 100, "some description"),
    ?assertEqual(11, ets:info(great_ideas_table, size)),
    great_ideas_catalogue:add_idea(20, "other idea", "Bill", 100, "other description"),
    ?assertEqual(12, ets:info(great_ideas_table, size)),
    great_ideas_catalogue:add_idea(20, "other idea", "Bill", 500, "updated description"),
    ?assertEqual(12, ets:info(great_ideas_table, size)),
    teardown(),
    ok.


get_idea_test() ->
    setup(),
    ?assertEqual({ok, {idea, 1, "Мороженое с огурцами", "Боб Бобов", 100,
                       "Крошим огурцы кубиками и добавляем в мороженое"}},
                 great_ideas_catalogue:get_idea(1)),
    ?assertEqual(not_found, great_ideas_catalogue:get_idea(777)),
    great_ideas_catalogue:add_idea(777, "some idea", "Bob", 100, "some description"),
    ?assertEqual({ok, {idea, 777, "some idea", "Bob", 100, "some description"}},
                 great_ideas_catalogue:get_idea(777)),
    teardown(),
    ok.


ideas_by_author_test() ->
    setup(),
    ?assertEqual([{idea,1,"Мороженое с огурцами","Боб Бобов",100,
                   "Крошим огурцы кубиками и добавляем в мороженое"},
                  {idea,4,"Куртка с тремя рукавами","Боб Бобов",15,
                   "Рукава из разных материалов, расчитаны на разную погоду."},
                  {idea,7,"Вулканический зонт","Боб Бобов",12,
                   "Защищает самолеты от вулканической пыли."}],
                 lists:sort(great_ideas_catalogue:ideas_by_author("Боб Бобов"))),
    ?assertEqual([{idea,3,"Извлечение энергии квазаров","П. И. Шурупов",
                   100500,"Секретно"},
                  {idea,9,"Автоматическая кормушка для котов","П. И. Шурупов",
                   9000,"Нужно использовать энергию квазаров для этой цели"}],
                 lists:sort(great_ideas_catalogue:ideas_by_author("П. И. Шурупов"))),
    ?assertEqual([], great_ideas_catalogue:ideas_by_author("Бил Билов")),
    teardown(),
    ok.


ideas_by_rating_test() ->
    setup(),
    ?assertEqual([{idea,3,"Извлечение энергии квазаров","П. И. Шурупов",
                   100500,"Секретно"}],
                 great_ideas_catalogue:ideas_by_rating(100500)),
    ?assertEqual([{idea,3,"Извлечение энергии квазаров","П. И. Шурупов",
                   100500,"Секретно"},
                  {idea,8,"Телефон-шар","Див Стобс",8383,
                   "Удобно лежит в руке, имеет устройство ввода на основе гироскопа"},
                  {idea,9,"Автоматическая кормушка для котов","П. И. Шурупов",
                   9000,"Нужно использовать энергию квазаров для этой цели"}],
                 lists:sort(great_ideas_catalogue:ideas_by_rating(1000))),
    teardown(),
    ok.


get_authors_test() ->
    setup(),
    ?assertEqual([{"Боб Бобов",3},
                  {"П. И. Шурупов",2},
                  {"Алекс Аквамаринов",1},
                  {"Билл Билов",1},
                  {"Васисуалий Л.",1},
                  {"Див Стобс",1},
                  {"Олечка",1}],
                 great_ideas_catalogue:get_authors()),
    teardown(),
    ok.
