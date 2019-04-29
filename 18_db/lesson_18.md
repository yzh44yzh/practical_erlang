# Работа с PostgreSQL

## PostreSQL

Воспользуемся контейнером, который использовался для тестового задания

https://github.com/wgnet/wg_forge_backend
https://github.com/wgnet/wg_forge_backend/blob/master/docker_instructions.md

```
docker run -p 5432:5432 -d yzh44yzh/wg_forge_backend_env:1.1
$ docker exec -ti <ID> bash
# psql --host=localhost --port=5432 --dbname=wg_forge_db --username=wg_forge --password
Password: a42
```

## epgsql

https://hex.pm/packages/epgsql
https://github.com/epgsql/epgsql

История форков

Синхронный и асинхронный режимы

simple query

extended query

prepared query

data representation


## epgsql_pool

https://github.com/wgnet/epgsql_pool

Пул соединений

transactions

keep alive

re-connect


## ORM

Зачем они нужны:
- DSL, скрывающий SQL, чтобы было "удобнее";
- модель данных в контексте бизнеса;
- преобразование модели бизнеса в модель БД и обратно;
- единое описание модели, без дублирования;
- миграция схемы и данных.

Обычно, когда говорят про ORM, много внимания уделяют первому пункту -- DSL. Тогда как этот DSL не особо важен, и часто, наоборот, является злом. Потому что этот уровень абстракции часто скрывает важные детали. Например, сколько запросов мы реально делаем в БД, 1 или 100? И не редко бывает глючным.

Но это зло необходимо, чтобы обеспечить все остальные пункты. И вот они гораздо важнее.

Почему в Erlang нет ORM. (Попытки есть, но нет общепринятых, известных решений).
Для этого нужно удобное метапрограммирование, в Erlang оно не удобное (parse transform).

Если бизнес-объектов (и таблиц) у вас в проекте 5-10 штук, то можно работать с ними вручную.
Если 20-100 и больше, то, скорее всего, эрланг не подходит для такого проекта.

Elixir/Ecto.
