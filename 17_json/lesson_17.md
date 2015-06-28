# Работа с JSON

## jiffy

https://github.com/davisp/jiffy

Содержит код на С.

см README
в т.ч. Data Format

```
git clone https://github.com/davisp/jiffy
cd jiffy
make
```

json/try_json.erl

```erlang
1> Data = try_json:init().
{[{<<"doc">>,<<"list of users">>}, ...
2> Bin = jiffy:encode(Data).
<<"{\"doc\":\"list of users\", ...
3> jiffy:decode(Bin).
{[{<<"doc">>,<<"list of users">>}, ...
```

## ej

https://github.com/seth/ej

Аналог xpath для XML

```
git clone https://github.com/seth/ej
cd ej
make
```

```
5> ej:get({<<"doc">>}, Data).
<<"list of users">>
6> ej:get({<<"version">>}, Data).
12
7> ej:get({<<"users">>, 1}, Data).
{[{<<"id">>,1},
  {<<"name">>,<<"Bob">>},
  {<<"gender">>,male},
  {<<"age">>,22}]}
8> ej:get({<<"users">>, 1, <<"name">>}, Data).
<<"Bob">>
9> ej:set({<<"users">>, 1, <<"name">>}, Data, <<"David">>).
{[{<<"doc">>,<<"list of users">>} ...
```

## jessy

https://github.com/klarna/jesse

реализует json-schema Draft 03.
Есть Draft 04, он лучше, но либой не поддерживается.

```
git clone https://github.com/klarna/jesse
cd jesse
make
```

```
{ok, Bin} = file:read_file("my_schema.json").
MySchema = jiffy:decode(Bin).
jesse:add_schema(my_schema, MySchema).
JSON1 = {[{<<"states">>, <<"read,hidden">>}, {<<"page">>, 1}]}.
JSON2 = {[{<<"states">>, <<"read,hidden">>}, {<<"page">>, -5}, {<<"some">>, <<"val">>}]}.
jesse:validate(my_schema, JSON1).
jesse:validate(my_schema, JSON2).
```
