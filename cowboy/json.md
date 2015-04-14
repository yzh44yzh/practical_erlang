# Работа с JSON

## jiffy

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

```
git clone https://github.com/seth/ej
cd ej
make
```

```erlang
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
