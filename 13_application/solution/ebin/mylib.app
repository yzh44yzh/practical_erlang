{application, mylib, [
    {description, "My Cool Lib"},
    {vsn, "0.1"},
    {modules, [mylib_app, mylib_sup, mylib_worker]},
    {registered, [mylib_sup, mylib_worker]},
    {applications, [kernel, stdlib]},
    {mod, {mylib_app, []}},
    {env, [{min_val, 2},
           {max_val, 10},
           {connection_timeout, 10000},
           {query_timeout, 10000}
          ]}
]}.
