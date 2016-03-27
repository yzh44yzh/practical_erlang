%% -*- mode: Erlang;-*-
{application, mcache,
 [
  {description, "memcached like service"},
  {vsn, "1"},
  {modules, [mcache_app, mcache_sup, mcache]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {mcache_app, []}},
  {env, []}
 ]}.
