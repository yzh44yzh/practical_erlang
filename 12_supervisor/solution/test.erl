1> main_sup:start_link().
main_sup init <0.36.0>
sup_1 init <0.37.0>
worker worker_1:<0.38.0> init
worker worker_2:<0.39.0> init
sup_2 init <0.40.0>
worker worker_3:<0.41.0> init
worker worker_4:<0.42.0> init
{ok,<0.36.0>}


2> supervisor:which_children(sup_2).
[{worker_4,<0.42.0>,supervisor,[worker]},
 {worker_3,<0.41.0>,supervisor,[worker]}]
3> sup_2:remove_worker(worker_3).
ok
4> supervisor:which_children(sup_2).
[{worker_4,<0.42.0>,supervisor,[worker]}]
5> sup_2:add_worker(worker_5).
worker worker_5:<0.47.0> init
{ok,<0.47.0>}
6> sup_2:add_worker(worker_6).
worker worker_6:<0.49.0> init
{ok,<0.49.0>}
7> sup_2:add_worker(worker_7).
worker worker_7:<0.51.0> init
{ok,<0.51.0>}
8> supervisor:which_children(sup_2).
[{worker_7,<0.51.0>,supervisor,[worker]},
 {worker_6,<0.49.0>,supervisor,[worker]},
 {worker_5,<0.47.0>,supervisor,[worker]},
 {worker_4,<0.42.0>,supervisor,[worker]}]
9> sup_2:remove_worker(worker_6).
ok
10> supervisor:which_children(sup_2).
[{worker_7,<0.51.0>,supervisor,[worker]},
 {worker_5,<0.47.0>,supervisor,[worker]},
 {worker_4,<0.42.0>,supervisor,[worker]}]


11> worker:ping(worker_1).
{worker_1,<0.38.0>}
12> worker:ping(worker_2).
{worker_2,<0.39.0>}
13> worker:ping(worker_4).
{worker_4,<0.42.0>}
14> worker:ping(worker_5).
{worker_5,<0.47.0>}
15> worker:ping(worker_7).
{worker_7,<0.51.0>}

16> supervisor:which_children(sup_1).
[{worker_2,<0.39.0>,supervisor,[worker]},
 {worker_1,<0.38.0>,supervisor,[worker]}]
17> supervisor:which_children(main_sup).
[{child_sup_2,<0.40.0>,supervisor,[sup_2]},
 {child_sup_1,<0.37.0>,supervisor,[sup_1]}]
