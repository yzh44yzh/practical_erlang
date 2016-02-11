-module(main_test).

-include_lib("eunit/include/eunit.hrl").


initial_sup_tree_test() ->
    {ok, MainSup_Pid} = main_sup:start_link(),
    MainSup_Childs = supervisor:which_children(MainSup_Pid),
    ?assertMatch([{_, _, supervisor, _},
                  {_, _, supervisor, _}], MainSup_Childs),
    [{_, Sup2_Pid, supervisor, _},
     {_, Sup1_Pid, supervisor, _}] = MainSup_Childs,

    Sup1_Childs = supervisor:which_children(Sup1_Pid),
    ?assertMatch([{_, _, worker, _},
                  {_, _, worker, _}], Sup1_Childs),
    [{_, Worker2_Pid, worker, _},
     {_, Worker1_Pid, worker, _}] = Sup1_Childs,

    Sup2_Childs = supervisor:which_children(Sup2_Pid),
    ?assertMatch([{_, _, worker, _},
                  {_, _, worker, _}
                 ], Sup2_Childs),
    [{_, Worker4_Pid, worker, _},
     {_, Worker3_Pid, worker, _}] = Sup2_Childs,

    ?assertMatch({_, Worker1_Pid}, worker:ping(Worker1_Pid)),
    ?assertMatch({_, Worker2_Pid}, worker:ping(Worker2_Pid)),
    ?assertMatch({_, Worker3_Pid}, worker:ping(Worker3_Pid)),
    ?assertMatch({_, Worker4_Pid}, worker:ping(Worker4_Pid)),

    Sup2_Childs = supervisor:which_children(sup_2),
    [{Child_2, _, worker, _},
     {Child_1, _, worker, _}] = Sup2_Childs,

    Child_3 = make_ref(),
    {ok, _} = sup_2:add_worker(Child_3),
    ?assertMatch([{Child_3, _, worker, _},
                  {Child_2, _, worker, _},
                  {Child_1, _, worker, _}], supervisor:which_children(sup_2)),

    Child_4 = make_ref(),
    {ok, _} = sup_2:add_worker(Child_4),
    ?assertMatch([{Child_4, _, worker, _},
                  {Child_3, _, worker, _},
                  {Child_2, _, worker, _},
                  {Child_1, _, worker, _}], supervisor:which_children(sup_2)),

    ok = sup_2:remove_worker(Child_2),
    ?assertMatch([{Child_4, _, worker, _},
                  {Child_3, _, worker, _},
                  {Child_1, _, worker, _}], supervisor:which_children(sup_2)),

    ok = sup_2:remove_worker(Child_3),
    ?assertMatch([{Child_4, _, worker, _},
                  {Child_1, _, worker, _}], supervisor:which_children(sup_2)),

    ok = sup_2:remove_worker(Child_4),
    ?assertMatch([{Child_1, _, worker, _}], supervisor:which_children(sup_2)),

    ok.
