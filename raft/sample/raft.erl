-module(raft).
-behaviour(gen_fsm).

-export([start/0, event/1]).
-export([init/1, follower/2, candidat/2, leader/2]).

-define(ELECTION_TIMEOUT, 4000).
-define(WAIT_FOR_VOTES_TIMEOUT, 2000).
-define(APPEND_ENTRIES_TIMEOUT, 1000).

-define(CLUSTER, ['alpha@127.0.0.1',
                  'bravo@127.0.0.1',
                  'charlie@127.0.0.1',
                  'delta@127.0.0.1']).

-record(state, {
          term = 0 :: integer(),
          log_id = 0 :: integer(),
          votes = 0 :: integer(),
          current_leader :: node(),
          election_timeout :: reference(),
          wait_for_votes_timeout :: reference(),
          append_entries_timeout :: reference()
         }).


%%% Module API

start() ->
    Self = node(),
    io:format("start~n"),
    Cluster = [{Node, net_adm:ping(Node)} || Node <- ?CLUSTER, Node /= Self],
    io:format("join to cluster:~p~n", [Cluster]),
    {ok, _Pid} = gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []),
    Cluster.


event(Event) ->
    gen_fsm:send_event(?MODULE, Event).


%%% FSM behaviour

init([]) ->
    io:format("raft fsm init, go to follower state~n"),
    Ref = gen_fsm:start_timer(?ELECTION_TIMEOUT, election),
    {ok, follower, #state{election_timeout = Ref}}.


follower({request_vote, Candidat, CTerm}, #state{term = Term,
                                                 current_leader = CurrLeader,
                                                 election_timeout = Ref} = State) ->
    io:format("follower got request_vote from ~p with term ~p~n", [Candidat, CTerm]),
    State2 = if
                 CTerm < Term ->
                     io:format("obsolete term, ignore~n"),
                     State;
                 CTerm == Term ->
                     case CurrLeader of
                         undefined ->
                             io:format("same term, not voted, vote for ~p~n", [Candidat]),
                             rpc:cast(Candidat, ?MODULE, event, [vote]),
                             State#state{current_leader = Candidat};
                         _ ->
                             io:format("same term, but already voted~n"),
                             State
                     end;
                 CTerm > Term ->
                     io:format("new term, vote for ~p~n", [Candidat]),
                     rpc:cast(Candidat, ?MODULE, event, [vote]),
                     State#state{current_leader = Candidat, term = CTerm}
             end,
    Ref2 = restart_election_timer(Ref),
    {next_state, follower, State2#state{election_timeout = Ref2}};

follower({append_entries, CTerm, Data, Leader}, #state{term = Term, election_timeout = Ref} = State) ->
    if
        CTerm >= Term ->
            io:format("follower got append_entries with Term:~p and Data:~p~n", [CTerm, Data]),
            %% do something with Data
            Ref2 = restart_election_timer(Ref),
            {next_state, follower, State#state{election_timeout = Ref2, term = CTerm, current_leader = Leader}};
        true ->
            %% ingore data
            {next_state, follower, State}
    end;

follower({timeout, Ref, election}, #state{election_timeout = Ref} = State) ->
    io:format("follower state, election event, go to candidat state~n"),
    gen_fsm:send_event(?MODULE, start_election),
    {next_state, candidat, State#state{votes = 0}};

follower(Event, State) ->
    io:format("unknown event in the follower state ~p, ~p~n", [Event, State]),
    {next_state, follower, State}.


candidat(start_election, #state{term = Term} = State) ->
    io:format("candidat state, start election~n"),
    Term2 = Term + 1,
    broadcast({request_vote, node(), Term2}),
    Ref = set_wait_for_votes_timer(),
    {next_state, candidat, State#state{term = Term2, votes = 1,
                                       current_leader = node(),
                                       wait_for_votes_timeout = Ref
                                      }};

candidat(vote, #state{votes = Votes, wait_for_votes_timeout = Ref} = State) ->
    Votes2 = Votes + 1,
    Majority = length(?CLUSTER) div 2 + 1,
    io:format("got one vote, votes:~p majority:~p~n", [Votes2, Majority]),
    if
        Votes2 >= Majority ->
            io:format("got majority, go to leader state~n"),
            safe_cancel_timer(Ref),
            gen_fsm:send_event(?MODULE, start_as_leader),
            {next_state, leader, State#state{votes = Votes2}};
        true ->
            %% wait for more votes
            {next_state, candidat, State#state{votes = Votes2}}
    end;

candidat({timeout, Ref, wait_for_votes}, #state{wait_for_votes_timeout = Ref} = State) ->
    io:format("wait_for_votes timeout~n"),
    gen_fsm:send_event(?MODULE, start_election),
    {next_state, candidat, State#state{votes = 0}};

candidat({request_vote, Candidat, CTerm}, #state{term = Term, wait_for_votes_timeout = Ref} = State) ->
    if
        CTerm > Term ->
            io:format("Candidat with higher term, vote for ~p~n", [Candidat]),
            safe_cancel_timer(Ref),
            rpc:cast(Candidat, ?MODULE, event, [vote]),
            {next_state, follower, State#state{term = CTerm, current_leader = Candidat}};
        true ->
            {next_state, candidat, State}
    end;

candidat({append_entries, CTerm, _Data, Leader},
         #state{term = Term, wait_for_votes_timeout = Ref} = State) ->
    if
        CTerm >= Term ->
            io:format("Leader with higher term~n"),
            safe_cancel_timer(Ref),
            {next_state, follower, State#state{term = CTerm, current_leader = Leader}};
        true ->
            {next_state, candidat, State}
    end;

candidat(Event, State) ->
    io:format("unknown event in the candidat state ~p, ~p~n", [Event, State]),
    {next_state, candidat, State}.


leader(start_as_leader, #state{term = Term, log_id = LogID} = State) ->
    io:format("start as leader~n"),
    broadcast({append_entries, Term, LogID + 1, node()}),
    Ref = gen_fsm:start_timer(?APPEND_ENTRIES_TIMEOUT, broadcast_append_entries),
    {next_state, leader, State#state{log_id = LogID + 1, append_entries_timeout = Ref}};

leader({timeout, Ref, broadcast_append_entries},
       #state{term = Term, log_id = LogID, append_entries_timeout = Ref} = State) ->
    broadcast({append_entries, Term, LogID + 1, node()}),
    Ref2 = gen_fsm:start_timer(?APPEND_ENTRIES_TIMEOUT, broadcast_append_entries),
    {next_state, leader, State#state{log_id = LogID + 1, append_entries_timeout = Ref2}};

leader({request_vote, Candidat, CTerm}, #state{term = Term, append_entries_timeout = Ref} = State) ->
    if
        CTerm > Term ->
            io:format("Candidat with higher term, vote for ~p~n", [Candidat]),
            safe_cancel_timer(Ref),
            rpc:cast(Candidat, ?MODULE, event, [vote]),
            {next_state, follower, State#state{term = CTerm, current_leader = Candidat}};
        true ->
            {next_state, leader, State}
    end;

leader({append_entries, CTerm, _Data, Leader}, #state{term = Term, append_entries_timeout = Ref} = State) ->
    if
        CTerm >= Term ->
            io:format("Leader with higher term~n"),
            safe_cancel_timer(Ref),
            {next_state, follower, State#state{term = CTerm, current_leader = Leader}};
        true ->
            {next_state, leader, State}
    end;

leader(vote, State) ->
    {next_state, leader, State};

leader(Event, State) ->
    io:format("unknown event in the leader state ~p, ~p~n", [Event, State]),
    {next_state, leader, State}.


%%% inner functions

broadcast(Event) ->
    io:format("broadcast ~p~n", [Event]),
    Self = node(),
    [rpc:cast(N, ?MODULE, event, [Event]) || N <- ?CLUSTER, N /= Self],
    ok.


restart_election_timer(Ref) ->
    safe_cancel_timer(Ref),
    gen_fsm:start_timer(?ELECTION_TIMEOUT, election).


set_wait_for_votes_timer() ->
    Min = ?WAIT_FOR_VOTES_TIMEOUT div 3,
    Max = ?WAIT_FOR_VOTES_TIMEOUT,
    Time = crypto:rand_uniform(Min, Max),
    io:format("wait for votes during ~p~n", [Time]),
    gen_fsm:start_timer(Time, wait_for_votes).


safe_cancel_timer(undefined) ->
    do_nothing;
safe_cancel_timer(Ref) ->
    gen_fsm:cancel_timer(Ref).
