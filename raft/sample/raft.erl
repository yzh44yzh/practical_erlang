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
                  'delta@127.0.0.1',
                  'echo@127.0.0.1']).


-record(state, {
          term = 0 :: integer(),
          log_id = 0 :: integer(),
          votes = 0 :: integer(),
          voted_candidat :: atom(),
          election_timeout :: reference(),
          wait_for_votes_timeout :: reference(),
          append_entries_timeout :: reference()
         }).


%%% Module API

start() ->
    Self = node(),
    log("start"),
    Cluster = [{Node, net_adm:ping(Node)} || Node <- ?CLUSTER, Node /= Self],
    log("cluster:~p", [Cluster]),
    {ok, _Pid} = gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []),
    Cluster.


event(Event) ->
    gen_fsm:send_event(?MODULE, Event).


%%% FSM behaviour

init([]) ->
    log("raft fsm init, go to follower state"),
    Ref = gen_fsm:start_timer(?ELECTION_TIMEOUT, election),
    {ok, follower, #state{election_timeout = Ref}}.


follower({request_vote, Candidat, CTerm}, #state{term = Term,
                                                 voted_candidat = VotedCandidat,
                                                 election_timeout = Ref} = State) ->
    log("follower got request_vote from ~p with term ~p", [Candidat, CTerm]),
    State2 = if
                 CTerm < Term ->
                     log("obsolete term, ignore"),
                     State;
                 CTerm == Term ->
                     case VotedCandidat of
                         undefined ->
                             log("same term, not voted, vote for ~p", [Candidat]),
                             rpc:cast(Candidat, ?MODULE, event, [vote]),
                             State#state{voted_candidat = Candidat};
                         _ ->
                             log("same term, but already voted for ~p", [VotedCandidat]),
                             State
                     end;
                 CTerm > Term ->
                     log("new term, vote for ~p", [Candidat]),
                     rpc:cast(Candidat, ?MODULE, event, [vote]),
                     State#state{voted_candidat = Candidat, term = CTerm}
             end,
    Ref2 = restart_election_timer(Ref),
    {next_state, follower, State2#state{election_timeout = Ref2}};

follower({append_entries, Term, Data}, #state{election_timeout = Ref} = State) ->
    log("follower got append_entries with Term:~p and Data:~p", [Term, Data]),
    %% do something with Data
    Ref2 = restart_election_timer(Ref),
    {next_state, follower, State#state{election_timeout = Ref2}};

follower({timeout, Ref, election}, #state{election_timeout = Ref} = State) ->
    log("follower state, election event, go to candidat state"),
    gen_fsm:send_event(?MODULE, start_election),
    {next_state, candidat, State#state{votes = 0}};

follower(Event, State) ->
    log("unknown event in the follower state ~p, ~p", [Event, State]),
    {next_state, follower, State}.


candidat(start_election, #state{term = Term} = State) ->
    log("candidat state, start election"),
    Term2 = Term + 1,
    broadcast({request_vote, node(), Term2}),
    Ref = set_wait_for_votes_timer(),
    {next_state, candidat, State#state{term = Term2,
                                       votes = 1,
                                       voted_candidat = node(),
                                       wait_for_votes_timeout = Ref
                                      }};

candidat(vote, #state{votes = Votes, wait_for_votes_timeout = Ref} = State) ->
    Votes2 = Votes + 1,
    Majority = length(?CLUSTER) div 2 + 1,
    log("got one vote, votes:~p majority:~p", [Votes2, Majority]),
    if
        Votes2 >= Majority ->
            log("got majority, go to leader state"),
            safe_cancel_timer(Ref),
            gen_fsm:send_event(?MODULE, start_as_leader),
            {next_state, leader, State#state{votes = Votes2}};
        true ->
            %% wait for more votes
            {next_state, candidat, State#state{votes = Votes2}}
    end;

candidat({timeout, Ref, wait_for_votes}, #state{wait_for_votes_timeout = Ref} = State) ->
    log("wait_for_votes timeout"),
    gen_fsm:send_event(?MODULE, start_election),
    {next_state, candidat, State#state{votes = 0}};

candidat({request_vote, Candidat, CTerm}, #state{term = Term, wait_for_votes_timeout = Ref} = State) ->
    if
        CTerm > Term ->
            log("Candidat with higher term, vote for ~p", [Candidat]),
            safe_cancel_timer(Ref),
            rpc:cast(Candidat, ?MODULE, event, [vote]),
            {next_state, follower, State#state{voted_candidat = Candidat, term = CTerm}};
        true ->
            {next_state, candidat, State}
    end;

candidat({append_entries, CTerm, _Data}, #state{term = Term, wait_for_votes_timeout = Ref} = State) ->
    if
        CTerm >= Term ->
            log("Leader with higher term"),
            safe_cancel_timer(Ref),
            {next_state, follower, State#state{term = CTerm}};
        true ->
            {next_state, candidat, State}
    end;

candidat(Event, State) ->
    log("unknown event in the candidat state ~p, ~p", [Event, State]),
    {next_state, candidat, State}.


leader(start_as_leader, #state{term = Term, log_id = LogID} = State) ->
    log("start as leader"),
    broadcast({append_entries, Term, LogID + 1}),
    Ref = gen_fsm:start_timer(?APPEND_ENTRIES_TIMEOUT, broadcast_append_entries),
    {next_state, leader, State#state{log_id = LogID + 1, append_entries_timeout = Ref}};

leader({timeout, Ref, broadcast_append_entries},
       #state{term = Term, log_id = LogID, append_entries_timeout = Ref} = State) ->
    broadcast({append_entries, Term, LogID + 1}),
    Ref2 = gen_fsm:start_timer(?APPEND_ENTRIES_TIMEOUT, broadcast_append_entries),
    {next_state, leader, State#state{log_id = LogID + 1, append_entries_timeout = Ref2}};

leader({request_vote, Candidat, CTerm}, #state{term = Term, append_entries_timeout = Ref} = State) ->
    if
        CTerm > Term ->
            log("Candidat with higher term, vote for ~p", [Candidat]),
            safe_cancel_timer(Ref),
            rpc:cast(Candidat, ?MODULE, event, [vote]),
            {next_state, follower, State#state{voted_candidat = Candidat, term = CTerm}};
        true ->
            {next_state, leader, State}
    end;

leader({append_entries, CTerm, _Data}, #state{term = Term, append_entries_timeout = Ref} = State) ->
    if
        CTerm >= Term ->
            log("Leader with higher term"),
            safe_cancel_timer(Ref),
            {next_state, follower, State#state{term = CTerm}};
        true ->
            {next_state, leader, State}
    end;

leader(vote, State) ->
    {next_state, leader, State};

leader(Event, State) ->
    log("unknown event in the leader state ~p, ~p", [Event, State]),
    {next_state, leader, State}.


%%% inner functions

broadcast(Event) ->
    log("broadcast ~p", [Event]),
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
    log("wait for votes during ~p", [Time]),
    gen_fsm:start_timer(Time, wait_for_votes).


safe_cancel_timer(undefined) ->
    do_nothing;
safe_cancel_timer(Ref) ->
    gen_fsm:cancel_timer(Ref).


log(Msg) ->
    io:format(Msg ++ "~n").
log(Msg, Params) ->
    io:format(Msg ++ "~n", Params).
