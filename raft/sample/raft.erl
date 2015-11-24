-module(raft).
-behaviour(gen_fsm).

-export([start/0, event/1]).
-export([init/1, follower/2, candidat/2, leader/2]).

-define(APPEND_LOG_TIMEOUT, 1000).
-define(ELECTION_TIMEOUT, 4000).

-define(CLUSTER, ['alpha@127.0.0.1',
                  'bravo@127.0.0.1',
                  'charlie@127.0.0.1',
                  'delta@127.0.0.1',
                  'echo@127.0.0.1']).


-record(state, {
          term = 0 :: integer(),
          votes = 0 :: integer(),
          voted_candidat :: atom(),
          append_log_timeout :: reference(),
          election_timeout :: reference()
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
    gen_fsm:cancel_timer(Ref2), %% TEMP
    {next_state, follower, State#state{election_timeout = Ref2}};

follower({timeout, Ref, election}, #state{election_timeout = Ref} = State) ->
    log("follower state, election event, go to candidat state"),
    gen_fsm:send_event(?MODULE, start_election),
    {next_state, candidat, State#state{election_timeout = undefined, votes = 0}};

follower(Event, State) ->
    log("unknown event in the follower state ~p, ~p", [Event, State]),
    {next_state, follower, State}.


candidat(start_election, #state{term = Term} = State) ->
    log("candidat state, start election"),
    Term2 = Term + 1,
    broadcast({request_vote, node(), Term2}),
    %% TODO run election timeout timer
    %% timeouts are chosen randomly from a fixed interval (e.g., 150–300ms)
    {next_state, candidat, State#state{term = Term2, votes = 1, voted_candidat = node()}};

candidat(vote, #state{votes = Votes} = State) ->
    Votes2 = Votes + 1,
    Majority = length(?CLUSTER) div 2 + 1,
    log("got one vote, votes:~p majority:~p", [Votes2, Majority]),
    if
        Votes2 >= Majority ->
            log("got majority, go to leader state"),
            gen_fsm:send_event(?MODULE, start_as_leader),
            {next_state, leader, State#state{votes = Votes2}};
        true ->
            %% wait for more votes
            {next_state, candidat, State#state{votes = Votes2}}
    end;

candidat({request_vote, _FromCandidat, _Term}, State) ->
    %% TODO check term
    %% ignore candidat with lower or same term
    %% go to follower if candidat have higher term
    {next_state, candidat, State};

candidat({append_entries, _Term, _Data}, State) ->
    %% go to follower if data have higher or same term
    {next_state, candidat, State};

candidat(Event, State) ->
    log("unknown event in the candidat state ~p, ~p", [Event, State]),
    {next_state, candidat, State}.


leader(start_as_leader, #state{term = Term} = State) ->
    log("start as leader"),
    broadcast({append_entries, Term, 1}),
    %% TODO repeat broadcast with APPEND_LOG_TIMEOUT
    {next_state, leader, State};

leader({request_vote, _FromCandidat, _Term}, State) ->
    %% TODO go to follower if candidat have higher term
    {next_state, leader, State};

leader({append_entries, _Term, _Data}, State) ->
    %% TODO go to follower if data have higher term
    %% TODO discover other leader with higher term -> [Candidat]
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
    gen_fsm:cancel_timer(Ref),
    gen_fsm:start_timer(?ELECTION_TIMEOUT, election).


log(Msg) ->
    io:format(Msg ++ "~n").
log(Msg, Params) ->
    io:format(Msg ++ "~n", Params).
