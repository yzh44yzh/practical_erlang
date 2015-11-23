-module(raft).
-behaviour(gen_fsm).

-export([start/0, request_vote/1, append_entries/1]).
-export([init/1, follower/2, candidat/2, leader/2]).

-define(APPEND_LOG_TIMEOUT, 2000).
-define(NO_LEADER_TIMEOUT, 5000).

-define(CLUSTER, ['alpha@127.0.0.1',
                  'bravo@127.0.0.1',
                  'charlie@127.0.0.1',
                  'delta@127.0.0.1',
                  'echo@127.0.0.1']).


-record(state, {append_log_timeout :: reference(),
                no_leader_timeout :: reference()
               }).

%%% Module API

start() ->
    Self = node(),
    log("start"),
    Cluster = [{Node, net_adm:ping(Node)} || Node <- ?CLUSTER, Node /= Self],
    log("cluster:~p", [Cluster]),
    {ok, _Pid} = gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []),
    Cluster.


request_vote(FromCandidat) ->
    gen_fsm:send_event(?MODULE, {request_vote, FromCandidat}).


append_entries(Data) ->
    gen_fsm:send_event(?MODULE, {append_entries, Data}).


%%% FSM behaviour

init([]) ->
    log("raft fsm init, go to follower state"),
    Ref = gen_fsm:start_timer(?NO_LEADER_TIMEOUT, no_leader),
    {ok, follower, #state{no_leader_timeout = Ref}}.


follower({request_vote, _FromCandidat}, #state{no_leader_timeout = Ref} = State) ->
    %% TODO vote for candidat
    gen_fsm:cancel_timer(Ref),
    Ref2 = gen_fsm:start_timer(?NO_LEADER_TIMEOUT, no_reply_from_leader),
    {next_state, follower, State#state{no_leader_timeout = Ref2}};

follower({append_entries, _Data}, #state{no_leader_timeout = Ref} = State) ->
    %% wait for Append-Entries during  ?TIMEOUT * 2
    gen_fsm:cancel_timer(Ref),
    Ref2 = gen_fsm:start_timer(?NO_LEADER_TIMEOUT, no_reply_from_leader),
    {next_state, follower, State#state{no_leader_timeout = Ref2}};

follower({timeout, Ref, no_leader}, #state{no_leader_timeout = Ref} = State) ->
    log("follower state, no_leader event, go to candidat state"),
    gen_fsm:send_event(?MODULE, start_election),
    {next_state, candidat, State#state{no_leader_timeout = undefined}};

follower(Event, State) ->
    log("unknown event in the follower state ~p, ~p", [Event, State]),
    {next_state, follower, State}.


candidat(start_election, State) ->
    log("candidat state, start election"),
    %% - receive votes from majority of servers -> [Leader]
    %% - timeout, new election -> [Candidat]
    %% - discover current leader or new term -> [Follower]
    {next_state, candidat, State};


candidat({request_vote, _FromCandidat}, State) ->
    %% TODO check term
    %% ignore candidat with lower or same term
    %% go to follower if candidat have higher term
    {next_state, candidat, State};

candidat({append_entries, _Data}, State) ->
    %% go to follower if data have higher term
    {next_state, candidat, State};

candidat(Event, State) ->
    log("unknown event in the candidat state ~p, ~p", [Event, State]),
    {next_state, candidat, State}.


leader({request_vote, _FromCandidat}, State) ->
    %% go to follower if candidat have higher term
    {next_state, leader, State};

leader({append_entries, _Data}, State) ->
    %% go to follower if data have higher term
    %% broadcast Append-Entries every ?TIMEOUT
    %% - discover other leader with higher term -> [Candidat]
    {next_state, leader, State};

leader(Event, State) ->
    log("unknown event in the leader state ~p, ~p", [Event, State]),
    {next_state, leader, State}.


%%% inner functions

broadcast(Event) ->
    %% TODO rpc to all cluster
    ok.

log(Msg) ->
    io:format("~p: " ++ Msg ++ "~n", [node()]).
log(Msg, Params) ->
    io:format("~p: " ++ Msg ++ "~n", [node() | Params]).
