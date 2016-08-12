-module(wg_push_sender).
-behavior(gen_server).

-include("wg_push.hrl").

-export([start_link/0, set_apns_host_port/2, send_message/2, send_messages/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([get_connection/2, send/2, parse_reply/1]). %% inner functions exported for testing


-record(state, {
        apns_host :: string(),
        apns_port :: integer(),
        connections = orddict:new() :: orddict:orddict(file:name_all(), port())
         }).


%%% module API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec set_apns_host_port(string(), integer()) -> ok.
set_apns_host_port(Host, Port) ->
    gen_server:call(?MODULE, {set_apns_host_port, Host, Port}).


-spec send_message(#wg_push_item{}, #wg_push_ssl_options{}) -> ok | {error, atom(), term()}.
send_message(Message, SSL_Options) ->
    send_messages([Message], SSL_Options).


-spec send_messages([#wg_push_item{}], #wg_push_ssl_options{}) -> ok | {error, atom(), term()}.
send_messages(Messages, SSL_Options) ->
    gen_server:call(?MODULE, {send_messages, Messages, SSL_Options}).


%%% gen_server API

init([]) ->
    {ok, #state{
            apns_host = application:get_env(wg_push, apns_host, "gateway.sandbox.push.apple.com"),
            apns_port = application:get_env(wg_push, apns_port, 2196)
           }}.


handle_call({set_apns_host_port, Host, Port}, _From, State) ->
    State2 = State#state{apns_host = Host, apns_port = Port},
    {reply, ok, State2};


handle_call({send_messages, Messages, SSL_Options}, _From, State) ->
    {Reply, State3} = send_messages(Messages, SSL_Options, State),
    {reply, Reply, State3};

handle_call(_Any, _From, State) ->
    {noreply, State}.


handle_cast(_Any, State) ->
    {noreply, State}.


handle_info(_Request, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVersion, State, _Extra) ->
    {ok, State}.



%%% inner functions
-spec send_messages([#wg_push_item{}], #wg_push_ssl_options{}, #state{}) ->
                           {ok, #state{}} | {{error, atom(), term()}, #state{}}.
send_messages([], _SSL_Options, State) ->
    {ok, State};
send_messages(Messages, #wg_push_ssl_options{certfile = CertFile} = SSL_Options,
              #state{connections = Connections} = State) ->
    case get_connection(SSL_Options, State) of
        {ok, Socket, State2} ->
            case send(Socket, Messages) of
                ok -> {ok, State2};
                {item_error, ItemID, shutdown} ->
                    %% TODO need exponential backoff
                    %% Should it be done at library level or at user code level?
                    ssl:close(Socket),
                    NewState = State#state{connections = orddict:erase(CertFile, Connections)},
                    RestMessages = remove_sent_messages(Messages, ItemID),
                    send_messages(RestMessages, SSL_Options, NewState);
                {item_error, _ItemID, Reason} ->
                    %% TODO need separate result for each message.
                    %% Messages before ItemID are sent successfully
                    %% Message with ItemID failed with Reason
                    %% Messages after ItemID were not sent.
                    ssl:close(Socket),
                    {{error, reply, Reason}, State2#state{connections = orddict:erase(CertFile, Connections)}};
                {error, Stage, Reason} ->
                    {{error, Stage, Reason}, State2#state{connections = orddict:erase(CertFile, Connections)}}
            end;
        {error, Reason} -> {{error, no_connection, Reason}, State}
    end.


-spec get_connection(#wg_push_ssl_options{}, #state{}) -> {ok, port(), #state{}} | {error, term()}.
get_connection(#wg_push_ssl_options{certfile = CertFile} = SSL_Options,
               #state{apns_host = Host, apns_port = Port, connections = Connections} = State) ->
    Res = case orddict:find(CertFile, Connections) of
              error -> need_new_socket;
              {ok, Socket} ->
                  case ssl:connection_information(Socket) of
                      {ok, _} -> {ok, Socket};
                      {error, _Reason} ->
                          ssl:close(Socket),
                          need_new_socket
                  end
          end,
    case Res of
        {ok, OldSocket} -> {ok, OldSocket, State};
        need_new_socket ->
            case open_connection(Host, Port, SSL_Options) of
                {ok, NewSocket} ->
                    State2 = State#state{connections = orddict:store(CertFile, NewSocket, Connections)},
                    {ok, NewSocket, State2};
                {error, Reason} -> {error, Reason}
            end
    end.


-spec open_connection(string(), integer(), #wg_push_ssl_options{}) -> {ok, port()} | {error, term()}.
open_connection(Host, Port, SSL_Options) ->
    Options = [{active, false}, binary] ++ wg_push_pack:build_ssl_options(SSL_Options),
    case ssl:connect(Host, Port, Options) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} -> {error, Reason}
    end.


-spec send(port(), [#wg_push_item{}]) -> ok | {error, atom(), term()} | {item_error, integer(), atom()}.
send(Socket, Messages) ->
    case wg_push_pack:pack_items(Messages) of
        {ok, Bin} ->
            case ssl:send(Socket, Bin) of
                ok ->
                    {ok, Timeout} = application:get_env(wg_push, wait_apns_reply_timeout),
                    case ssl:recv(Socket, 6, Timeout) of
                        {ok, Bin2} -> parse_reply(Bin2);
                        {error, timeout} -> ok; %% Messages are sent successfully
                        {error, closed} -> ok;  %% Messages are sent successfully
                        {error, Reason} -> {error, reply, Reason}
                    end;
                {error, Reason} -> {error, send, Reason}
            end;
        {error, _ItemId, Reason} -> {error, pack, Reason}
    end.

remove_sent_messages(Messages, ItemID) ->
    case lists:dropwhile(fun(#wg_push_item{id = Id}) -> Id /= ItemID end, Messages) of
        [] -> [];
        [_LastSent | T] -> T
    end.

parse_reply(<<8, 0, _ItemID/binary>>) -> ok;
parse_reply(<<8, 1, ItemID/binary>>) -> {item_error, ItemID, processing_error};
parse_reply(<<8, 2, ItemID/binary>>) -> {item_error, ItemID, missing_device_token};
parse_reply(<<8, 3, ItemID/binary>>) -> {item_error, ItemID, missing_topic};
parse_reply(<<8, 4, ItemID/binary>>) -> {item_error, ItemID, missing_payload};
parse_reply(<<8, 5, ItemID/binary>>) -> {item_error, ItemID, invalid_token_size};
parse_reply(<<8, 6, ItemID/binary>>) -> {item_error, ItemID, invalid_topic_size};
parse_reply(<<8, 7, ItemID/binary>>) -> {item_error, ItemID, invalid_payload_size};
parse_reply(<<8, 8, ItemID/binary>>) -> {item_error, ItemID, invalid_token};
parse_reply(<<8, 10, ItemID/binary>>) -> {item_error, ItemID, shutdown};
parse_reply(<<8, 255, ItemID/binary>>) -> {item_error, ItemID, unknown_error};
parse_reply(_) -> {error, reply, unknown_reply}.
