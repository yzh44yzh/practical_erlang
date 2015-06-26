-module(short_link).
-behavior(gen_server).

-export([start_link/0, create_short/1, get_long/1, rand_str/1, fill_with_data/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("otp_types.hrl").

-record(state, {size :: integer(),
                ttl :: integer()}).

%%% module API

-spec start_link() -> gs_start_link_reply().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

fill_with_data() ->
    lists:map(fun  create_short/1,
              ["google.com", "yandex.ru", "wargaming.net", "tut.by"]).

create_short(LongLink) ->
    gen_server:call(?MODULE, {create_short, LongLink}).


get_long(ShortLink) ->
    gen_server:call(?MODULE, {get_long, ShortLink}).


%%% gen_server API

-spec init(gs_args()) -> gs_init_reply().
init([]) ->
    lager:info("short_link started ~p", [self()]),
    ets:new(short2long, [named_table]),
    ets:new(long2short, [named_table]),
    Size = application:get_env(short_link, short_link_size, 10),
    TTL = application:get_env(short_link, short_link_ttl, 48),
    State = #state{size = Size, ttl = TTL},
    lager:info("State:~p", [State]),
    {ok, State}.


-spec handle_call(gs_request(), gs_from(), gs_reply()) -> gs_call_reply().
handle_call({create_short, LongLink}, _From, #state{size = Size} = State) ->
    Reply = case ets:lookup(long2short, LongLink) of
                [{_, ShortLink, _}] ->
                    %% TODO ttl
                    ShortLink;
                [] -> ShortLink = rand_str(Size),
                      Time = erlang:now(),
                      ets:insert(short2long, {ShortLink, LongLink, Time}),
                      ets:insert(long2short, {LongLink, ShortLink, Time}),
                      ShortLink
            end,
    {reply, Reply, State};

handle_call({get_long, ShortLink}, _From, State) ->
    Reply = case ets:lookup(short2long, ShortLink) of
                [] -> {error, not_found};
                [{_, LongLink, _}] ->
                    %% TODO ttl
                    {ok, LongLink}
            end,
    {reply, Reply, State};

handle_call(Any, _From, State) ->
    lager:error("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


-spec handle_cast(gs_request(), gs_state()) -> gs_cast_reply().
handle_cast(Any, State) ->
    lager:error("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


-spec handle_info(gs_request(), gs_state()) -> gs_info_reply().
handle_info(Request, State) ->
    lager:error("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


-spec terminate(terminate_reason(), gs_state()) -> ok.
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(), term(), term()) -> gs_code_change_reply().
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.



%%% inner functions

%% generates random string of chars [a-zA-Z0-9]
rand_str(Length) ->
    Str = lists:map(fun(Char) when Char > 83 -> Char + 13;
                       (Char) when Char > 57 -> Char + 7;
                       (Char) -> Char
                    end,
                    [crypto:rand_uniform(48, 110) || _ <- lists:seq(1, Length)]),
    unicode:characters_to_binary(Str).
