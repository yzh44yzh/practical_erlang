-module(my_crypt).
-behavior(gen_server).

-export([start_link/0, encode/1, get_key/0, set_key/1, hash/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("otp_types.hrl").

-record(state, {
    encrypt_key :: binary(),
    hash_table :: [byte()]
}).

%%% module API

-spec start_link() -> gs_start_link_reply().
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec encode(binary()) -> binary().
encode(Data) ->
    Key = extend_key(byte_size(Data), get_key()),
    DataBytes = binary_to_list(Data),
    KeyBytes = binary_to_list(Key),
    encode(DataBytes, KeyBytes, []).


-spec get_key() -> binary().
get_key() ->
    gen_server:call(?MODULE, get_key).


-spec set_key(binary()) -> ok.
set_key(Key) ->
    gen_server:call(?MODULE,{set_key, Key}).


-spec hash(binary()) -> binary().
hash(Data) ->
    gen_server:call(?MODULE, {hash, Data}).


%%% gen_server API

-spec init(gs_args()) -> gs_init_reply().
init([]) ->
    {ok, HashSeed} = application:get_env(my_crypt, hash_seed),
    <<A:32, B:32, C:32>> = HashSeed,
    rand:seed(exsp, {A,B,C}), %% seed it with constant to always get the same hash table
    HashTable = generate_hash_table(),
    {ok, Key} = application:get_env(my_crypt, crypt_key),
    {ok, #state{encrypt_key = Key, hash_table = HashTable}}.


-spec handle_call(gs_request(), gs_from(), gs_reply()) -> gs_call_reply().
handle_call(get_key, _From, #state{encrypt_key = Key} = State) ->
    {reply, Key, State};

handle_call({set_key, NewKey}, _From, State) ->
    {reply, ok, State#state{encrypt_key = NewKey}};

handle_call({hash, Data}, _From, #state{hash_table = HashTable} = State) ->
    {reply, hash(Data, HashTable), State};

handle_call(Any, _From, State) ->
    error_logger:error_msg("unknown call ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


-spec handle_cast(gs_request(), gs_state()) -> gs_cast_reply().
handle_cast(Any, State) ->
    error_logger:error_msg("unknown cast ~p in ~p ~n", [Any, ?MODULE]),
    {noreply, State}.


-spec handle_info(gs_request(), gs_state()) -> gs_info_reply().
handle_info(Request, State) ->
    error_logger:error_msg("unknown info ~p in ~p ~n", [Request, ?MODULE]),
    {noreply, State}.


-spec terminate(terminate_reason(), gs_state()) -> ok.
terminate(_Reason, _State) ->
    ok.


-spec code_change(term(), term(), term()) -> gs_code_change_reply().
code_change(_OldVersion, State, _Extra) ->
    {ok, State}.



%%% inner functions

-spec encode([integer()], [integer()], [integer()]) -> [integer()].
encode([], _, Res) -> list_to_binary(lists:reverse(Res));
encode([DByte | RestData], [KByte | RestKey], Res) ->
    encode(RestData, RestKey, [DByte bxor KByte | Res]).


-spec extend_key(integer(), binary()) -> binary().
extend_key(Size, Key) when byte_size(Key) >= Size -> Key;
extend_key(Size, Key) -> extend_key(Size, <<Key/binary, Key/binary>>).


-spec generate_hash_table() -> [byte()].
generate_hash_table() ->
    Range = lists:seq(0, 255),
    shuffle(Range, []).


-spec shuffle([byte()], [byte()]) -> [byte()].
shuffle([], Res) -> Res;
shuffle(List, Res) ->
    Index = rand:uniform(length(List)),
    Byte = lists:nth(Index, List),
    shuffle(lists:delete(Byte, List), [Byte | Res]).


-spec hash(binary(), [byte()]) -> binary().
hash(Data, HashTable) ->
    DataBytes = binary_to_list(Data),
    {ok, HashSize} = application:get_env(my_crypt, hash_size),
    unicode:characters_to_binary(
        lists:map(
            fun(Num) ->
                Hash = pearson_hashing(Num, DataBytes, HashTable),
                int2bin(Hash)
            end,
            lists:seq(1, HashSize div 2))).


-spec pearson_hashing(byte(), [byte()], [byte()]) -> byte().
pearson_hashing(Start, DataBytes, HashTable) ->
    lists:foldl(
        fun(Byte, Hash) ->
            Index = Hash bxor Byte,
            lists:nth(Index + 1, HashTable)
        end,
        Start, DataBytes).


-spec int2bin(integer()) -> binary().
int2bin(Int) when Int < 16 -> <<"0", (integer_to_binary(Int, 16))/binary>>;
int2bin(Int) -> integer_to_binary(Int, 16).