%% shorter definitions for gen_server and supervisor callback types

-type(gs_args() :: term()).
-type(gs_state() :: term()).
-type(gs_reason() :: term()).

-type(gs_start_link_reply() :: {ok, pid()} | ignore | {error, term()}).

-type(gs_init_reply() ::
    {ok, gs_state()} | {ok, gs_state(), timeout() | hibernate} |
    {stop, gs_reason()} | ignore).

-type(gs_request() :: term()).
-type(gs_from() :: {pid(), term()}).
-type(gs_reply() :: term()).

-type(gs_call_reply() ::
    {reply, gs_reply(), gs_state()} |
    {reply, gs_reply(), gs_state(), timeout() | hibernate} |
    {noreply, gs_state()} |
    {noreply, gs_state(), timeout() | hibernate} |
    {stop, gs_reason(), gs_reply(), gs_state()} |
    {stop, gs_reason(), gs_state()}).

-type(gs_cast_reply() ::
    {noreply, gs_state()} |
    {noreply, gs_state(), timeout() | hibernate} |
    {stop, gs_reason(), gs_state()}).

-type(gs_info_reply() ::
    {noreply, gs_state()} |
    {noreply, gs_state(), timeout() | hibernate} |
    {stop, gs_reason(), gs_state()}).

-type(terminate_reason() :: normal | shutdown | {shutdown, term()} | term()).

-type(gs_code_change_reply() ::
    {ok, gs_state()} | {error, gs_reason()}).


-type(sup_init_reply() ::
        {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()},
              [supervisor:child_spec()]}}
      | ignore).
