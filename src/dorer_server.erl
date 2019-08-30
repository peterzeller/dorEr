-module(dorer_server).

-behaviour(gen_server).

%% API
-export([start_link/1, stop/0, call/1]).

%% gen_server callbacks
-export([init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
  options :: dorer:options(),
  size :: integer(),
  mode :: random | small,
  generated_values = [] :: [{Name :: any(), Gen :: dorer_generators:generator(), Value :: any()}],
  replay_gen :: undefined | [{Name :: any(), Gen :: dorer_generators:generator(), Value :: any()}],
  log = [] :: [any()]
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(any()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Options) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Options], []).

stop() ->
  gen_server:stop(?MODULE).


call(Request) ->
  case gen_server:call(?MODULE, Request) of
    {ok, Res} -> Res;
    ok -> ok;
    Error -> throw(Error)
  end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
  {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init([Options]) ->
  rand:seed(exsplus, {47, 1, 1}),
  {ok, #state{
    options = Options,
    mode    = maps:get(strategy, Options, random),
    size    = 5
  }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
  State :: #state{}) ->
  {reply, Reply :: term(), NewState :: #state{}} |
  {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_call(Req, _From, State) ->
  try
    handle_call2(Req, State)
  catch
    T:E:S ->
      {reply, {error, {T, E, S}}, State}
  end.

handle_call2({generate, Name, Gen}, State) ->
  RS = case State#state.replay_gen of
    undefined ->
      RM = dorer_generators:random_gen(Gen, State#state.size),
      {ok, RM, State};
    ReplayGen ->
      case dorer_generators:name(Gen) of
        has_more ->
          R = lists:any(fun
            ({Name2, _, _}) -> dorer_list_utils:is_prefix(Name, Name2)
          end, ReplayGen),
          {ok, R, State};
        _ ->
          case lists:search(fun({Name2, Gen2, _Val}) -> Name == Name2 andalso dorer_generators:name(Gen) == dorer_generators:name(Gen2) end, ReplayGen) of
            {value, X = {_Name2, _Gen2, Val}} ->
              RM = dorer_generators:try_adapt_value(Gen, Val),
              S2 = State#state{
                replay_gen = ReplayGen -- [X]
              },
              {ok, RM, S2};
            false ->
              {error, {dorer_replay_error, {'could not find value for', Name}}}
          end
      end
  end,
  case RS of
    {ok, ResM, State2} ->
      Res = dorer_generators:remove_metadata(Gen, ResM),
      {reply, {ok, Res}, State2#state{
        generated_values = [{Name, Gen, ResM} | State#state.generated_values]
      }};
    {error, Reason} ->
      {reply, {error, Reason}, State}
  end;
handle_call2({init, I}, State) ->
  NewState = State#state{
    log              = [],
    generated_values = [],
    size             = 1 + 1 * I,
    replay_gen       = undefined
  },
  {reply, ok, NewState};
handle_call2({init_replay, GeneratedS}, State) when is_list(GeneratedS) ->
  NewState = State#state{
    log              = [],
    generated_values = [],
    replay_gen       = GeneratedS
  },
  {reply, ok, NewState};
handle_call2(get_log, State) ->
  {reply, {ok, lists:reverse(State#state.log)}, State};
handle_call2(get_generated_values, State) ->
  {reply, {ok, lists:reverse(State#state.generated_values)}, State};
handle_call2({log, Message}, State) ->
  {reply, ok, State#state{log = [Message | State#state.log]}};
handle_call2(Request, State) ->
  {reply, {unhandled_request, Request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
  {noreply, NewState :: #state{}} |
  {noreply, NewState :: #state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #state{}}).
handle_info(_Info, State) ->
  {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
  State :: #state{}) -> term()).
terminate(_Reason, _State) ->
  ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
  Extra :: term()) ->
  {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
