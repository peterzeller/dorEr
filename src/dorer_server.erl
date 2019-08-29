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
  gen_server:call(?MODULE, Request).

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
  io:format(user, "Initializing Dorer Server~n", []),
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
handle_call({generate, Name, Gen}, _From, State) ->
  {Res, State2} = case State#state.replay_gen of
    undefined ->
      R = dorer_generators:random_gen(Gen, State#state.size),
      {R, State};
    ReplayGen ->
      case maps:get(name, Gen) of
        has_more ->
          R = lists:any(fun
            ({Name2, _, _}) -> dorer_list_utils:is_prefix(Name, Name2)
          end, ReplayGen),
          {R, State};
        _ ->
          case lists:search(fun({Name2, Gen2, _Val}) -> Name == Name2 andalso maps:get(name, Gen) == maps:get(name, Gen2) end, ReplayGen) of
            {value, X = {_Name2, _Gen2, Val}} ->
              R = dorer_generators:try_adapt_value(Gen, Val),
              S2 = State#state{
                replay_gen = ReplayGen -- [X]
              },
              {R, S2};
            false ->
              throw({dorer_replay_error, {'could not find value for', Name}})
          end
      end
  end,
  {reply, Res, State2#state{
    generated_values = [{Name, Gen, Res} | State#state.generated_values]
  }};
handle_call({init, I}, _From, State) ->
  NewState = State#state{
    log              = [],
    generated_values = [],
    size             = 1 + 1 * I,
    replay_gen       = undefined
  },
  {reply, ok, NewState};
handle_call({init_replay, GeneratedS}, _From, State) when is_list(GeneratedS) ->
  NewState = State#state{
    log              = [],
    generated_values = [],
    replay_gen       = GeneratedS
  },
  {reply, ok, NewState};
handle_call(get_log, _From, State) ->
  {reply, lists:reverse(State#state.log), State};
handle_call(get_generated_values, _From, State) ->
  {reply, lists:reverse(State#state.generated_values), State};
handle_call({log, Message}, _From, State) ->
  {reply, ok, State#state{log = [Message | State#state.log]}};
handle_call(Request, _From, _State) ->
  throw({unhandled_request, Request}).

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
