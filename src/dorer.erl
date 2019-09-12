-module(dorer).
-include_lib("eunit/include/eunit.hrl").
-export([check/1, check/2, gen/1, log/1, log/2, gen/2]).

-type strategy() :: small | random.

-type options() :: #{
strategy => strategy(),
max_shrink_time => {pos_integer(), erlang:time_unit()},
n => integer(),
print_generated_values => boolean() | if_log_empty
}.

-type error_data() :: #{
exception := any(),
stacktrace := any(),
log := any(),
gen := any()
}.

% The default options
-spec default_options() -> options().
default_options() -> #{
  strategy => random,
  max_shrink_time => {3, second},
  n => 100,
  print_generated_values => if_log_empty
}.

%% See check/2, uses default options
-spec check(fun(() -> any())) -> ok.
check(Fun) ->
  check(#{}, Fun).

% Runs the given function multiple times to explore different executions.
% Within the function one can use dorer:gen to generate values
-spec check(options(), fun(() -> any())) -> ok.
check(Options1, Fun) ->
  Options = maps:merge(default_options(), Options1),

  case maps:get(strategy, Options, random) of
    random -> check_random(Options, Fun);
    small -> check_small(Options, Fun)
  end.


run_fun(Fun) ->
  try
    Fun(),
    ok
  catch
    T:E:S ->
      Log = dorer_server:call(get_log),
      Gen = dorer_server:call(get_generated_values),
      {error, #{
        exception => {T, E},
        stacktrace => S,
        log => Log,
        gen => Gen
      }}
  end.

check_random(Options, Fun) ->
  Self = self(),
  P = spawn_link(fun() ->
    {ok, _Pid} = dorer_server:start_link(Options),
    N = maps:get(n, Options, 100),
    Result =
      find_error(N, fun(I) ->
        print("."),
        dorer_server:call({init, I}),
        run_fun(Fun)
      end),

    ShrinkResult = case Result of
      ok -> ok;
      {error, Data} ->
        print("~n~nFound a problem: ~n~n", []),
        show_result_data(Options, Data, false),
        print("Trying to shrink counter example ... ~n~n", []),
        {MaxShrinkTime, MaxShrinkTimeUnit} = maps:get(max_shrink_time, Options, {20, second}),
        ShrinkEndTime = erlang:system_time(MaxShrinkTimeUnit) + MaxShrinkTime,
        shrink_random_execution(Options, Data, Fun, ShrinkEndTime, MaxShrinkTimeUnit)
    end,

    dorer_server:stop(),
    Self ! {result, self(), ShrinkResult}
  end),
  receive
    {result, P, Result} ->
      case Result of
        {error, Data} ->
          print("Minimized Counter Example (~p reductions): ~n~n", [maps:get(shrink_count, Data, 0)]),
          show_result_data(Options, Data, true);
        _ ->
          Result
      end
  end.

show_result_data(Options, Data, Rethrow) ->
  Log = maps:get(log, Data),
  case maps:get(print_generated_values, Options) of
    true ->
      print_generated_values(Data);
    if_log_empty when Log == [] ->
      print_generated_values(Data);
    _ -> ok
  end,

  print_log(Data),

  Exception = maps:get(exception, Data),
  Stacktrace = maps:get(stacktrace, Data),
  {K, E} = Exception,
  print("ERROR ~p: ~p~n ~p~n", [K, E, maps:get(stacktrace, Data)]),
  case dorer_list_utils:confuse_dialyzer(Rethrow) of
    true ->
      case Exception of
        {error, E} ->
          erlang:error({E, Stacktrace});
        {K, E} ->
          print("~nException ~p~n ~p", [E, Stacktrace]),
          throw({K, E, Stacktrace})
      end;
    false ->
      ok
  end.

print_generated_values(Data) ->
  print("~nGenerated Values:~n"),
  lists:foreach(fun
    ({N, G, V}) ->
      print(" ~p: ~p => ~p~n", [N, dorer_generators:name(G), dorer_generators:remove_metadata(G, V)])
  end, maps:get(gen, Data)),
  print("~n").

print_log(Data) ->
  print("~nLOG:~n", []),
  lists:foreach(fun
    (L) ->
      case io_lib:char_list(L) of
        true -> print(" ~s~n", [L]);
        false -> print(" ~p~n", [L])
      end
  end, maps:get(log, Data)),
  print("~n").

check_small(_Options, _Fun) ->
  erlang:error(not_implemented).

% logs a message, only shown on test failures
-spec log(string()) -> ok.
log(Message) ->
  dorer_server:call({log, Message}).

% logs a message using a format string and arguments, only shown on test failures
-spec log(string(), list()) -> ok.
log(Message, Args) ->
  case whereis(dorer_server) of
    undefined ->
      io:format(Message, Args);
    _ ->
      IoList = io_lib:format(Message, Args),
      Bin = erlang:iolist_to_binary(IoList),
      String = binary_to_list(Bin),
      dorer_server:call({log, String})
  end.


find_error(N, Fun) ->
  find_error(0, N, Fun).
find_error(I, N, Fun) ->
  case I >= N of
    true -> ok;
    _ ->
      case Fun(I) of
        ok -> find_error(I + 1, N, Fun);
        Err -> Err
      end
  end.

% generates a value with name 'default', see gen/2
-spec gen(dorer_generators:generator_ext(T)) -> T.
gen(Generator) ->
  gen([default], Generator).

% Generates a value using the given generator (see dorer_generators module)
% The name can be used together with dorer:generators:has_more to generate a sequence of values.
-spec gen([atom()] | atom(), dorer_generators:generator_ext(T)) -> T.
gen(Name, Generator) when not is_list(Name) ->
  gen([Name], Generator);
gen(Name, Generator) ->
  dorer_server:call({generate, Name, Generator}).



shrink_random_execution(Options, Data, Fun, ShrinkEndTime, MaxShrinkTimeUnit) ->
  Generated1 = maps:get(gen, Data),
  % remove special 'is_more' generator
  Generated = lists:filter(fun({_Name, Gen, _Value}) -> dorer_generators:name(Gen) /= has_more end, Generated1),
  ByName = dorer_list_utils:group_by(fun({Name, _Gen, _Value}) -> Name end, Generated),
  Names = maps:keys(ByName),
  ShrinksByName = dorer_lazyseq:flatmap(fun(Name) ->
    NGenerated = maps:get(Name, ByName),
    Shrinks = case lists:any(fun({Name2, Gen, _Value}) ->
      dorer_generators:name(Gen) == has_more
        andalso dorer_list_utils:is_prefix(Name2, Name)
    end, Generated1) of
      true ->
        % if there are has_more queries, then try to shrink the list:
        dorer_generators:shrink_list(fun({Name2, Gen, Value}) ->
          dorer_lazyseq:map(fun(V) -> {Name2, Gen, V} end, dorer_generators:shrinks(Gen, Value))
        end, NGenerated);
      false ->
%%        print("NGenerated = ~p~n", [NGenerated]),
        % otherwise, just try to shrink the list items:
        S = dorer_generators:shrink_list_items(fun({Name2, Gen, Value}) ->
          dorer_lazyseq:map(fun(V) -> {Name2, Gen, V} end, dorer_generators:shrinks(Gen, Value))
        end, NGenerated),
%%        print("NS = ~p~n", [dorer_lazyseq:to_list(S)]),
        S
    end,

    dorer_lazyseq:map(fun(S) ->
      M = maps:put(Name, S, ByName),
      lists:flatten(maps:values(M))
    end, Shrinks)
  end, Names),

  Exec = fun(GeneratedS) ->
    dorer_server:call({init_replay, GeneratedS}),
    case run_fun(Fun) of
      ok -> ok;
      {error, Data2} ->
        NewGen = maps:get(gen, Data2),
        NewLen = length(NewGen),
        OldGen = maps:get(gen, Data),
        OldLen = length(OldGen),
%%        print("NewLen = ~p < ~p~n", [NewLen, OldLen]),
%%        case OldLen == NewLen of
%%          false -> ok;
%%          true ->
%%            print("NewGen: ~p~n", [dorer_list_utils:limit(10, [#{index => I, old => O, new => N} || {I, {O,N}} <- dorer_list_utils:with_index(lists:zip(maps:get(log, Data2), maps:get(log, Data))), O /= N])])
%%        end,
        case NewLen =< OldLen of
          true -> {error, Data2};
          false -> ok
        end
    end
  end,

%%  print("ShrinksByName = ~p~n", [ShrinksByName]),
%%  print("ShrinksByNameL = ~p~n", [[[{N, dorer_generators:name(G), V} || {N,G,V} <- List] || List <- dorer_lazyseq:to_list(ShrinksByName)]]),

  case lazyseq_find_error(Exec, ShrinksByName, ShrinkEndTime, MaxShrinkTimeUnit) of
    {error, SmallerData} ->
      print("."),
%%      print("Found smaller example: ~n", []),
%%      print_log(SmallerData),
      ShrinkCount = maps:get(shrink_count, Data, 0),
      SmallerData2 = maps:put(shrink_count, ShrinkCount + 1, SmallerData),
      shrink_random_execution(Options, SmallerData2, Fun, ShrinkEndTime, MaxShrinkTimeUnit);
    ok ->
      % no smaller execution found -> return original error and data
      {error, Data}
  end.


-spec lazyseq_find_error(fun((T) -> ok | {error, error_data()}), dorer_lazyseq:seq(T), integer(), erlang:time_unit()) -> ok | {error, error_data()}.
lazyseq_find_error(Fun, Seq, ShrinkEndTime, TimeUnit) ->
  CurrentTime = erlang:system_time(TimeUnit),
  case CurrentTime > ShrinkEndTime of
    true ->
      print("~nCould not complete shrinking, because timeout was reached.~n"),
      ok;
    false ->
      case dorer_lazyseq:remove_first(Seq) of
        eof -> ok;
        {next, X, Rest} ->
          case Fun(X) of
            ok -> lazyseq_find_error(Fun, Rest, ShrinkEndTime, TimeUnit);
            {error, Data} ->
              Err = maps:get(exception, Data),
              case is_dorer_error(Err) of
                true ->
                  io:format(user, "Could not shrink because of dorer error: ~n ~p~n ~p~n", [Err, maps:get(stacktrace, Data)]),
                  lazyseq_find_error(Fun, Rest, ShrinkEndTime, TimeUnit);
                false ->
                  {error, Data}
              end
          end
      end
  end.

is_dorer_error({throw, Err}) ->
  is_dorer_error(Err);
is_dorer_error({throw, Err, _}) ->
  is_dorer_error(Err);
is_dorer_error({error, Err}) ->
  is_dorer_error(Err);
is_dorer_error({dorer_replay_error, _}) ->
  true;
is_dorer_error(_) ->
  false.

print(Msg) ->
  print(Msg, []).
print(Msg, Args) ->
  UserPid = whereis(user),
  case group_leader() of
    UserPid -> ok;
    _Other -> io:format(user, Msg, Args)
  end,
  io:format(Msg, Args).
