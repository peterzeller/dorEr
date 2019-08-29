-module(dorer).
-include_lib("eunit/include/eunit.hrl").
-export([check/1, check/2, gen/1, log/1, log/2, gen/2]).

-type strategy() :: small | random.

-type options() :: #{
strategy => strategy()
}.

check(Fun) ->
  check(#{}, Fun).

-spec check(options(), fun(() -> any())) -> ok.
check(Options, Fun) ->
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
        dorer_server:call({init, I}),
        run_fun(Fun)
      end),

    ShrinkResult = case Result of
      ok -> ok;
      {error, Data} ->
        print("Found a problem: ~n~n", []),
        show_result_data(Data, false),
        print("Trying to shrink counter example ... ~n~n", []),
        shrink_random_execution(Options, Data, Fun)
    end,

    dorer_server:stop(),
    Self ! {result, self(), ShrinkResult}
  end),
  receive
    {result, P, Result} ->
      case Result of
        {error, Data} ->
          print("Minimized Counter Example: ~n~n", []),
          show_result_data(Data, true);
        _ ->
          Result
      end
  after maps:get(timeout, Options, 30000) ->
    throw(timeout)
  end.

show_result_data(Data, Rethrow) ->
  print_generated_values(Data),
  print_log(Data),
  Exception = maps:get(exception, Data),
  case Rethrow of
    true ->
      case Exception of
        {error, E} ->
          erlang:error(E);
        {_, E} ->
          print("~nException ~p~n ~p", [E, maps:get(stacktrace, Data)]),
          throw(E)
      end;
    false ->
      {K, E} = Exception,
      print("ERROR ~p: ~p~n ~p~n", [K, E, maps:get(stacktrace, Data)])
  end.

print_generated_values(Data) ->
  print("~nGenerated Values:~n"),
  lists:foreach(fun
    ({N, G, V}) ->
      print(" ~p: ~p => ~p~n", [N, maps:get(name, G), V])
  end, maps:get(gen, Data)).

print_log(Data) ->
  print("~nLOG:~n", []),
  lists:foreach(fun
    (L) ->
      case io_lib:char_list(L) of
        true -> print(" ~s~n", [L]);
        false -> print(" ~p~n", [L])
      end
  end, maps:get(log, Data)).

check_small(_Options, _Fun) ->
  erlang:error(not_implemented).

log(Message) ->
  dorer_server:call({log, Message}).

log(Message, Args) ->
  IoList = io_lib:format(Message, Args),
  Bin = erlang:iolist_to_binary(IoList),
  String = binary_to_list(Bin),
  dorer_server:call({log, String}).


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

gen(Generator) ->
  gen([default], Generator).
gen(Name, Generator) when not is_list(Name) ->
  gen([Name], Generator);
gen(Name, Generator) ->
  dorer_server:call({generate, Name, Generator}).



shrink_random_execution(Options, Data, Fun) ->
  Generated1 = maps:get(gen, Data),
  % remove special 'is_more' generator
  Generated = lists:filter(fun({_Name, Gen, _Value}) -> maps:get(name, Gen) /= has_more end, Generated1),
  ByName = dorer_list_utils:group_by(fun({Name, _Gen, _Value}) -> Name end, Generated),
  Names = maps:keys(ByName),
  ShrinksByName = dorer_lazyseq:flatmap(fun(Name) ->
    NGenerated = maps:get(Name, ByName),
    Shrinks = dorer_generators:shrink_list(fun({Name2, Gen, Value}) ->
      dorer_lazyseq:map(fun(V) -> {Name2, Gen, V} end, dorer_generators:shrinks(Gen, Value))
    end, NGenerated),
    dorer_lazyseq:map(fun(S) ->
      M = maps:put(Name, S, ByName),
      lists:flatten(maps:values(M))
    end, Shrinks)
  end, Names),

  Exec = fun(GeneratedS) ->
    dorer_server:call({init_replay, GeneratedS}),
    Res = run_fun(Fun),
    Res
  end,

  case lazyseq_find_error(Exec, ShrinksByName) of
    {error, SmallerData} ->
%%      print("Found smaller example: ~n", []),
%%      print_log(SmallerData),
      shrink_random_execution(Options, SmallerData, Fun);
    ok ->
      % no smaller execution found -> return original error and data
      {error, Data}
  end.


-spec lazyseq_find_error(fun((T) -> ok | X), dorer_lazyseq:seq(T)) -> ok | X.
lazyseq_find_error(Fun, Seq) ->
  case dorer_lazyseq:remove_first(Seq) of
    eof -> ok;
    {next, X, Rest} ->
      case Fun(X) of
        ok -> lazyseq_find_error(Fun, Rest);
        Err -> Err
      end
  end.

print(Msg) ->
  print(Msg, []).
print(Msg, Args) ->
  case group_leader() of
    user -> ok;
    _Other -> io:format(user, Msg, Args)
  end,
  io:format(Msg, Args).
