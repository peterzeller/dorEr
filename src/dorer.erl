-module(dorer).
-include_lib("eunit/include/eunit.hrl").
-export([check/1, check/2, gen/1]).

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

check_random(Options, Fun) ->
  Self = self(),
  P = spawn_link(fun() ->
    {ok, _Pid} = dorer_server:start_link(Options),
    N = maps:get(n, Options, 100),
    Result = try
      loop(N, fun(I) ->
        dorer_server:call({init, I}),
        Fun()
      end)
    catch
      T:E:S ->
        Log = dorer_server:call(get_log),
        Gen = dorer_server:call(get_generated_values),
        {exception, #{exception => {T, E}, stacktrace => S, log => Log, gen => Gen}}
    end,
    dorer_server:stop(),
    Self ! {result, self(), Result}
  end),
  receive
    {result, P, Result} ->
      case Result of
        {exception, Data} ->
          io:format("~nGenerated Values:~n"),
          lists:foreach(fun(L) ->
            io:format(" ~p~n", [L])
          end, maps:get(gen, Data)),
          io:format("~nLOG:~n"),
          lists:foreach(fun(L) ->
            io:format(" ~p~n", [L])
          end, maps:get(log, Data)),
          case maps:get(exception, Data) of
            {error, E} ->
              erlang:error(E);
            {_, E} ->
              io:format("~nException ~p~n ~p", [E, maps:get(stacktrace, Data)]),
              throw(E)
          end;
        _ ->
          Result
      end
  after maps:get(timeout, Options, 30000) ->
    throw(timeout)
  end.

check_small(_Options, _Fun) ->
  erlang:error(not_implemented).

loop(N, Fun) ->
  loop(0, N, Fun).
loop(I, N, Fun) ->
  case I >= N of
    true -> ok;
    _ ->
      Fun(I),
      loop(I + 1, N, Fun)
  end.

gen(Generator) ->
  dorer_server:call({generate, Generator}).

