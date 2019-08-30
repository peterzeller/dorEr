-module(dorer_examples).
-include_lib("eunit/include/eunit.hrl").

-import(dorer_generators, [integer/0, transform/2, such_that/2, frequency/1, frequency_gen/1, range/2, oneof/1, list/1, with_custom_shrinks/2, map/2]).

transform_test() ->
  dorer:check(fun() ->
    X = dorer:gen(transform(integer(), fun(X) -> 2 * X end)),
    dorer:log("X = ~p", [X]),
    ?assert(is_integer(X)),
    ?assert(X rem 2 == 0)
  end).


such_that_test() ->
  dorer:check(fun() ->
    X = dorer:gen(such_that(integer(), fun(X) -> X rem 2 == 0 end)),
    dorer:log("X = ~p", [X]),
    ?assert(is_integer(X)),
    ?assert(X rem 2 == 0)
  end).



transform_tuple_test() ->
  dorer:check(fun() ->
    T = dorer:gen({transform(integer(), fun(X) -> 2 * X end), transform(integer(), fun(X) -> 3 * X end)}),
    dorer:log("generated ~p", [T]),
    {X, Y} = T,
    ?assert(is_integer(X)),
    ?assert(is_integer(Y)),

    ?assert(X rem 2 == 0),
    ?assert(Y rem 3 == 0)
  end).




frequency_test() ->
  dorer:check(fun() ->
    X = dorer:gen(frequency_gen([
      {1, range(1, 10)},
      {10, range(1000, 2000)}
    ])),
    dorer:log("generated ~p", [X]),
    ?assert(is_integer(X))
  end).


%%map_test() ->
%%  dorer:check(fun() ->
%%    X = dorer:gen(transform(map(range(1, 20), range(100, 200)), fun maps:to_list/1)),
%%    dorer:log("generated ~p", [X]),
%%    ?assert(is_list(X)),
%%    Sum = lists:sum([K + V || {K, V} <- X]),
%%    dorer:log("sum = ~p", [Sum]),
%%    ?assert(Sum < 1000)
%%  end).


%%frequency_tuple_test() ->
%%  dorer:check(#{max_shrink_time => {5, millisecond}}, fun() ->
%%    X = dorer:gen(list(frequency_gen([
%%      {5, {oneof([a, b, c, d, e]), oneof([1, 2, 3])}},
%%      {10, integer()}
%%    ]))),
%%    dorer:log("generated ~p", [X]),
%%    lists:foreach(fun(Y) ->
%%      case Y of
%%        I when is_integer(I) -> ok;
%%        {A, I} when is_atom(A), is_integer(I) ->
%%          ?assert(A < c)
%%      end
%%    end, X)
%%  end).


lazyseq_flatmap_test() ->
  dorer:check(fun() ->
    X = dorer:gen(list(such_that(integer(), fun(X) -> X >= 0 end))),
    dorer:log("generated ~p", [X]),
    T = dorer_lazyseq:flatmap(fun(I) -> [1 || I > 0, _ <- lists:seq(1, I)] end, X),
    L = dorer_lazyseq:to_list(T),
    dorer:log("L = ~p", [L]),
    lists:foreach(fun(Elem) ->
      ?assert(is_integer(Elem))
    end, L),
    ?assertEqual(lists:sum(X), lists:sum(L))
  end).

%%custom_shrink_test() ->
%%  dorer:check(#{n => 10000}, fun() ->
%%    X = dorer:gen(with_custom_shrinks(list(integer()),
%%      fun(Base, L) ->
%%        dorer_lazyseq:append([move_left(L), Base(L), combine2(L)])
%%      end)),
%%    dorer:log("generated ~p", [X]),
%%    dorer:log("sum = ~p", [lists:sum(X)]),
%%    Sum2 = 1000 - 10 * length(X),
%%    dorer:log("Sum2 = ~p", [Sum2]),
%%    ?assert(lists:sum(X) < Sum2)
%%  end).

%%list_shrink_test() ->
%%  dorer:check(#{n => 10000}, fun() ->
%%    X = dorer:gen(list(integer())),
%%    dorer:log("generated ~p", [X]),
%%    dorer:log("sum = ~p", [lists:sum(X)]),
%%    Sum2 = 1000 - 10 * length(X),
%%    dorer:log("Sum2 = ~p", [Sum2]),
%%    ?assert(length(X) < 20 orelse lists:sum(X) < 1000 orelse lists:max(X) < 100 orelse lists:min(X) > -100)
%%  end).

combine2([]) -> [];
combine2([X]) -> [];
combine2([X, Y | Xs]) ->
  [[X + Y | Xs]] ++ [[X] ++ R || R <- combine2([Y | Xs])].


combine2_test() ->
  ?assertEqual([
    [3, 3, 4, 5, 6],
    [1, 5, 4, 5, 6],
    [1, 2, 7, 5, 6],
    [1, 2, 3, 9, 6],
    [1, 2, 3, 4, 11]
  ], combine2([1, 2, 3, 4, 5, 6])).


move_left([]) -> [];
move_left([X]) -> [];
move_left([X, Y | Xs]) ->
  N = Y div 2 + Y rem 2,
  [[X + N, Y - N | Xs] || Y > 0] ++ [[X] ++ R || R <- move_left([Y | Xs])].


move_left_test() ->
  ?assertEqual([
    [2, 1, 3, 4, 5, 6],
    [1, 4, 1, 4, 5, 6],
    [1, 2, 5, 2, 5, 6],
    [1, 2, 3, 7, 2, 6],
    [1, 2, 3, 4, 8, 3]
  ], move_left([1, 2, 3, 4, 5, 6])).


%%big_shrink_test() ->
%%  dorer:check(#{n => 10000}, fun() ->
%%    X = [dorer:gen(integer()) || _ <- lists:seq(1, 50)],
%%    dorer:log("generated ~p", [X]),
%%    dorer:log("sum = ~p", [lists:sum(X)]),
%%    ?assert(lists:sum(X) < 1000)
%%  end).

%%big_shrink_test() ->
%%  dorer:check(#{n => 10000}, fun() ->
%%    X = dorer:gen(list(integer())),
%%    dorer:log("generated ~p", [X]),
%%    dorer:log("sum = ~p", [lists:sum(X)]),
%%    ?assert(lists:sum(X) < 1000)
%%  end).

%%big_test() ->
%%  dorer:check(#{n => 10000000}, fun() ->
%%    X = [dorer:gen(integer()) || _ <- lists:seq(1, 1000)],
%%    dorer:log("generated ~p", [X]),
%%    ?assert(lists:sum(X) < 100000000)
%%  end).

