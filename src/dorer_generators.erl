-module(dorer_generators).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([integer/0, list/1, random_gen/2, shrinks/2, has_more/0, shrink_list/2, try_adapt_value/2, oneof/1, range/2, frequency/1, frequency_gen/1]).

-type random_gen(T) :: #{
produce := fun((Size :: integer()) -> T),
shrink := shrinker(T),
try_adapt := fun((T) -> T)
}.

-type generator_name() :: atom() | {atom(), [generator_name()]}.

-type shrinker(T) :: fun((T) -> dorer_lazyseq:seq(T)).

-type generator(T) :: #{
name := generator_name(),
random_gen => random_gen(T),
small_gen => fun((Bound :: integer()) -> dorer_lazyseq:seq(T))
}.

-type generator_ext(T) :: generator(T) | any().


-spec integer() -> generator(integer()).
integer() ->
  #{
    name => integer,
    random_gen => #{
      produce => fun(Size) ->
        rand:uniform(2 * Size) - Size
      end,
      shrink => fun shrink_int/1,
      try_adapt => fun(T) when is_integer(T) -> T end
    },
    small_gen => fun(Bound) ->
      dorer_lazyseq:iterate(0, fun
        (0) -> {next, 0, 1};
        (S) when S < Bound -> {nexts, [S, -S], S + 1};
        (_) -> eof
      end)
    end
  }.

-spec range(integer(), integer()) -> generator(integer()).
range(Min, Max) when Max >= Min ->
  #{
    name => range,
    random_gen => #{
      produce => fun(Size) ->
        rand:uniform(min(1 + Max - Min, Size)) + Min - 1
      end,
      shrink => fun(X) ->
        dorer_lazyseq:map(fun(A) -> A + Min end, shrink_int(X - Min))
      end,
      try_adapt => fun(T) when T >= Min andalso T =< Max -> T end
    },
    small_gen => fun(Bound) ->
      dorer_lazyseq:iterate(Min, fun
        (S) when S =< Max andalso S - Min =< Bound -> {next, S, S + 1};
        (_) -> eof
      end)
    end
  }.


shrink_int(0) -> [];
shrink_int(X) ->
  [-X || X < 0] ++ [0] ++ [X - Y || Y <- dorer_list_utils:iterate(X, fun(A) when abs(A) < 2 -> break; (A) ->
    {continue, A div 2} end)].


-spec list(generator(T)) -> generator(list(T)).
list(ItemGen) ->
  #{
    name => {list, [maps:get(name, ItemGen)]},
    random_gen => #{
      produce => fun(Size) -> produce_list(ItemGen, Size) end,
      shrink => fun(List) -> shrink_list(shrinker(ItemGen), List) end,
      try_adapt => fun(List) when is_list(List) ->
        [try_adapt_value(ItemGen, V) || V <- List]
      end
    }
  }.

-spec map(generator(K), generator(V)) -> generator(#{K => V}).
map(KeyGen, ValueGen) ->
  #{
    name => {list, [maps:get(name, KeyGen), maps:get(name, ValueGen)]},
    random_gen => #{
      produce => fun(Size) ->
        Keys = remove_duplicates_twice(lists:sort(produce_list(KeyGen, Size))),
        maps:from_list([{K, random_gen(ValueGen, Size - 1)} || K <- Keys])
      end,
      shrink => fun(Elem) ->
        maps:from_list(shrink_list(tuple_shrinker({KeyGen, ValueGen}), maps:to_list(Elem)))
      end,
      try_adapt => fun(Map) when is_map(Map) ->
        maps:from_list([{try_adapt_value(KeyGen, K), try_adapt_value(ValueGen, V)} || {K, V} <- maps:to_list(Map)])
      end
    }
  }.

remove_duplicates_twice([]) -> [];
remove_duplicates_twice([X, X | Xs]) ->
  remove_duplicates_twice(Xs);
remove_duplicates_twice([X | Xs]) ->
  [X | remove_duplicates_twice(Xs)].

-spec oneof([T]) -> generator(T).
oneof(Choices) ->
  #{
    name => oneof,
    random_gen => #{
      prodce => fun(Size) ->
        case length(Choices) of
          0 ->
            throw('Could not generate a choice, because the list is empty');
          N ->
            I = min(max(Size, 1), rand:uniform(N)),
            lists:nth(I, Choices)
        end
      end,
      shrink => fun(Elem) ->
        shrink_oneof(Elem, Choices)
      end,
      try_adapt => fun(Elem) ->
        dorer_list_utils:pick_most_similar(Elem, Choices)
      end
    }
  }.

-spec frequency([{integer(), T}]) -> generator(T).
frequency(Choices) ->
  Sum = lists:sum([F || {F, _} <- Choices]),
  #{
    name => frequency,
    random_gen => #{
      prodce => fun(_Size) ->
        I = rand:uniform(Sum),
        pick_from_frequencies(I, Choices)
      end,
      shrink => fun(Elem) ->
        shrink_oneof(Elem, [C || {_, C} <- Choices])
      end,
      try_adapt => fun(Elem) ->
        dorer_list_utils:pick_most_similar(Elem, [C || {_, C} <- Choices])
      end
    }
  }.

-spec frequency_gen([generator_ext(T)]) -> generator(T).
frequency_gen(Choices) ->
  Sum = lists:sum([F || {F, _} <- Choices]),
  #{
    name => frequency,
    random_gen => #{
      prodce => fun(Size) ->
        I = rand:uniform(Sum),
        CGen = pick_from_frequencies(I, Choices),
        random_gen(CGen, Size)
      end,
      shrink => fun(Elem) ->
        dorer_lazyseq:flatmap(fun(CGen) ->
          try
            A = try_adapt_value(CGen, Elem),
            shrinks(CGen, A)
          catch
            _:_:_ -> []
          end
        end, [C || {_, C} <- Choices])
      end,
      try_adapt => fun(Elem) ->
        pick_first_without_exception(fun(CGen) -> try_adapt_value(CGen, Elem) end, [C || {_, C} <- Choices])
      end
    }
  }.


pick_first_without_exception(_, []) -> throw('no element could be adapted');
pick_first_without_exception(F, [X]) -> F(X);
pick_first_without_exception(F, [X | Xs]) ->
  try
    F(X)
  catch
    _:_:_ ->
      pick_first_without_exception(F, Xs)
  end.

pick_from_frequencies(_, [{_, X}]) -> X;
pick_from_frequencies(I, [{F, X} | Rest]) ->
  if
    F > 0 andalso I =< F -> X;
    true -> pick_from_frequencies(I - F, Rest)
  end.


shrink_oneof(_Elem, []) -> [];
shrink_oneof(Elem, [Elem | _]) -> [];
shrink_oneof(Elem, [C | Cs]) ->
  [C | shrink_oneof(Elem, Cs)].


-spec shrink_list(shrinker(T), [T]) -> dorer_lazyseq:seq([T]).
shrink_list(_, []) -> [];
shrink_list(ItemShrinker, List) ->
  N = length(List),
  HalfLists = if
    N > 3 -> {First, Second} = lists:split(N div 2, List),
      [First, Second];
    true -> []
  end,

  OneRemoved = dorer_lazyseq:iterate(0, fun
    (I) when I >= N -> eof;
    (I) ->
      {A, [_ | B]} = lists:split(I, List),
      {next, A ++ B, I + 1}
  end),
  ItemShrinkers = lists:map(fun({I, X}) ->
    {A, [_ | B]} = lists:split(I, List),
    {I, ItemShrinker(X), A, B}
  end, dorer_list_utils:with_index(List)),

  OneShrunk = dorer_lazyseq:iterate(queue:from_list(ItemShrinkers), fun(Q) ->
    case queue:out(Q) of
      {empty, _} -> eof;
      {{value, {I, Shrinks, A, B}}, Q2} ->
        case dorer_lazyseq:remove_first(Shrinks) of
          eof ->
            {nexts, [], Q2};
          {next, X2, RestShrinks} ->
            {next, A ++ [X2 | B], queue:in({I, RestShrinks, A, B}, Q2)}
        end
    end
  end),
  dorer_lazyseq:append([HalfLists, OneRemoved, OneShrunk]).

% Generates a boolean, false with probability 1/size
-spec has_more() -> generator(boolean()).
has_more() ->
  #{
    name => has_more,
    random_gen => #{
      produce => fun(Size) ->
        rand:uniform(Size) > 1
      end,
      shrink => fun
        (true) -> [false];
        (false) -> []
      end,
      try_adapt => fun(X) when is_boolean(X) -> X end
    },
    small_gen => fun(Bound) ->
      case Bound =< 0 of
        true -> [false];
        false -> [false, true]
      end
    end
  }.


produce_list(_ItemGen, 0) -> [];
produce_list(ItemGen, N) ->
  case rand:uniform(N) of
    1 -> [];
    _ ->
      [random_gen(ItemGen, N) | produce_list(ItemGen, N - 1)]
  end.


-spec random_gen(generator_ext(T), non_neg_integer()) -> T.
random_gen(Generator = #{name := _}, Size) ->
  #{random_gen := #{produce := Produce}} = Generator,
  Produce(Size);
random_gen(Tuple, Size) when is_tuple(Tuple) ->
  list_to_tuple([random_gen(E, Size) || E <- tuple_to_list(Tuple)]);
random_gen(Elem, _Size) ->
  Elem.


-spec shrinks(generator(T), T) -> dorer_lazyseq:seq(T).
shrinks(Generator, X) ->
  (shrinker(Generator))(X).

-spec shrinker(generator(T)) -> shrinker(T).
shrinker(Generator = #{name := _}) ->
  #{random_gen := #{shrink := Shrink}} = Generator,
  Shrink;
shrinker(TupleGen) when is_tuple(TupleGen) ->
  tuple_shrinker(TupleGen);
shrinker(_) ->
  % default: do not shrink
  fun(_Elem) -> [] end.

tuple_shrinker(TupleGen) ->
  fun(Tuple) ->
    TupleL = tuple_to_list(Tuple),
    TupleGenL = tuple_to_list(TupleGen),
    ItemShrinkers = lists:map(fun({I, {Gen, Elem}}) ->
      {A, [_ | B]} = lists:split(I, TupleL),
      {I, (shrinker(Gen))(Elem), A, B}
    end, dorer_list_utils:with_index(lists:zip(TupleGenL, TupleL))),


    dorer_lazyseq:iterate(queue:from_list(ItemShrinkers), fun(Q) ->
      case queue:out(Q) of
        {empty, _} -> eof;
        {{value, {I, Shrinks, A, B}}, Q2} ->
          case dorer_lazyseq:remove_first(Shrinks) of
            eof ->
              {nexts, [], Q2};
            {next, X2, RestShrinks} ->
              {next, list_to_tuple(A ++ [X2 | B]), queue:in({I, RestShrinks, A, B}, Q2)}
          end
      end
    end)
  end.

% checks if the value could be generated by this generator
% adapts the value or throws {dorer_replay_error, _} if not
-spec try_adapt_value(generator(T), T) -> T.
try_adapt_value(Generator, Value) ->
  #{random_gen := #{try_adapt := Adapt}} = Generator,
  try Adapt(Value)
  catch
    T:E:S -> {dorer_replay_error, {'could not adapt value', Value, {T, E, S}}}
  end.




-ifdef(TEST).

shrink_list_test() ->
  Gen = list(integer()),
  ?assertEqual([
    % halfs
    [1, 2],
    [3, 4, 5],
    % one removed
    [2, 3, 4, 5],
    [1, 3, 4, 5],
    [1, 2, 4, 5],
    [1, 2, 3, 5],
    [1, 2, 3, 4],
    % one shrunk
    [0, 2, 3, 4, 5],
    [1, 0, 3, 4, 5],
    [1, 2, 0, 4, 5],
    [1, 2, 3, 0, 5],
    [1, 2, 3, 4, 0],
    [1, 1, 3, 4, 5],
    [1, 2, 2, 4, 5],
    [1, 2, 3, 2, 5],
    [1, 2, 3, 4, 3],
    [1, 2, 3, 3, 5],
    [1, 2, 3, 4, 4]],
    dorer_lazyseq:to_list(shrinks(Gen, [1, 2, 3, 4, 5]))).

shrink_list2_test() ->
  Gen = list(integer()),
  ?assertEqual([
    % one removed
    [2000, 3000],
    [1000, 3000],
    [1000, 2000],
    % smaller elements:
    [0, 2000, 3000],
    [1000, 0, 3000],
    [1000, 2000, 0],
    [500, 2000, 3000],
    [1000, 1000, 3000],
    [1000, 2000, 1500],
    [750, 2000, 3000],
    [1000, 1500, 3000],
    [1000, 2000, 2250],
    [875, 2000, 3000],
    [1000, 1750, 3000],
    [1000, 2000, 2625],
    [938, 2000, 3000],
    [1000, 1875, 3000],
    [1000, 2000, 2813],
    [969, 2000, 3000],
    [1000, 1938, 3000],
    [1000, 2000, 2907],
    [985, 2000, 3000],
    [1000, 1969, 3000],
    [1000, 2000, 2954],
    [993, 2000, 3000],
    [1000, 1985, 3000],
    [1000, 2000, 2977],
    [997, 2000, 3000],
    [1000, 1993, 3000],
    [1000, 2000, 2989],
    [999, 2000, 3000],
    [1000, 1997, 3000],
    [1000, 2000, 2995],
    [1000, 1999, 3000],
    [1000, 2000, 2998],
    [1000, 2000, 2999]],
    dorer_lazyseq:to_list(shrinks(Gen, [1000, 2000, 3000]))).


shrink_integer_test() ->
  ?assertEqual([0, 500, 750, 875, 938, 969, 985, 993, 997, 999],
    dorer_lazyseq:to_list(shrinks(integer(), 1000))),
  ?assertEqual([1000, 0, -500, -750, -875, -938, -969, -985, -993, -997, -999],
    dorer_lazyseq:to_list(shrinks(integer(), -1000))).

shrink_tuple_test() ->
  ?assertEqual([
    {ok, 10, 33, 60},
    {ok, 17, 30, 60},
    {ok, 17, 33, 50},
    {ok, 14, 33, 60},
    {ok, 17, 32, 60},
    {ok, 17, 33, 55},
    {ok, 16, 33, 60},
    {ok, 17, 33, 58},
    {ok, 17, 33, 59}],
    dorer_lazyseq:to_list(shrinks({ok, range(10, 20), range(30, 40), range(50, 60)}, {ok, 17, 33, 60}))).


remove_duplicates_twice_test() ->
  ?assertEqual([a, c], remove_duplicates_twice(lists:sort([a, b, c, a, b, b, a, b]))).

-endif.


