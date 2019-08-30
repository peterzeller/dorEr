-module(dorer_generators).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API
-export([integer/0, list/1, random_gen/2, shrinks/2, has_more/0, shrink_list/2, try_adapt_value/2, oneof/1, range/2, frequency/1, frequency_gen/1, map/2, set/1, name/1, transform/2, such_that/2, remove_metadata/2, shrink_list_items/2, with_custom_shrinks/2, metadata/1, no_shrink/1]).

-export_type([generator/1, random_gen/1]).

-record(random_gen, {
  produce :: fun((Size :: integer()) -> any()),
  remove_metadata :: fun((any()) -> any()),
  shrink :: shrinker(any()),
  try_adapt :: fun((any()) -> any())
}).

-opaque random_gen(T) :: #random_gen{
produce :: fun((Size :: integer()) -> T2),
remove_metadata :: fun((T2) -> T),
shrink :: shrinker(T2),
try_adapt :: fun((T2) -> T2)
}.


-type generator_name() :: atom() | {atom(), [generator_name()]}.

-type shrinker(T) :: fun((T) -> dorer_lazyseq:seq(T)).

-record(generator, {
  name :: generator_name(),
  metadata :: any(),
  random_gen :: undefined | random_gen(any()),
  small_gen :: undefined | small_gen(any())
}).

-type small_gen(T) :: fun((Bound :: integer()) -> dorer_lazyseq:seq(T)).

-opaque generator(T) :: #generator{
random_gen :: undefined | random_gen(T),
small_gen :: undefined | small_gen(T)
}.



-type generator_ext(T) :: generator(T) | any().


-spec integer() -> generator(integer()).
integer() ->
  #generator{
    name = integer,
    random_gen = #random_gen{
      produce = fun(Size) ->
        rand:uniform(2 * Size) - Size
      end,
      remove_metadata = fun identity/1,
      shrink = fun shrink_int/1,
      try_adapt = fun(T) when is_integer(T) -> T end
    },
    small_gen = fun(Bound) ->
      dorer_lazyseq:iterate(0, fun
        (0) -> {next, 0, 1};
        (S) when S < Bound -> {nexts, [S, -S], S + 1};
        (_) -> eof
      end)
    end
  }.

-spec range(integer(), integer()) -> generator(integer()).
range(Min, Max) when Max >= Min ->
  #generator{
    name = range,
    random_gen = #random_gen{
      produce = fun(Size) ->
        rand:uniform(min(1 + Max - Min, Size)) + Min - 1
      end,
      remove_metadata = fun identity/1,
      shrink = fun(X) ->
        dorer_lazyseq:map(fun(A) -> A + Min end, shrink_int(X - Min))
      end,
      try_adapt = fun(T) when T >= Min andalso T =< Max -> T end
    },
    small_gen = fun(Bound) ->
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
  #generator{
    name = {list, [name(ItemGen)]},
    metadata = {list, [metadata(ItemGen)]},
    random_gen = #random_gen{
      produce = fun(Size) -> produce_list(ItemGen, Size) end,
      remove_metadata = fun(List) -> lists:map(fun(X) -> remove_metadata(ItemGen, X) end, List) end,
      shrink = fun(List) -> shrink_list(shrinker(ItemGen), List) end,
      try_adapt = fun(List) when is_list(List) ->
        [try_adapt_value(ItemGen, V) || V <- List]
      end
    }
  }.

-spec set(generator(T)) -> generator(ordsets:ordset(T)).
set(ItemGen) ->
  #generator{
    name = {set, [name(ItemGen)]},
    random_gen = #random_gen{
      produce = fun(Size) ->
        Elems = remove_duplicates_twice(lists:sort(produce_list(ItemGen, Size))),
        ordsets:from_list(Elems)
      end,
      remove_metadata = fun(List) -> lists:map(fun(X) -> remove_metadata(ItemGen, X) end, List) end,
      shrink = fun(Elem) ->
        dorer_lazyseq:map(fun ordsets:from_list/1,
          shrink_list(shrinker(ItemGen), Elem))
      end,
      try_adapt = fun(Set) when is_list(Set) ->
        ordsets:from_list(lists:map(fun(E) -> try_adapt_value(ItemGen, E) end, Set))
      end
    }
  }.

-spec map(generator(K), generator(V)) -> generator(#{K => V}).
map(KeyGen, ValueGen) ->
  #generator{
    name = {map, [name(KeyGen), name(ValueGen)]},
    random_gen = #random_gen{
      produce = fun(Size) ->
        Keys = remove_duplicates_twice(lists:sort(produce_list(KeyGen, Size))),
        maps:from_list([{K, random_gen(ValueGen, Size - 1)} || K <- Keys])
      end,
      remove_metadata = fun(Map) ->
        maps:from_list(
          lists:map(fun({K, V}) -> {remove_metadata(KeyGen, K), remove_metadata(ValueGen, V)} end, maps:to_list(Map)))
      end,
      shrink = fun(Elem) ->
        dorer_lazyseq:map(fun maps:from_list/1, shrink_list(tuple_shrinker({KeyGen, ValueGen}), maps:to_list(Elem)))
      end,
      try_adapt = fun(Map) when is_map(Map) ->
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
  #generator{
    name = oneof,
    random_gen = #random_gen{
      produce = fun(Size) ->
        case length(Choices) of
          0 ->
            throw('Could not generate a choice, because the list is empty');
          N ->
            I = min(max(Size, 1), rand:uniform(N)),
            lists:nth(I, Choices)
        end
      end,
      remove_metadata = fun identity/1,
      shrink = fun(Elem) ->
        shrink_oneof(Elem, Choices)
      end,
      try_adapt = fun(Elem) ->
        dorer_list_utils:pick_most_similar(Elem, Choices)
      end
    }
  }.

-spec frequency([{integer(), T}]) -> generator(T).
frequency(Choices) ->
  Sum = lists:sum([F || {F, _} <- Choices]),
  #generator{
    name = frequency,
    random_gen = #random_gen{
      produce = fun(_Size) ->
        I = rand:uniform(Sum),
        {_Pos, Elem} = pick_from_frequencies(I, Choices),
        Elem
      end,
      remove_metadata = fun identity/1,
      shrink = fun(Elem) ->
        shrink_oneof(Elem, [C || {_, C} <- Choices])
      end,
      try_adapt = fun(Elem) ->
        dorer_list_utils:pick_most_similar(Elem, [C || {_, C} <- Choices])
      end
    }
  }.

-spec frequency_gen([generator_ext(T)]) -> generator(T).
frequency_gen(Choices) ->
  Sum = lists:sum([F || {F, _} <- Choices]),
  #generator{
    name = frequency_gen,
    metadata = Choices,
    random_gen = #random_gen{
      produce = fun(Size) ->
        I = rand:uniform(Sum),
        {Pos, CGen} = pick_from_frequencies(I, Choices),
        true = is_integer(Pos),
        {Pos, random_gen(CGen, Size)}
      end,
      remove_metadata = fun({Pos, X}) ->
        {_F, CGen} = lists:nth(Pos, Choices),
        remove_metadata(CGen, X)
      end,
      shrink = fun({Pos, X}) ->
        {_F, CGen} = lists:nth(Pos, Choices),
        dorer_lazyseq:map(fun(A) -> {Pos, A} end, shrinks(CGen, X))
      end,
      try_adapt = fun({Pos, X}) ->
        {_F, CGen} = lists:nth(Pos, Choices),
        {Pos, try_adapt_value(CGen, X)}
      end
    }
  }.


-spec transform(generator(T), fun((T) -> S)) -> generator(S).
transform(Gen, F) ->
  #generator{
    name = {transform, [name(Gen)]},
    random_gen = #random_gen{
      produce = fun(Size) ->
        X = random_gen(Gen, Size),
        {X, F(X)}
      end,
      remove_metadata = fun({_O, T}) -> T end,
      shrink = fun({O, _T}) ->
        dorer_lazyseq:map(fun(OS) -> {OS, F(OS)} end, shrinks(Gen, O))
      end,
      try_adapt = fun({O, _T}) ->
        OAdapted = try_adapt_value(Gen, O),
        {OAdapted, F(OAdapted)}
      end
    }
  }.

-spec such_that(generator(T), fun((T) -> boolean())) -> generator(T).
such_that(Gen, Pred) ->
  #generator{
    name = {such_that, [name(Gen)]},
    random_gen = #random_gen{
      produce = fun(Size) ->
        dorer_list_utils:loop(0, fun(I) ->
          if
            I > 1000 ->
              throw({'could not generate value'});
            true ->
              X = random_gen(Gen, Size + I),
              case Pred(X) of
                true -> {return, X};
                false -> {continue, I + 1}
              end
          end
        end)
      end,
      remove_metadata = fun identity/1,
      shrink = fun(X) ->
        dorer_lazyseq:filter(Pred, shrinks(Gen, X))
      end,
      try_adapt = fun(O) ->
        X = try_adapt_value(Gen, O),
        true = Pred(X),
        X
      end
    }
  }.

-spec with_custom_shrinks(generator(T), fun((Base :: shrinker(T2), Elem :: T2) -> dorer_lazyseq:seq(T2))) -> generator(T).
with_custom_shrinks(Gen = #generator{}, CustomShrinks) ->
  RandomGen = Gen#generator.random_gen,
  #generator{
    name = {with_custom_shrinks, [name(Gen)]},
    random_gen = RandomGen#random_gen{
      shrink = fun(Elem) ->
        CustomShrinks(RandomGen#random_gen.shrink, Elem)
      end
    },
    small_gen = Gen#generator.small_gen
  }.


-spec no_shrink(generator(T)) -> generator(T).
no_shrink(Gen = #generator{}) ->
  with_custom_shrinks(Gen, fun(_Base, _Elem) -> [] end).

-spec pick_from_frequencies(non_neg_integer(), list({_, X})) -> {non_neg_integer(), X}.
pick_from_frequencies(_, [{_, X}]) -> {1, X};
pick_from_frequencies(I, [{F, X} | Rest]) ->
  if
    F > 0 andalso I =< F -> {1, X};
    true ->
      {Pos, Res} = pick_from_frequencies(I - F, Rest),
      {Pos + 1, Res}
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
  OneShrunk = shrink_list_items(ItemShrinker, List),
  dorer_lazyseq:append([HalfLists, OneRemoved, OneShrunk]).

-spec shrink_list_items(shrinker(T), [T]) -> dorer_lazyseq:seq([T]).
shrink_list_items(ItemShrinker, List) ->
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
  OneShrunk.

% Generates a boolean, false with probability 1/size
-spec has_more() -> generator(boolean()).
has_more() ->
  #generator{
    name = has_more,
    random_gen = #random_gen{
      produce = fun(Size) ->
        rand:uniform(Size) > 1
      end,
      remove_metadata = fun identity/1,
      shrink = fun
        (true) -> [false];
        (false) -> []
      end,
      try_adapt = fun(X) when is_boolean(X) -> X end
    },
    small_gen = fun(Bound) ->
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
random_gen(Generator = #generator{}, Size) ->
  Produce = Generator#generator.random_gen#random_gen.produce,
  Produce(Size);
random_gen(Tuple, Size) when is_tuple(Tuple) ->
  list_to_tuple([random_gen(E, Size) || E <- tuple_to_list(Tuple)]);
random_gen(Elem, _Size) ->
  Elem.


-spec shrinks(generator(T), T) -> dorer_lazyseq:seq(T).
shrinks(Generator, X) ->
  try
    (shrinker(Generator))(X)
  catch
    T:E:S ->
      throw({'error trying to shrink', name(Generator), X, {T, E, S}})
  end.

-spec shrinker(generator(T)) -> shrinker(T).
shrinker(Generator = #generator{}) ->
  #generator{random_gen = #random_gen{shrink = Shrink}} = Generator,
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
-spec try_adapt_value(generator_ext(T), T) -> T.
try_adapt_value(Generator = #generator{}, Value) ->
  #generator{random_gen = #random_gen{try_adapt = Adapt}} = Generator,
  try Adapt(Value)
  catch
    T:E:S -> throw({dorer_replay_error, {'could not adapt value', #{
      generator_name => name(Generator),
      metadata => metadata(Generator),
      value => Value, error => {T, E, S}}}})
  end;
try_adapt_value(TupleGen, Tuple) when is_tuple(TupleGen) ->
  TupleGenL = tuple_to_list(TupleGen),
  TupleL = tuple_to_list(Tuple),
  case length(TupleGenL) == length(TupleL) of
    true ->
      Adapted = [try_adapt_value(Gen, V) || {Gen, V} <- lists:zip(TupleGenL, TupleL)],
      list_to_tuple(Adapted);
    false ->
      throw({dorer_replay_error, {'could not adapt tuple value', #{
        generators => [name(G) || G <- TupleGenL],
        values => TupleL}}})
  end;
try_adapt_value(_, X) -> X.

-spec name(generator(any())) -> generator_name().
name(Gen = #generator{}) ->
  Gen#generator.name;
name(Tuple) when is_tuple(Tuple) ->
  {tuple, [name(G) || G <- tuple_to_list(Tuple)]};
name(X) -> X.

-spec metadata(generator(any())) -> any().
metadata(Gen = #generator{}) ->
  Gen#generator.metadata;
metadata(_) -> undefined.

identity(X) -> X.


-spec remove_metadata(generator_ext(T), any()) -> T.
remove_metadata(Gen = #generator{}, X) ->
  RemoveMetadata = Gen#generator.random_gen#random_gen.remove_metadata,
  try
    RemoveMetadata(X)
  catch
    T:E:S ->
      throw({dorer_replay_error, {'could not remove metadata', name(Gen), X, {T, E, S}}})
  end;
remove_metadata(TupleGen, Tuple) when is_tuple(TupleGen) ->
  list_to_tuple(
    [remove_metadata(Gen, Elem) || {Gen, Elem} <- lists:zip(tuple_to_list(TupleGen), tuple_to_list(Tuple))]
  );
remove_metadata(_, X) -> X.


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

shrink_list_items_test() ->
  ?assertEqual([
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
    dorer_lazyseq:to_list(shrink_list_items(shrinker(integer()), [1, 2, 3, 4, 5]))).


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

adapt_tuple_test() ->
  Gen = list(frequency_gen([
    {5, {oneof([a, b, c, d, e]), oneof([1, 2, 3])}},
    {10, integer()}
  ])),
  ?assertEqual([{1, {c, 3}}, {2, 0}], try_adapt_value(Gen, [{1, {c, 3}}, {2, 0}])).

frequency_shrink_test() ->

  Gen = list(frequency_gen([
    {5, {oneof([a, b, c, d, e]), oneof([1, 2, 3])}},
    {10, integer()}
  ])),

  Shrinks = dorer_lazyseq:to_list(shrinks(Gen, [{1, {c, 3}}, {2, -2}, {2, 0}])),
  ?assertEqual([
    [{2, -2}, {2, 0}],
    [{1, {c, 3}}, {2, 0}],
    [{1, {c, 3}}, {2, -2}],
    [{1, {a, 3}}, {2, -2}, {2, 0}],
    [{1, {c, 3}}, {2, 2}, {2, 0}],
    [{1, {c, 1}}, {2, -2}, {2, 0}],
    [{1, {c, 3}}, {2, 0}, {2, 0}],
    [{1, {b, 3}}, {2, -2}, {2, 0}],
    [{1, {c, 3}}, {2, -1}, {2, 0}],
    [{1, {c, 2}}, {2, -2}, {2, 0}]
  ], Shrinks),

  Adapted = [try_adapt_value(Gen, V) || V <- Shrinks],
  ?assertEqual(Shrinks, Adapted).






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

shrink_list3_test() ->
  ?assertEqual([
    [5, 7],
    [1, 7],
    [1, 5],
    [0, 5, 7],
    [1, 0, 7],
    [1, 5, 0],
    [1, 3, 7],
    [1, 5, 4],
    [1, 4, 7],
    [1, 5, 6]],
    dorer_lazyseq:to_list(shrinks(list(integer()), [1, 5, 7]))).

shrink_set_test() ->
  ?assertEqual([
    [5, 7],
    [1, 7],
    [1, 5],
    [0, 5, 7],
    [0, 1, 7],
    [0, 1, 5],
    [1, 3, 7],
    [1, 4, 5],
    [1, 4, 7],
    [1, 5, 6]],
    dorer_lazyseq:to_list(shrinks(set(integer()), [1, 5, 7]))).

shrink_map_test() ->
  ?assertEqual(
    [#{b => 2, d => 5},
      #{a => 10, d => 5},
      #{a => 10, b => 2},
      #{a => 0, b => 2, d => 5},
      #{a => 2, d => 5},
      #{a => 5, b => 2},
      #{a => 5, b => 2, d => 5},
      #{a => 10, b => 0, d => 5},
      #{a => 10, b => 2, d => 0},
      #{a => 8, b => 2, d => 5},
      #{a => 10, b => 1, d => 5},
      #{a => 10, b => 5},
      #{a => 9, b => 2, d => 5},
      #{a => 10, b => 2, d => 3},
      #{a => 10, b => 2, c => 5},
      #{a => 10, b => 2, d => 4}],
    dorer_lazyseq:to_list(shrinks(map(oneof([a, b, c, d]), integer()), #{a => 10, b => 2, d => 5}))).

remove_duplicates_twice_test() ->
  ?assertEqual([a, c], remove_duplicates_twice(lists:sort([a, b, c, a, b, b, a, b]))).

-endif.




