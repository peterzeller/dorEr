-module(dorer_list_utils).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


-export([group_by_first/1, group_by/2, group_by/4, reduce/2, topsort/2, is_prefix/2, with_index/1, iterate/2, pick_most_similar/2, confuse_dialyzer/1, loop/2, limit/2]).


%% groups a list of key-value pairs by key
-spec group_by_first([{K, V}]) -> #{K => [V]}.
group_by_first(List) ->
  group_by(
    fun({K, _}) -> K end,
    fun({_, V}) -> [V] end,
    fun({_, V}, Xs) -> [V | Xs] end,
    List
  ).

% groups
-spec group_by(fun((E) -> K), [E]) -> #{K => [E]}.
group_by(F, List) ->
  group_by(F, fun(X) -> [X] end, fun(X, Xs) -> [X | Xs] end, List).

-spec group_by(fun((E) -> K), fun((E) -> V), fun((E, V) -> V), [E]) -> #{K => V}.
group_by(F, Init, Merge, List) ->
  lists:foldr(fun(X, M) ->
    K = F(X),
    maps:update_with(K, fun(L) -> Merge(X, L) end, Init(X), M)
  end, maps:new(), List).


-spec reduce(fun((E, E) -> E), [E]) -> E.
reduce(_, []) -> throw('cannot reduce empty list');
reduce(_, [X]) -> X;
reduce(M, [X, Y | Xs]) -> reduce(M, [M(X, Y) | Xs]).


topsort(_Cmp, []) -> [];
topsort(Cmp, Xs) ->
  % Min are all elements X from Xs, such that for all elements Y from Xs: not Y < X
  {Min, NotMin} = lists:partition(
    fun(X) ->
      lists:all(fun(Y) -> not Cmp(Y, X) end, Xs)
    end,
    Xs
  ),
  Min ++ topsort(Cmp, NotMin).


-spec is_prefix([T], [T]) -> boolean().
is_prefix([], _) -> true;
is_prefix([X | Xs], [X | Ys]) ->
  is_prefix(Xs, Ys);
is_prefix(_, _) -> false.

-spec with_index([T]) -> [{non_neg_integer(), T}].
with_index(List) ->
  lists:zip(lists:seq(0, length(List) - 1), List).


-spec iterate(T, fun((T) -> break | {continue, T})) -> [T].
iterate(Start, Fun) ->
  case Fun(Start) of
    break -> [];
    {continue, X} -> [X | iterate(X, Fun)]
  end.


-spec loop(T, fun((T) -> {return, X} | {continue, T})) -> X.
loop(Start, Fun) ->
  case Fun(Start) of
    {return, X} -> X;
    {continue, X} -> loop(X, Fun)
  end.

-spec limit(integer(), [T]) -> [T].
limit(N, [X | Xs]) when N > 0 ->
  [X | limit(N - 1, Xs)];
limit(_, _) -> [].



pick_most_similar(_Elem, []) -> throw('no choices available');
pick_most_similar(Elem, List) ->
  case lists:member(Elem, List) of
    true -> Elem;
    false ->
      WithSimilarity = [{similarity(Elem, X), X} || X <- List],
      {_, Res} = lists:max(WithSimilarity),
      Res
  end.


similarity(X, X) -> 1;
similarity(X, Y) when is_atom(X) andalso is_atom(Y) ->
  0.1;
similarity(X, Y) when is_list(X) andalso is_list(Y) ->
  case {X, Y} of
    {[], _} -> 0.1;
    {_, []} -> 0.1;
    {[A | As], [A | Bs]} ->
      L = max(length(As), length(Bs)),
      1 / (1 + L) + similarity(As, Bs) * L / (1 + L);
    {[A | As], Xs} ->
      L = max(1 + length(As), length(Xs)),
      Sim = pick_most_similar(A, Xs),
      similarity(A, Sim) / L + similarity(As, Xs -- [Sim]) * (L - 1) / L
  end;
similarity(X, Y) when is_tuple(X) andalso is_tuple(Y) ->
  0.5 + similarity(tuple_to_list(X), tuple_to_list(Y)) * 0.5;
similarity(X, Y) when is_map(X) andalso is_map(Y) ->
  0.5 + similarity(maps:to_list(X), maps:to_list(Y)) * 0.5;
similarity(_, _) -> 0.


% the identitiy function to confuse dialyzer in case of false positives
-spec confuse_dialyzer(T) -> T.
confuse_dialyzer(X) -> X.


-ifdef(TEST).
group_by_first_test() ->
  M = group_by_first([{a, 1}, {b, 2}, {a, 3}, {a, 4}]),
  ?assertEqual(#{a => [1, 3, 4], b => [2]}, M).

with_index_test() ->
  ?assertEqual([{0, a}, {1, b}, {2, c}], with_index([a, b, c])).

-endif.
