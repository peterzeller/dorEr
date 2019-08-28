-module(dorer_generators).

%% API
-export([integer/0, list/1, random_gen/2, shrinks/2]).

-type random_gen(T) :: #{
produce := fun((Size :: integer()) -> T),
shrink := fun((T) -> dorer_lazyseq:seq(T))
}.

-type generator(T) :: #{
random_gen => random_gen(T),
small_gen => fun((Bound :: integer()) -> dorer_lazyseq:seq(T))
}.


-spec integer() -> generator(integer()).
integer() ->
  #{
    random_gen => #{
      produce => fun(Size) ->
        rand:uniform(2 * Size) - Size
      end,
      shrink => fun(T) ->
        if
          T > 0 -> [T / 2, T - 1];
          T < 0 -> [T / 2, T + 1];
          true -> []
        end
      end
    },
    small_gen => fun(Bound) ->
      dorer_lazyseq:iterate(0, fun
        (0) -> {next, 0, 1};
        (S) when S < Bound -> {nexts, [S, -S], S + 1};
        (_) -> eof
      end)
    end
  }.

-spec list(generator(T)) -> generator(list(T)).
list(ItemGen) ->
  #{
    random_gen => #{
      produce => fun(Size) -> produce_list(ItemGen, Size) end,
      shrink => fun
        ([]) -> [];
        (List) ->
          N = length(List),
          {First, Second} = lists:split(N div 2, List),
          HalfLists = [First, Second],
          OneRemoved = dorer_lazyseq:iterate(0, fun
            (I) when I + 1 >= N -> eof;
            (I) ->
              {A, [_ | B]} = lists:split(I, List),
              {next, A ++ B, I + 1}
          end),
          OneShrunk = dorer_lazyseq:iterate(0, fun
            (I) when I + 1 >= N -> eof;
            (I) ->
              {A, [X | B]} = lists:split(I, List),
              {nexts, dorer_lazyseq:map(fun(X2) -> A ++ [X2 | B] end, shrinks(ItemGen, X)), I + 1}
          end),
          dorer_lazyseq:append([HalfLists, OneRemoved, OneShrunk])
      end
    }
  }.

produce_list(_ItemGen, 0) -> [];
produce_list(ItemGen, N) ->
  case rand:uniform(N) of
    1 -> [];
    _ ->
      [random_gen(ItemGen, N) | produce_list(ItemGen, N - 1)]
  end.


-spec random_gen(generator(T), non_neg_integer()) -> T.
random_gen(Generator, Size) ->
  #{random_gen := #{produce := Produce}} = Generator,
  Produce(Size).

-spec shrinks(generator(T), T) -> dorer_lazyseq:seq(T).
shrinks(Generator, X) ->
  #{random_gen := #{shrink := Shrink}} = Generator,
  Shrink(X).