-module(dorer_lazyseq).


-export([empty/0, append/2, from_list/1, iterate/2, remove_first/1, map/2, flatmap/2, append/1, to_list/1, filter/2]).

-export_type([seq/1]).

-type seq(T) ::
fun(() -> eof | {next, T, seq(T)})
| list(T).

-spec empty() -> seq(any()).
empty() ->
  fun() -> eof end.

-spec append([seq(T)]) -> seq(T).
append([]) -> [];
append([X]) -> X;
append([X | Xs]) -> append(X, append(Xs)).

-spec append(seq(T), seq(T)) -> seq(T).
append(A, B) ->
  fun() ->
    case remove_first(A) of
      eof -> remove_first(B);
      {next, T, A2} ->
        {next, T, append(A2, B)}
    end
  end.

-spec iterate(S, fun((S) -> eof | {next, T, S} | {nexts, seq(T), S})) -> seq(S).
iterate(First, Next) ->
  fun() ->
    case Next(First) of
      eof -> eof;
      {next, T, S} ->
        {next, T, iterate(S, Next)};
      {nexts, Ts, S} ->
        case remove_first(Ts) of
          eof -> (iterate(S, Next))();
          {next, T, Ts2} ->
            {next, T, append(Ts2, iterate(S, Next))}
        end
    end
  end.

-spec from_list([T]) -> seq(T).
from_list(List) -> List.

-spec to_list(seq(T)) -> list(T).
to_list(L) ->
  case remove_first(L) of
    eof -> [];
    {next, X, Rest} ->
      [X | to_list(Rest)]
  end.

-spec remove_first(seq(T)) -> eof | {next, T, seq(T)}.
remove_first([]) -> eof;
remove_first([X | Xs]) -> {next, X, Xs};
remove_first(A) when is_function(A) -> A().


-spec map(fun((A) -> B), seq(A)) -> seq(B).
map(F, Seq) ->
  fun() ->
    case remove_first(Seq) of
      eof -> eof;
      {next, X, Rest} ->
        {next, F(X), map(F, Rest)}
    end
  end.

-spec filter(fun((A) -> boolean()), seq(A)) -> seq(A).
filter(Pred, Seq) ->
  fun() ->
    case remove_first(Seq) of
      eof -> eof;
      {next, X, Rest} ->
        {nexts, [X || Pred(X)], filter(Pred, Rest)}
    end
  end.

-spec flatmap(fun((A) -> seq(B)), seq(A)) -> seq(B).
flatmap(F, Seq) ->
  fun() ->
    case remove_first(Seq) of
      eof -> eof;
      {next, X, Rest} ->
        M = F(X),
        remove_first(append(M, flatmap(F, Rest)))
    end
  end.
