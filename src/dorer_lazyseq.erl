-module(dorer_lazyseq).


-export([empty/0, append/2, from_list/1, iterate/2, remove_first/1, map/2, flatmap/2, append/1]).

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
append([X|Xs]) -> append(X, append(Xs)).

-spec append(seq(T), seq(T)) -> seq(T).
append(A, B) ->
  fun() ->
    case remove_first(A) of
      eof -> B();
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

-spec flatmap(fun((A) -> seq(B)), seq(A)) -> seq(B).
flatmap(F, Seq) ->
  fun() ->
    case remove_first(Seq) of
      eof -> eof;
      {next, X, Rest} ->
        M = F(X),
        case remove_first(M) of
          eof -> flatmap(F, Rest);
          {next, MX, MRest} ->
            {next, MX, append(MRest, flatmap(F, Rest))}
        end
    end
  end.