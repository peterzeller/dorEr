-module(dorer_examples).
-include_lib("eunit/include/eunit.hrl").

-import(dorer_generators, [integer/0, transform/2, such_that/2, frequency/1, frequency_gen/1, range/2, oneof/1]).

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
      {1, range(1,10)},
      {10, range(1000, 2000)}
    ])),
    dorer:log("generated ~p", [X]),
    ?assert(is_integer(X))
  end).

