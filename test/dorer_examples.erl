-module(dorer_examples).
-author("peter").

-include_lib("eunit/include/eunit.hrl").

biggest([Head | _Tail]) ->
  Head.

simple_test() ->
  dorer:check(fun() ->
    List = dorer:gen(dorer_generators:list(dorer_generators:integer())),
    io:format(user, "List = ~p~n", [List]),
    case List of
      [] -> ok;
      _ ->
        ?assertEqual(lists:last(lists:sort(List)), biggest(List))
    end
  end).
