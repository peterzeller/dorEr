-module(dorer_examples_SUITE).

-include_lib("eunit/include/eunit.hrl").


-export([all/0, simple2_test/1]).

all() -> [
  simple2_test
].

biggest([Head | _Tail]) ->
  Head.

%%simple_test() ->
%%  dorer:check(fun() ->
%%    List = dorer:gen(dorer_generators:list(dorer_generators:integer())),
%%    dorer:log("List = ~p", [List]),
%%    io:format(user, "List = ~p~n", [List]),
%%    case List of
%%      [] -> ok;
%%      _ ->
%%        ?assertEqual(lists:last(lists:sort(List)), biggest(List))
%%    end
%%  end).


gen_list() ->
  case dorer:gen(list_elements, dorer_generators:has_more()) of
    false -> [];
    true ->
      X = dorer:gen([list_elements, elem], dorer_generators:integer()),
      [X | gen_list()]
  end.

simple2_test(_Config) ->
  dorer:check(fun() ->
    List = gen_list(),
    dorer:log("List = ~p", [List]),
    case List of
      [] -> ok;
      _ ->
        ?assertEqual(lists:last(lists:sort(List)), biggest(List))
    end
  end).
