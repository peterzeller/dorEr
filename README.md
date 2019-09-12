Dorer
=====

Dorer is a library for property based testing.
 
It helps you explore different execution paths and inputs with your property based tests.

Examples
--------

This property tests a function named `biggest`, which should return the biggest element in a nonempty list.
We check that the result is always equal to the last element in the sorted list: 

    simple_test(_Config) ->
      dorer:check(fun() ->
        List = dorer:gen(dorer_generators:list(dorer_generators:integer())),
        case List of
          [] -> ok;
          _ ->
            ?assertEqual(lists:last(lists:sort(List)), biggest(List))
        end
      end).

If biggest is implemented incorrectly and always returns the first element in a list, Dorer finds the following counter example.

    Generated Values:
     [default]: {list,[integer]} => [-1,0]
    
    
    LOG:
    
    ERROR error: {assertEqual,[{module,dorer_examples_SUITE},
                               {line,22},
                               {expression,"biggest ( List )"},
                               {expected,0},
                               {value,-1}]}



Usage
-----

Add `dorer` to your list of dependencies.
Then use `dorer:check` to run a function multiple times, each exploring a different execution.
This can be done from within a normal common test suite or eunit test (if you set the timeout high enough).

Inside the check function you should call `dorer:gen` to generate different values.
For debugging use `dorer:log`.


### Options

Options are specified using a map that can have the following entries:

    -type strategy() :: small | random.
    -type options() :: #{
        strategy => strategy(),
        max_shrink_time => {pos_integer(), erlang:time_unit()},
        n => integer(),
        print_generated_values => boolean() | if_log_empty
    }.

Default values:

    #{
      strategy => random,
      max_shrink_time => {3, second},
      n => 100
      print_generated_values => if_log_empty
    }

Build
-----

    $ rebar3 compile
