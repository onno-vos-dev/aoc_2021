-module(aoc).

-export([ single_pass/0
        , single_pass/1
        , multi_pass/0
        , multi_pass/1
        , multi_pass/2
        ]).

single_pass() ->
  single_pass(days()).

single_pass(Days) ->
  io:format("Results for single iteration: ~n"),
  lists:foreach(fun(Day) ->
                  io:format("~p -> ", [Day]),
                  {T, _} = timer:tc(fun Day:solve/0),
                  io:format("Time: ~p us (~p ms)~n", [T, T div 1000])
                end, Days).

multi_pass() ->
  multi_pass(days()).

multi_pass(Days) ->
  multi_pass(Days, 10).

multi_pass(Days, N) ->
  io:format("~nResults for ~p iterations: ~n", [N]),
  lists:foreach(fun(Day) ->
                  io:format("~p -> ", [Day]),
                  util:time_avg(fun Day:solve/0, N)
                end, Days).

days() ->
  Files = os:cmd("ls " ++ filename:join([code:priv_dir(aoc), "..", "src"])),
  lists:filtermap(fun("day" ++ _ = Day) ->
                    {true, list_to_existing_atom(filename:basename(Day, ".erl"))};
                  (_) ->
                    false
               end, string:tokens(Files, "\n")).
