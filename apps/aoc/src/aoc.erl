-module(aoc).

-export([run/0]).

run() ->
  Days = days(),
  single_pass(Days),
  multi_pass(Days).

single_pass(Days) ->
  io:format("Results for single iteration: ~n"),
  lists:foreach(fun(Day) ->
                  io:format("~p -> ", [Day]),
                  {T, _} = timer:tc(fun Day:solve/0),
                  io:format("Time: ~p (~p ms)~n", [T, T div 1000])
                end, Days).

multi_pass(Days) ->
  io:format("~nResults for 50 iterations: ~n"),
  lists:foreach(fun(Day) ->
                  io:format("~p -> ", [Day]),
                  util:time_avg(fun Day:solve/0, 50)
                end, Days).

days() ->
  Files = os:cmd("ls " ++ filename:join([code:priv_dir(aoc), "..", "src"])),
  lists:filtermap(fun("day" ++ _ = Day) ->
                    {true, list_to_existing_atom(filename:basename(Day, ".erl"))};
                  (_) ->
                    false
               end, string:tokens(Files, "\n")).
