-module(day01).

-export([ solve/0 ]).

solve() ->
  Input = input(),
  Part1 = do_solve(fun() -> Input end),
  Part2 = do_solve(fun() -> aoc_helpers:sum_sliding_threes(Input) end),
  {1462, 1497} = {Part1, Part2}.

input() ->
  util:read_file("day1.txt", <<"\n">>, fun erlang:binary_to_integer/1).

do_solve(Fun) ->
  Input = Fun(),
  aoc_helpers:count_increases(Input).
