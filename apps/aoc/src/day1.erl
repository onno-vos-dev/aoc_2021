-module(day1).

-export([ solve/0 ]).

solve() ->
  Input = input(),
  Part1 = do_solve(fun() -> Input end),
  Part2 = do_solve(fun() -> sum_threes(Input, []) end),
  {Part1, Part2}.

input() ->
  util:read_file("day1.txt", <<"\n">>, fun erlang:binary_to_integer/1).

do_solve(Fun) ->
  Input = Fun(),
  do_solve(tl(Input), {hd(Input), 0}).

do_solve([], {_Last, Acc}) ->
  Acc;
do_solve([H | T], {Last, Acc}) ->
  case H > Last of
    true ->
      do_solve(T, {H, Acc + 1});
    false ->
      do_solve(T, {H, Acc})
  end.

sum_threes([_, _], Acc) -> lists:reverse(Acc);
sum_threes([H1, H2, H3 | T], Acc) ->
  sum_threes([H2, H3 | T], [H1 + H2 + H3 | Acc]).
