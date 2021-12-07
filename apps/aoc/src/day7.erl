-module(day7).

-export([ solve/0, solve/1, input/0 ]).

solve() ->
  solve(input()).

solve(Input) ->
  Part1 = calculate_fuel(floor(util:median(Input)), Input, fun(A) -> A end),
  Part2 = calculate_fuel(ceil(util:mean(Input)) - 1, Input, fun termial/1),
  {348996, 98231647} = {Part1, Part2}.

calculate_fuel(H, Input, F) ->
  round(do_calculate_fuel(H, Input, F, 0)).

do_calculate_fuel(_Pos, [], _F, Acc) -> Acc;
do_calculate_fuel(Pos, [H | T], F, Acc) ->
  Move = Pos - H,
  case Move >= 0 of
    true ->
      do_calculate_fuel(Pos, T, F, F(Move) + Acc);
    false ->
      do_calculate_fuel(Pos, T, F, F(Move * -1) + Acc)
  end.

termial(X) -> X * (X + 1) div 2.

input() ->
  util:read_file("day7.txt", <<",">>, fun erlang:binary_to_integer/1).
