-module(day7).

-export([ solve/0, solve/1, input/0 ]).

solve() ->
  solve(input()).

solve(Input) ->
  {348996, 98231647} = {part1(Input), part2(Input)}.

part1(Input) ->
  MinMax = lists:seq(lists:min(Input), lists:max(Input)),
  calculate_fuel(MinMax, Input, fun(A) -> A end, []).

part2(Input) ->
  MinMax = lists:seq(lists:min(Input), lists:max(Input)),
  F = fun termial/1,
  calculate_fuel(MinMax, Input, F, []).

calculate_fuel([], _Input, _F, Acc) ->
  lists:min(Acc);
calculate_fuel([H | T], Input, F, Acc) ->
  FuelCost = do_calculate_fuel(H, Input, F, 0),
  calculate_fuel(T, Input, F, [ FuelCost | Acc ]).

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
