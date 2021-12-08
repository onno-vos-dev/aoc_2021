-module(day7).

-export([ solve/0, solve/1, input/0 ]).

-dialyzer({nowarn_function, [ solve/0
                            , solve/1
                            , input/0
                            , calc_fuel/3
                            , do_calc_fuel/4
                            ]}).

solve() ->
  solve(input()).

solve(Input) ->
  Median = floor(util:median_nif(Input)),
  Mean = ceil(util:mean_nif(Input)) - 1,
  {348996, 98231647} = calc_fuel({Median, Mean}, Input, fun termial/1).

calc_fuel({Median, Mean}, Input, F) ->
  do_calc_fuel({Median, Mean}, Input, F, {0, 0}).

do_calc_fuel({_Median, _Mean}, [], _F, {AccMedian, AccMean}) -> {AccMedian, AccMean};
do_calc_fuel({Median, Mean}, [H | T], F, {AccMedian, AccMean}) ->
  {MMedian, MMove} = {Median - H, Mean - H},
  case {MMedian >= 0, MMove >= 0} of
    {true, true} ->
      do_calc_fuel({Median, Mean}, T, F, {MMedian + AccMedian, F(MMove) + AccMean});
    {true, false} ->
      do_calc_fuel({Median, Mean}, T, F, {MMedian + AccMedian, F(MMove * -1) + AccMean});
    {false, true} ->
      do_calc_fuel({Median, Mean}, T, F, {MMedian * - 1 + AccMedian, F(MMove) + AccMean});
    {false, false} ->
      do_calc_fuel({Median, Mean}, T, F, {MMedian * - 1 + AccMedian, F(MMove * -1) + AccMean})
  end.

termial(X) -> X * (X + 1) div 2.

input() ->
  util:read_file("day7.txt", <<",">>, fun erlang:binary_to_integer/1).
