-module(day06).

-export([ solve/0, solve/1, input/0 ]).

solve() ->
  solve(input()).

solve(Input) ->
  Output1 = fishes({0, 80}, Input),
  Output2 = fishes({80, 256}, Output1),
  Part1 = lists:sum(maps:values(Output1)),
  Part2 = lists:sum(maps:values(Output2)),
  {352872, 1604361182149} = {Part1, Part2}.

fishes({Day, MaxDays}, Acc) when Day =:= MaxDays -> Acc;
fishes({Day, MaxDays}, #{ 0 := Zero
                        , 1 := One
                        , 2 := Two
                        , 3 := Three
                        , 4 := Four
                        , 5 := Five
                        , 6 := Six
                        , 7 := Seven
                        , 8:= Eigth}) ->
  NewAcc = #{ 0 => One
            , 1 => Two
            , 2 => Three
            , 3 => Four
            , 4 => Five
            , 5 => Six
            , 6 => Zero + Seven
            , 7 => Eigth
            , 8 => Zero},
  fishes({Day + 1, MaxDays}, NewAcc).

input() ->
  Acc = #{ 0 => 0, 1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 0, 6 => 0, 7 => 0, 8 => 0},
  FoldFun = fun(B, A) ->
              Int = binary_to_integer(B),
              #{Int := Old} = A,
              A#{Int => Old + 1}
            end,
  util:read_file_fold("day6.txt", <<",">>, FoldFun, Acc).
