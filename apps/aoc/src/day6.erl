-module(day6).

-export([ solve/0, initialize/1, input/0 ]).

solve() ->
  Acc = initialize(input()),
  Output1 = fishes({0, 80}, Acc),
  Output2 = fishes({80, 256}, Output1),
  Part1 = lists:sum(maps:values(Output1)),
  Part2 = lists:sum(maps:values(Output2)),
  {Part1, Part2}.

initialize(Input) ->
  Acc = #{ 0 => 0, 1 => 0, 2 => 0, 3 => 0, 4 => 0, 5 => 0, 6 => 0, 7 => 0, 8 => 0},
  lists:foldl(fun(In, A) -> maps:update(In, maps:get(In, A) + 1, A) end, Acc, Input).

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
  util:read_file("day6.txt", <<",">>, fun erlang:binary_to_integer/1).
