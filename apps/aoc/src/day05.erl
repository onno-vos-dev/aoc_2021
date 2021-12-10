-module(day05).

-export([solve/0]).

solve() ->
  Input = input(),
  Tid = ets:new(?MODULE, [set, private]),
  {Part1, _} = HVAcc = build_coordinates(Input, false, {0, Tid}),
  {Part2, _} = build_coordinates(Input, true, HVAcc),
  {6856, 20666} = {Part1, Part2}.

input() ->
  util:read_file("day5.txt",
                 <<"\n">>,
                 fun(A) ->
                   [X1, Y1, X2, Y2] = string:tokens(binary_to_list(A), ", ->"),
                   [ {list_to_integer(X1), list_to_integer(Y1)}
                   , {list_to_integer(X2), list_to_integer(Y2)}
                   ]
                 end).

build_coordinates([], _Diagonal, Acc) -> Acc;
build_coordinates([[{X1, Y1}, {X2, Y2}] | T], Diagonal, Acc) ->
  case {X1 =:= X2, Y1 =:= Y2} of
    {true, false} when not Diagonal ->
      build_coordinates(T, Diagonal, new_acc(Acc, X1, 1, seq(Y1, Y2)));
    {false, true} when not Diagonal ->
      build_coordinates(T, Diagonal, new_acc(Acc, Y1, 2, seq(X1, X2)));
    {false, false} when Diagonal ->
      Positions = diagonal_positions({X1, Y1}, {X2, Y2}),
      NewAcc = lists:foldl(fun(Key, A) ->
                             update_acc(Key, A)
                           end, Acc, Positions),
      build_coordinates(T, Diagonal, NewAcc);
    _ ->
      build_coordinates(T, Diagonal, Acc)
  end.

new_acc(Acc, Z, KeyPos, Positions) ->
  lists:foldl(fun(Pos, A) when KeyPos =:= 1->
                   update_acc({Z, Pos}, A);
                 (Pos, A) when KeyPos =:= 2->
                  update_acc({Pos, Z}, A)
              end, Acc, Positions).

update_acc(Key, {Count, Tid}) ->
  case ets:update_counter(Tid, Key, {2, 1}, {Key, 0}) of
    1 -> {Count, Tid};
    2 -> {Count + 1, Tid};
    _ -> {Count, Tid}
  end.

diagonal_positions({X1, Y1}, {X2, Y2}) ->
  L = lists:seq(0, subtract(X1, X2)),
  Xs = lists:map(fun(X) when X1 > X2 -> X1 - X;
                    (X) -> X1 + X
                 end, L),
  Ys = lists:map(fun(Y) when Y1 > Y2 -> Y1 - Y;
                    (Y) -> Y1 + Y
                 end, L),
  lists:zip(Xs, Ys).

subtract(X1, X2) when X1 > X2 -> X1 - X2;
subtract(X1, X2) -> X2 - X1.

seq(A, B) when A =< B -> lists:seq(A, B);
seq(A, B) when A > B -> lists:seq(B, A).
