-module(day09).

-export([ solve/0, input/0 ]).

solve() ->
  Grid = to_grid(input()),
  Lows = find_lows(Grid),
  Basins = find_basins(Lows, Grid, []),
  Part1 = lists:sum(lists:map(fun({_Pos, I}) -> I + 1 end, Lows)),
  [A, B, C | _] = lists:reverse(lists:sort(Basins)),
  Part2 = A * B * C,
  {558, 882942} = {Part1, Part2}.

input() ->
  util:read_file("day9.txt", <<"\n">>, fun binary_to_list/1).

to_grid(Lines) ->
  {_, Grid} = lists:foldl(fun(Line, {Y, Acc}) ->
                            to_grid(Line, {Y, Acc})
                          end, {1, []}, Lines),
  maps:from_list(Grid).

to_grid(Line, {Y, Acc}) ->
  {_, NewAcc} = lists:foldl(fun(V, {X, A}) ->
                              {X + 1, [{{X, Y}, V - $0} | A]}
                            end, {1, Acc}, Line),
  {Y + 1, NewAcc}.

find_lows(Grid) ->
  maps:fold(fun(K, V, Acc) ->
               case is_lowest(K, V, Grid) of
                 true -> [{K, V} | Acc];
                 false -> Acc
               end
            end,
            [],
            Grid).

is_lowest({X, Y}, V, Grid) ->
  maps:get({X, Y - 1}, Grid, 9) > V andalso
  maps:get({X, Y + 1}, Grid, 9) > V andalso
  maps:get({X + 1, Y}, Grid, 9) > V andalso
  maps:get({X - 1, Y}, Grid, 9) > V.

find_basins([], _Grid, Acc) -> Acc;
find_basins([{Pos, Height} | T], Grid, Acc) ->
  Neighbors = neighbors(Pos, Height, #{}, Grid),
  Seen = lists:foldl(fun({P, _}, A) -> maps:put(P, [], A) end, #{}, Neighbors),
  find_basins(T, Grid, [calculate_basin(Neighbors, Grid, {1, Seen}) | Acc]).

calculate_basin([] = _Neighbors, _Grid, {Size, _Seen}) -> Size;
calculate_basin([{Pos, Height} | T], Grid, {Size, Seen}) ->
  Neighbors = neighbors(Pos, Height, Seen, Grid),
  case Neighbors of
    [] -> calculate_basin(T, Grid, {Size + 1, Seen});
    _ ->
      NewSeen = lists:foldl(fun({P, _}, A) -> maps:put(P, [], A) end, Seen, Neighbors),
      calculate_basin(Neighbors ++ T, Grid, {Size + 1, NewSeen})
  end.

neighbors({X, Y}, Height, Seen, Grid) ->
  lists:filtermap(fun(Pos) ->
                 Val = maps:get(Pos, Grid, 9),
                 case not maps:is_key(Pos, Seen) andalso Val =/= 9 andalso Val >= Height of
                   false -> false;
                   true -> {true, {Pos, Val}}
                 end
               end, [{X, Y - 1}, {X, Y + 1}, {X - 1, Y}, {X + 1, Y}]).
