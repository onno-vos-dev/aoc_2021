-module(day13).

-export([solve/0]).

%% API ========================================================================
solve() ->
  {Grid, FoldOpsReversed} = input(),
  FoldOps = lists:reverse(FoldOpsReversed),
  Part1 = fold_ops(Grid, [hd(FoldOps)]),
  Part2 = fold_ops(Part1, tl(FoldOps)),
  io:format("Part1: ~p~n", [maps:size(Part1)]),
  print(Part2).

%% Logic ======================================================================
fold_ops(Grid, []) -> Grid;
fold_ops(Grid, [Op | T]) ->
  fold_ops(fold(Op, Grid), T).

fold({Dir, Line}, Grid) ->
  maps:fold(fun({X, Y}, _, Acc) when Dir =:= x andalso X > Line -> Acc#{{2 * Line - X, Y} => []};
               ({X, Y}, _, Acc) when Dir =:= y andalso Y > Line -> Acc#{{X, 2 * Line - Y} => []};
               ({X, Y}, _, Acc) -> Acc#{{X, Y} => []}
            end, #{}, Grid).

print(Part2) ->
  io:format("Part2: ~n"),
  lists:foreach(fun(Line) ->
                 print_line(maps:to_list(maps:filter(fun({_, Y}, _) -> Y =:= Line end, Part2)))
               end, lists:usort([ Y || {_, Y} <- maps:keys(Part2) ])).

print_line(Chars) ->
  Xs = [ X || {{X, _}, []} <- Chars ],
  Str = lists:foldl(fun(I, Acc) ->
                      case lists:member(I, Xs) of
                        true -> [ "#" | Acc ];
                        false -> [ " " | Acc ]
                      end
                    end, [], lists:seq(0, lists:max(Xs))),
  case hd(lists:reverse(Str)) of
    $\s ->
      io:format("~s~n", [[" " | lists:reverse(Str)]]);
    _ ->
      io:format("~s~n", [lists:reverse(Str)])
  end.

%% Parsing ====================================================================
input() ->
  FoldFun = fun(<<>>, Acc) -> Acc;
               (<<"fold along y=", Rest/binary>>, {Grid, FoldInstructions}) ->
                {Grid, [{y, binary_to_integer(Rest)} |  FoldInstructions]};
               (<<"fold along x=", Rest/binary>>, {Grid, FoldInstructions}) ->
                {Grid, [{x, binary_to_integer(Rest)} |  FoldInstructions]};
               (Bin, {Grid, FoldInstructions}) ->
                 [X, Y] = string:tokens(binary_to_list(Bin), ","),
                 {maps:put({list_to_integer(X), list_to_integer(Y)}, [], Grid), FoldInstructions}
            end,
  util:read_file_fold("day13.txt", <<"\n">>, FoldFun, {#{}, []}).
