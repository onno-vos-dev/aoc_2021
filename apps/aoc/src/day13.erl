-module(day13).

-export([solve/0, print/1]).

%% API ========================================================================
solve() ->
  {Grid, FoldOpsReversed} = input(),
  FoldOps = lists:reverse(FoldOpsReversed),
  Part1 = fold_ops(Grid, [hd(FoldOps)]),
  Part2 = fold_ops(Part1, tl(FoldOps)),
  {850, Part2} = {maps:size(Part1), part2_expected_output()}.

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

part2_expected_output() ->
  #{{1,3} => [], {26,5} => [], {22,3} => [], {5,4} => [], {38,4} => [], {32,3} => [], {31,0} => [],
  {0,2} => [], {10,2} => [], {35,2} => [], {28,3} => [], {5,1} => [], {20,1} => [], {30,2} => [],
  {13,3} => [], {12,0} => [], {16,0} => [], {1,0} => [], {18,4} => [], {3,5} => [], {10,1} => [],
  {30,4} => [], {28,4} => [], {8,0} => [], {5,5} => [], {15,4} => [], {5,3} => [], {30,5} => [],
  {22,0} => [], {28,5} => [], {33,5} => [], {20,4} => [], {13,4} => [], {38,0} => [], {21,3} => [],
  {13,5} => [], {3,3} => [], {20,2} => [], {36,5} => [], {11,0} => [], {15,1} => [], {16,5} => [],
  {18,1} => [], {20,0} => [], {3,4} => [], {33,1} => [], {8,2} => [], {33,2} => [], {10,4} => [],
  {12,5} => [], {2,3} => [], {21,0} => [], {0,4} => [], {30,3} => [], {27,3} => [], {25,2} => [],
  {27,5} => [], {31,3} => [], {7,2} => [], {6,2} => [], {17,5} => [], {23,2} => [], {23,1} => [],
  {27,0} => [], {8,4} => [], {8,3} => [], {2,0} => [], {3,1} => [], {25,1} => [], {11,5} => [],
  {33,4} => [], {35,4} => [], {37,5} => [], {8,1} => [], {10,3} => [], {3,2} => [], {35,1} => [],
  {13,1} => [], {25,3} => [], {20,5} => [], {0,1} => [], {0,5} => [], {26,0} => [], {5,0} => [],
  {35,3} => [], {35,0} => [], {25,4} => [], {0,3} => [], {38,3} => [], {30,1} => [], {38,1} => [],
  {33,3} => [], {28,1} => [], {5,2} => [], {12,3} => [], {15,2} => [], {8,5} => [], {32,0} => [],
  {15,3} => [], {17,0} => [], {20,3} => [], {38,2} => []}.

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
