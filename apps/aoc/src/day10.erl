-module(day10).

-export([solve/0]).

solve() ->
  {Part1, Part2} = input(),
  {318081, 4361305341} = {Part1, round(util:median(Part2))}.

input() ->
  FoldFun = fun(Line, {Part1Acc, Part2Acc}) ->
              %% Apparently binary_to_list/1 and iterating the list of chars is faster
              %% than iterating the binary using <<H:1/binary, T/binary>> so going with that.
              case score(binary_to_list(Line)) of
                {unexpected, Points} -> {Points + Part1Acc, Part2Acc};
                {incomplete, Chars} -> {Part1Acc, [incomplete_points(Chars) | Part2Acc]}
              end
            end,
  Acc = {0, []},
  util:read_file_fold("day10.txt", <<"\n">>, FoldFun, Acc).

score(Line) ->
  do_score(Line, []).

do_score([], Acc) -> {incomplete, Acc};
do_score([$( | T], []) -> do_score(T, [$)]);
do_score([$[ | T], []) -> do_score(T, [$]]);
do_score([${ | T], []) -> do_score(T, [$}]);
do_score([$< | T], []) -> do_score(T, [$>]);
do_score([$) | T1], [$) | T2]) -> do_score(T1, T2);
do_score([$] | T1], [$] | T2]) -> do_score(T1, T2);
do_score([$} | T1], [$} | T2]) -> do_score(T1, T2);
do_score([$> | T1], [$> | T2]) -> do_score(T1, T2);
do_score([$( | T], Acc) -> do_score(T, [$) | Acc]);
do_score([$[ | T], Acc) -> do_score(T, [$] | Acc]);
do_score([${ | T], Acc) -> do_score(T, [$} | Acc]);
do_score([$< | T], Acc) -> do_score(T, [$> | Acc]);
do_score([$) | _], _) -> {unexpected, 3};
do_score([$] | _], _) -> {unexpected, 57};
do_score([$} | _], _) -> {unexpected, 1197};
do_score([$> | _], _) -> {unexpected, 25137}.

incomplete_points(Chars) ->
  incomplete_points(Chars, 0).

incomplete_points([], Acc) -> Acc;
incomplete_points([$) | T], Acc) -> incomplete_points(T, (Acc * 5) + 1);
incomplete_points([$] | T], Acc) -> incomplete_points(T, (Acc * 5) + 2);
incomplete_points([$} | T], Acc) -> incomplete_points(T, (Acc * 5) + 3);
incomplete_points([$> | T], Acc) -> incomplete_points(T, (Acc * 5) + 4).
