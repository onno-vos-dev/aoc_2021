-module(day14).

-export([solve/0]).

%% API ========================================================================
solve() ->
  {Polymer, Ops} = input(),
  Part1 = quantity(step(Polymer, Ops, 10)),
  Part2 = quantity(step(Polymer, Ops, 40)),
  {3230, 3542388214529} = {Part1, Part2}.

%% Logic ======================================================================
step(Polymer, Ops, Steps) ->
  do_step(Polymer, Ops, Steps, 0).

do_step({Pairs, Chars}, _Ops, Steps, CurrentStep) when Steps =:= CurrentStep -> {Pairs, Chars};
do_step({Pairs, Chars}, Ops, Steps, CurrentStep) ->
  {NewPairs, CharAcc} =
    maps:fold(fun({A, B}, Count, {PairAcc, CharAcc}) ->
                Char = maps:get({A, B}, Ops),
                NewPairAcc = add_to_acc([{{Char, B}, Count}, {{A, Char}, Count}], PairAcc),
                NewCharAcc = add_to_acc([{Char, Count}], CharAcc),
                {NewPairAcc, NewCharAcc}
              end, {#{}, Chars}, Pairs),
  do_step({NewPairs, CharAcc}, Ops, Steps, CurrentStep + 1).

add_to_acc([], Acc) -> Acc;
add_to_acc([{H, C} | T], Acc) ->
  add_to_acc(T, maps:update_with(H, fun(V) -> V + C end, C, Acc)).

quantity({_PairsAcc, CharAcc}) ->
  [{_, Least} | T] = lists:keysort(2, maps:to_list(CharAcc)),
  {_, Most} = lists:last(T),
  Most - Least.

%% Parse ======================================================================
input() ->
  FoldFun = fun(<<>>, Acc) -> Acc;
               (Bin, {Polymer, Ops}) ->
                 case string:tokens(binary_to_list(Bin), [$\s]) of
                   [P] -> {parse_polymer(P, Polymer), Ops};
                   [[A, B], _, [C]] -> {Polymer, Ops#{{A, B} => C}}
                 end
            end,
  Acc = {{#{}, #{}}, #{}},
  util:read_file_fold("day14.txt", <<"\n">>, FoldFun, Acc).

parse_polymer([H], {PairAcc, CharAcc}) ->
  {PairAcc, add_to_acc([{H, 1}], CharAcc)};
parse_polymer([A, B | T], {PairAcc, CharAcc}) ->
  parse_polymer([B | T], {add_to_acc([{{A, B}, 1}], PairAcc), add_to_acc([{A, 1}], CharAcc)}).
