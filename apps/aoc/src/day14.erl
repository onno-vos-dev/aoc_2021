-module(day14).

-export([solve/0]).

%% API ========================================================================
solve() ->
  {Polymer, Ops} = input(),
  Part1 = quantity(step(Polymer, Ops, 10)),
  Part2 = quantity(step(Polymer, Ops, 40)),
  {3230, 3542388214529} = {Part1 - 1, Part2 - 1}.

%% Logic ======================================================================
step(Polymer, Ops, Steps) ->
  do_step_v2(pairs(Polymer, #{}), Ops, Steps, 0).

pairs([_], Acc) -> Acc;
pairs([A, B | T], Acc) ->
  pairs([B | T], maps:update_with({A, B}, fun(V) -> V + 1 end, 1, Acc)).

do_step_v2(Pairs, _Ops, Steps, CurrentStep) when Steps =:= CurrentStep -> Pairs;
do_step_v2(Pairs, Ops, Steps, CurrentStep) ->
  NewPairs =
    maps:fold(fun({A, B}, Count, Acc) ->
                case maps:get({A, B}, Ops) of
                  Char ->
                    maps:update_with({Char, B},
                                     fun(V) -> V + Count end,
                                     Count,
                                     maps:update_with({A, Char},
                                                      fun(V) -> V + Count end,
                                                      Count,
                                                      Acc))
                end
              end, #{}, Pairs),
  do_step_v2(NewPairs, Ops, Steps, CurrentStep + 1).

quantity(Pairs) ->
  L = lists:flatmap(fun({{A, _}, Count}) -> [{A, Count}] end, maps:to_list(Pairs)),
  L2 = lists:foldl(fun({A, Count}, Acc) ->
                     maps:update_with(A, fun(V) -> V + Count end, Count, Acc)
                   end, #{}, L),
  [{_, Least} | _] = L3 = lists:keysort(2, maps:to_list(L2)),
  {_, Most} = hd(lists:reverse(L3)),
  Most - Least.

%% Parse ======================================================================
input() ->
  FoldFun = fun(<<>>, Acc) -> Acc;
               (Bin, {Polymer, Ops}) ->
                 case string:tokens(binary_to_list(Bin), [$\s]) of
                   [P] -> {P, Ops};
                   [[A, B], _, [C]] -> {Polymer, Ops#{{A, B} => C}}
                 end
            end,
  Acc = {undefined, #{}},
  util:read_file_fold("day14.txt", <<"\n">>, FoldFun, Acc).
