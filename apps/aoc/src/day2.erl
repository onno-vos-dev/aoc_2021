-module(day2).

-export([ solve/0 ]).

solve() ->
  Input = util:read_file("day2.txt",
                         <<"\n">>,
                         fun(A) ->
                           [Dir, Num] = binary:split(A, <<" ">>), {Dir, binary_to_integer(Num)}
                         end),
  {Horizontal, Depth, {DepthWithAim, _Aim}} = plan_course(Input, {0, 0, {0, 0}}),
  {Horizontal * Depth, Horizontal * DepthWithAim}.

plan_course([], Acc) ->
  Acc;
plan_course([{<<"forward">>, Num} | T], {Horizontal, Depth, {DepthWithAim, Aim}}) ->
  plan_course(T, {Horizontal + Num, Depth, {DepthWithAim + Aim * Num, Aim}});
plan_course([{<<"down">>, Num}  | T], {Horizontal, Depth, {DepthWithAim, Aim}}) ->
  plan_course(T, {Horizontal, Depth + Num, {DepthWithAim, Aim + Num}});
plan_course([{<<"up">>, Num}  | T], {Horizontal, Depth, {DepthWithAim, Aim}}) ->
  plan_course(T, {Horizontal, Depth - Num, {DepthWithAim, Aim - Num}}).
