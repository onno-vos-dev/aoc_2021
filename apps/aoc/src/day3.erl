-module(day3).

-export([ solve/0 ]).

solve() ->
  Input = util:read_file("day3.txt",
                         <<"\n">>,
                         fun(A) ->
                           lists:map(fun erlang:binary_to_integer/1, re:split(A, <<"">>, [trim]))
                         end
                        ),
  {3923414, 5852595} = {part1(Input), part2(Input)}.

%% Part1 ======================================================================
part1(Input) ->
  Commonalities = commonalities(Input),
  Gamma = util:binary_to_decimal(calc_rate(Commonalities, fun(X, Y) -> X > Y end)),
  Epsilon = util:binary_to_decimal(calc_rate(Commonalities, fun(X, Y) -> X < Y end)),
  Gamma * Epsilon.

calc_rate(Commonalities, Fun) ->
  maps:fold(fun(_, {X, Y}, Acc) ->
              case Fun(X, Y) of
                true -> <<Acc/binary, "0">>;
                false -> <<Acc/binary, "1">>
              end
            end, <<"">>, Commonalities).

%% Part2 ======================================================================
part2(Input) ->
  OxygenFilter = fun(One, Zero) -> One >= Zero end,
  ScrubberFilter = fun(One, Zero) -> One < Zero end,
  Oxygen = util:binary_to_decimal(filter(Input, OxygenFilter, {1, commonalities(Input)})),
  Scrubber = util:binary_to_decimal(filter(Input, ScrubberFilter, {1, commonalities(Input)})),
  Oxygen * Scrubber.

filter(Input, _, _) when length(Input) =:= 1 ->
  lists:foldl(fun(X, Acc) -> <<Acc/binary, (integer_to_binary(X))/binary>> end, <<"">>, hd(Input));
filter(Input, Filter, {Pos, Commonalities}) ->
  NewInput = lists:filter(fun(L) ->
                            {Zero, One} = maps:get(Pos, Commonalities),
                            case Filter(One, Zero) of
                              false ->
                                lists:nth(Pos, L) =:= 0;
                              true ->
                                lists:nth(Pos, L) =:= 1
                            end
                          end,
                          Input),
  filter(NewInput, Filter, {Pos + 1, commonalities(NewInput)}).

%% Common =====================================================================
commonalities(L) ->
  {_, Commonalities} = lists:foldl(fun(X, {_, Acc}) -> map_bits(X, Acc) end, {ignore, #{}}, L),
  Commonalities.

map_bits(List, Acc) ->
  lists:foldl(fun(Y, {Pos, A}) ->
    NewA = maps:update_with(Pos,
                            fun({Zero, One}) ->
                              case Y of
                                0 -> {Zero + 1, One};
                                1 -> {Zero, One + 1}
                              end
                            end,
                            case Y of
                              0 -> {1, 0};
                              1 -> {0, 1}
                            end,
                            A),
    {Pos + 1, NewA}
  end,
  {1, Acc},
  List).
