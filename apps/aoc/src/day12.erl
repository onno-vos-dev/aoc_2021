-module(day12).

-export([solve/0]).

%% API ========================================================================
solve() ->
  Input = parse(input(), #{}),
  Part1 = part1(Input),
  Part2 = part2(Input),
  {4413, 118803} = {Part1, Part2}.

%% Part1 ======================================================================
part1(Input) ->
  AllowTwoVisits = false,
  routes(Input, AllowTwoVisits).

%% Part2 ======================================================================
part2(Input) ->
  AllowTwoVisits = true,
  routes(Input, AllowTwoVisits).

%% Logic ======================================================================
routes(Input, AllowTwoVisits) ->
  Routes = calculate_routes([{[{small, <<"start">>}], AllowTwoVisits}], Input, [], AllowTwoVisits),
  length(Routes).

calculate_routes([], _Input, Acc, _AllowTwoVisits) -> Acc;
calculate_routes([{Q, VisitedTwice} | T], Input, Acc, AllowTwoVisits) ->
  case with_next(hd(Q), Input) of
    [{small, <<"end">>}] ->
      calculate_routes(T, Input, [Q | Acc], AllowTwoVisits);
    Vals ->
      Twice = case VisitedTwice andalso AllowTwoVisits of
                true ->
                  SmallCaves = lists:filter(fun(Item) -> element(1, Item) =:= small end, Q),
                  length(lists:usort(SmallCaves)) =:= length(SmallCaves);
                false ->
                  VisitedTwice
              end,
      NewQueues = [ {[V | Q], Twice} || V <- Vals,
                    condition(Twice, V, Q) =:= false, V =/= {small, <<"start">>}
                  ],
      calculate_routes(NewQueues ++ T, Input, Acc, AllowTwoVisits)
  end.

condition(false, Val, Acc) ->
  lists:member(Val, Acc) andalso element(1, Val) =:= small andalso hd(Acc) =/= Val;
condition(true, Val, Acc) ->
  element(1, Val) =:= <<"start">> andalso hd(Acc) =/= Val.

with_next({small, <<"end">>}, _Input) -> [{small, <<"end">>}];
with_next(Val, Input) ->
  case maps:get(Val, Input, undefined) of
    undefined ->
      [];
    Vals ->
      Vals
  end.

%% Parsing ====================================================================
parse([], Acc) -> maps:map(fun(_, V) -> V -- [<<"start">>] end, Acc);
parse([[Start0, End0] | T], Acc) ->
  Start = to_node(Start0),
  End = to_node(End0),
  parse(T, maps:update_with(End,
                            fun(V) -> [Start | V] end,
                            [Start],
                            maps:update_with(Start, fun(V) -> [End | V] end, [End], Acc))).

to_node(Str) ->
  case string:uppercase(Str) =:= Str of
    true -> {big, Str};
    false -> {small, Str}
  end.

input() ->
  util:read_file("day12.txt",
                 <<"\n">>,
                 fun(A) -> [list_to_binary(V) || V <- string:tokens(binary_to_list(A), "-")] end).
