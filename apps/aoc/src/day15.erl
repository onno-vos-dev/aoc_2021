-module(day15).

-export([solve/0]).

%% API ========================================================================
solve() ->
  Grid = to_grid(#{}, input()),
  {687, 2957} = {calculate_lowest_risk(Grid), calculate_lowest_risk(expand_grid(Grid))}.

expand_grid(Grid) ->
  XYs = [ {X, Y} || X <- lists:seq(0, 4),
                    Y <- lists:seq(0, 4)
        ],
  do_expand_grid(Grid, XYs, lists:max(maps:keys(Grid)), #{}).

do_expand_grid(_, [], _, Acc) -> Acc;
do_expand_grid(Grid, [ {OffsetX, OffsetY} | T ], {MaxX, MaxY}, Acc) ->
  NewAcc = maps:fold(fun({X, Y}, V, A) ->
                       maps:put({X + OffsetX * MaxX, Y + OffsetY * MaxY},
                                plus_one_with_wrap(V, {OffsetX, OffsetY}),
                                A)
                     end, Acc, Grid),
  do_expand_grid(Grid, T, {MaxX, MaxY}, NewAcc).

plus_one_with_wrap(V, {OffsetX, OffsetY}) ->
  case V + OffsetX + OffsetY of
    Val when Val > 9 -> Val rem 9;
    Val -> Val
  end.

%% Logic ======================================================================
calculate_lowest_risk(Grid) ->
  Keys = maps:keys(Grid),
  {MinNode, MaxNode} = {lists:min(Keys), lists:max(Keys)},
  Checked = gb_sets:add_element({0, MinNode}, gb_sets:new()),
  do_calculate_lowest_risk(Grid, Checked, #{}, MaxNode, #{}).

do_calculate_lowest_risk(_Grid, {0, nil}, _Seen, MaxNode, Costs) ->
  maps:get(MaxNode, Costs);
do_calculate_lowest_risk(Grid, Checked, Seen, MaxNode, Costs) ->
  {{Cost, {X, Y}}, NewSet0} = gb_sets:take_smallest(Checked),
  case maps:is_key({X, Y}, Seen) of
    true ->
      do_calculate_lowest_risk(Grid, NewSet0, Seen, MaxNode, Costs);
    false ->
      NewSet = build_new(NewSet0, X, Y, Seen, Cost, Grid),
      NewSeen = maps:put({X, Y}, true, Seen),
      NewCosts = maps:put({X, Y}, Cost, Costs),
      do_calculate_lowest_risk(Grid, NewSet, NewSeen, MaxNode, NewCosts)
  end.

build_new(Set, X, Y, Seen, Cost, Grid) ->
  lists:foldl(fun({C, Coord}, Acc) ->
                 case maps:is_key(Coord, Seen) of
                   true -> Acc;
                   false -> gb_sets:add_element({Cost + C, Coord}, Acc)
                 end
              end,
              Set,
              surrounding(X, Y, Grid)).

surrounding(X, Y, Grid) ->
  lists:foldl(fun(Pos, Acc) ->
                case maps:get(Pos, Grid, undefined) of
                  undefined -> Acc;
                  Val -> [ {Val, Pos} | Acc ]
                end
              end, [], [{X + 1, Y}, {X, Y + 1}, {X - 1, Y}, {X, Y - 1}]).

%% Parsing ====================================================================
input() ->
  util:read_file("day15.txt", <<"\n">>, fun binary_to_list/1).

to_grid(Acc, Lines) ->
  {_, NewAcc} = lists:foldl(fun(Line, {Y, A}) ->
                              to_grid(A, Line, Y)
                            end, {1, Acc}, Lines),
  NewAcc.

to_grid(Acc, Line, Y) ->
  {_, NewAcc} = lists:foldl(fun(V, {X, A}) ->
                              {X + 1, maps:put({X, Y}, V - $0, A)}
                            end, {1, Acc}, Line),
  {Y + 1, NewAcc}.
