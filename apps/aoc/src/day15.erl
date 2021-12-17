-module(day15).

-export([solve/0, solve_nif/0]).

-dialyzer({nowarn_function, [ solve_nif/0
                            ]}).

%% Taken from:
%% https://github.com/jesperes/aoc_erlang/blob/b53a0d2475920ef7beb330536e468eac6cfd659f/src/2021/aoc2021_day15.erl#L32
%% Encoding the {X, Y} in the Seen map shaves off a good 800 ms from the total runtime.
-define(BIT_XY(X, Y), X bsl 12 bor Y).

%% API ========================================================================
solve() ->
  Grid = to_grid(#{}, input()),
  {687, 2957} = {calculate_lowest_risk(Grid), calculate_lowest_risk(expand_grid(Grid))}.

solve_nif() ->
  Grid = to_grid(#{}, input()),
  {687, 2957} = {util:dijkstra(Grid), util:dijkstra(expand_grid(Grid))}.

%% Logic ======================================================================
calculate_lowest_risk(Grid) ->
  Keys = maps:keys(Grid),
  MinNode = lists:min(Keys),
  Checked = gb_sets:add_element({0, MinNode}, gb_sets:new()),
  do_calculate_lowest_risk(Grid, Checked, #{}, infinity).

do_calculate_lowest_risk(_Grid, {0, nil}, _Seen, Score) ->
  Score;
do_calculate_lowest_risk(Grid, Checked, Seen, Score) ->
  {{Cost, {X, Y}}, NewSet0} = gb_sets:take_smallest(Checked),
  case maps:is_key(?BIT_XY(X, Y), Seen) of
    true ->
      do_calculate_lowest_risk(Grid, NewSet0, Seen, Score);
    false ->
      NewSet = build_new(NewSet0, X, Y, Seen, Cost, Grid),
      NewSeen = maps:put(?BIT_XY(X, Y), true, Seen),
      do_calculate_lowest_risk(Grid, NewSet, NewSeen, Cost)
  end.

build_new(Set, X, Y, Seen, Cost, Grid) ->
  lists:foldl(fun({C, {Xc, Yc} = Coord}, Acc) ->
                 case maps:is_key(?BIT_XY(Xc, Yc), Seen) of
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
