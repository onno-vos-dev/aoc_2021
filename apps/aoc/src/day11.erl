-module(day11).

-export([solve/0]).

%% API ========================================================================
solve() ->
  Acc = to_grid(#{}, input()),
  {Part1, NewAcc} = flashes_after_n_steps(Acc, 100),
  Part2 = steps_until_sync(NewAcc, 100),
  {1588, 517} = {Part1, Part2}.

%% Part1 ======================================================================
flashes_after_n_steps(Acc, N) ->
  flashes_after_n_steps(Acc, 0, N, 0).

flashes_after_n_steps(Acc, N, N, Flashes) -> {Flashes, Acc};
flashes_after_n_steps(Acc, N, Max, Flashes) ->
  NewAcc = do_step(Acc),
  flashes_after_n_steps(NewAcc, N + 1, Max, Flashes + num_flashed_octopus(NewAcc)).

%% Part2 ======================================================================
steps_until_sync(Acc, Steps) ->
  NewAcc = do_step(Acc),
  case all_in_sync(NewAcc) of
    true ->
      Steps + 1;
    false ->
      steps_until_sync(NewAcc, Steps + 1)
  end.

%% Logic ======================================================================
do_step(Acc0) ->
  Acc1 = update_energy_levels(Acc0),
  Acc2 = maybe_flash_neighbors(Acc1, find_flashing_octopus(Acc1)),
  reset_energy_levels(Acc2).

maybe_flash_neighbors(Acc, FlashedPositions) ->
  Flashed = maps:from_list([ {Pos, []} || Pos <- FlashedPositions ]),
  NewAcc = maybe_flash_neighbors(Acc, Flashed, FlashedPositions),
  NewAcc.

maybe_flash_neighbors(Acc, _Flashed, []) -> Acc;
maybe_flash_neighbors(Acc, Flashed, [{X, Y} | T]) ->
  Neighbors = [ {X - 1, Y}
              , {X + 1, Y}
              , {X - 1, Y + 1}
              , {X - 1, Y - 1}
              , {X + 1, Y - 1}
              , {X + 1, Y + 1}
              , {X, Y + 1}
              , {X, Y - 1}
              ],
  {NewFlashed, NewAcc} = lists:foldl(fun(Pos, {InnerAcc, OuterAcc}) ->
                                       case maps:is_key(Pos, InnerAcc) of
                                         true ->
                                           {InnerAcc, OuterAcc};
                                         false ->
                                           NewOuterAcc = increase_energy(OuterAcc, Pos),
                                           case has_flashed(NewOuterAcc, Pos) of
                                             true -> {maps:put(Pos, [], InnerAcc), NewOuterAcc};
                                             false -> {InnerAcc, NewOuterAcc}
                                           end
                                       end
                                     end, {#{}, Acc}, Neighbors),
  NewNeighbors = lists:filter(fun(Pos) ->
                                not maps:is_key(Pos, Flashed)
                              end, find_flashing_octopus(NewAcc)),
  maybe_flash_neighbors(NewAcc, maps:merge(Flashed, NewFlashed), NewNeighbors ++ T).

%% Parsing ====================================================================
input() ->
  util:read_file("day11.txt", <<"\n">>, fun binary_to_list/1).

to_grid(Acc, Lines) ->
  {_, NewAcc} = lists:foldl(fun(Line, {Y, A}) ->
                              to_grid(A, Line, Y)
                            end, {1, Acc}, Lines),
  NewAcc.

to_grid(Acc, Line, Y) ->
  {_, NewAcc} = lists:foldl(fun(V, {X, A}) ->
                              {X + 1, add_to_grid(A, {{X, Y}, V - $0})}
                            end, {1, Acc}, Line),
  {Y + 1, NewAcc}.

%% Data access ================================================================
add_to_grid(Acc, {Pos, Val}) ->
  maps:put(Pos, Val, Acc).

reset_energy_levels(Acc) ->
  maps:map(fun(_, V) when V > 9 -> 0; (_, V) -> V end, Acc).

update_energy_levels(Acc) ->
  maps:map(fun(_, V) -> V + 1 end, Acc).

find_flashing_octopus(Acc) ->
  maps:keys(maps:filter(fun(_, V) -> V > 9 end, Acc)).

all_in_sync(Acc) ->
  maps:size(Acc) =:= num_flashed_octopus(Acc).

num_flashed_octopus(Acc) ->
  maps:size(maps:filter(fun(_, V) -> V =:= 0 end, Acc)).

increase_energy(Acc, Pos) ->
  case maps:is_key(Pos, Acc) of
    true -> maps:update_with(Pos, fun(V) -> V + 1 end, Acc);
    false -> Acc
  end.

has_flashed(Acc, Pos) ->
  case maps:get(Pos, Acc, undefined) of
    Val when Val > 9 -> true;
    _ -> false
  end.
