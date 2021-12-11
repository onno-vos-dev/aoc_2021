-module(day11).

-export([solve/0]).

%% API ========================================================================
solve() ->
  Tid = ets:new(?MODULE, [set, private]),
  ok = to_grid(Tid, input()),
  Part1 = flashes_after_n_steps(Tid, 100),
  Part2 = steps_until_sync(Tid, 100),
  {1588, 517} = {Part1, Part2}.

%% Part1 ======================================================================
flashes_after_n_steps(Tid, N) ->
  flashes_after_n_steps(Tid, 0, N, 0).

flashes_after_n_steps(_Tid, N, N, Flashes) -> Flashes;
flashes_after_n_steps(Tid, N, Max, Flashes) ->
  Flashed = do_step(Tid),
  flashes_after_n_steps(Tid, N + 1, Max, Flashed + Flashes).

%% Part2 ======================================================================
steps_until_sync(Tid, Steps) ->
  do_step(Tid),
  case all_in_sync(Tid) of
    true ->
      Steps + 1;
    false ->
      steps_until_sync(Tid, Steps + 1)
  end.

%% Logic ======================================================================
do_step(Tid) ->
  update_energy_levels(Tid),
  Flashed = maybe_flash_neighbors(Tid, find_flashing_octopus(Tid)),
  reset_energy_levels(Tid),
  Flashed.

maybe_flash_neighbors(Tid, FlashedPositions) ->
  Flashed = maps:from_list([ {Pos, []} || Pos <- FlashedPositions ]),
  maps:size(maybe_flash_neighbors(Tid, Flashed, FlashedPositions)).

maybe_flash_neighbors(_Tid, Flashed, []) -> Flashed;
maybe_flash_neighbors(Tid, Flashed, [{X, Y} | T]) ->
  Neighbors = [ {X - 1, Y}
              , {X + 1, Y}
              , {X - 1, Y + 1}
              , {X - 1, Y - 1}
              , {X + 1, Y - 1}
              , {X + 1, Y + 1}
              , {X, Y + 1}
              , {X, Y - 1}
              ],
  NewFlashed = lists:foldl(fun(Pos, Acc) ->
                              case maps:is_key(Pos, Acc) of
                                true ->
                                  Acc;
                                false ->
                                increase_energy(Tid, Pos),
                                case has_flashed(Tid, Pos) of
                                  true -> maps:put(Pos, [], Acc);
                                  false -> Acc
                                end
                              end
                            end, #{}, Neighbors),
  NewNeighbors = lists:filter(fun(Pos) ->
                                not maps:is_key(Pos, Flashed)
                              end, find_flashing_octopus(Tid)),
  maybe_flash_neighbors(Tid, maps:merge(Flashed, NewFlashed), NewNeighbors ++ T).

%% Parsing ====================================================================
input() ->
  util:read_file("day11.txt", <<"\n">>, fun binary_to_list/1).

to_grid(Tid, Lines) ->
  lists:foldl(fun(Line, Y) ->
                  to_grid(Tid, Line, Y)
                end, 1, Lines),
  ok.

to_grid(Tid, Line, Y) ->
  lists:foldl(fun(V, X) ->
                add_to_grid(Tid, {{X, Y}, V - $0}),
                X + 1
              end, 1, Line),
  Y + 1.

%% Data access ================================================================
add_to_grid(Tid, Val) ->
  ets:insert(Tid, Val).

reset_energy_levels(Tid) ->
  ets:select_replace(Tid, [{{'$1', '$2'}, [{'>', '$2', 9}], [{{'$1', 0}}]}]).

update_energy_levels(Tid) ->
  MS = [{{'$1', '$2'}, [], [{{'$1', {'+', '$2', 1}}}]}],
  ets:select_replace(Tid, MS).

find_flashing_octopus(Tid) ->
  MS = [{{'$1', '$2'}, [{'>', '$2', 9}], ['$1']}],
  ets:select(Tid, MS).

all_in_sync(Tid) ->
  ets:info(Tid, size) =:= ets:select_count(Tid, [{{'$1', 0}, [], [true]}]).

increase_energy(Tid, Pos) ->
  MS = [{{'$1', '$2'}, [{'=:=', '$1', {Pos}}], [{{'$1', {'+', '$2', 1}}}]}],
  ets:select_replace(Tid, MS).

has_flashed(Tid, Pos) ->
  case ets:lookup(Tid, Pos) of
    [{Pos, Val}] when Val > 9 -> true;
    _ -> false
  end.
