-module(aoc_helpers).

-export([ count_increases/1
        , count_decreases/1
        , sum_sliding_threes/1
        ]).

%% API ========================================================================
%% @doc
%% Given a list a of integers in random order
%% count the amount of times a subsequent integer is an increase of the previous.
%% @end
-spec count_increases([integer()]) -> integer().
count_increases(Input) ->
  count_change(tl(Input), fun(A, B) -> A > B end, {hd(Input), 0}).

%% @doc
%% Given a list a of integers in random order
%% count the amount of times a subsequent integer is an increase of the previous.
%% @end
-spec count_decreases([integer()]) -> integer().
count_decreases(Input) ->
  count_change(tl(Input), fun(A, B) -> A < B end, {hd(Input), 0}).

count_change([], _F, {_Last, Acc}) -> Acc;
count_change([H | T], F, {Last, Acc}) ->
  case F(H, Last) of
    true ->
      count_change(T, F, {H, Acc + 1});
    false ->
      count_change(T, F, {H, Acc})
  end.

%% @doc
%% Given a list of integers in random order
%% sum in sets of threes.
%% @end
-spec sum_sliding_threes([integer()]) -> [integer()].
sum_sliding_threes(Input) ->
  sum_sliding_threes(Input, []).

sum_sliding_threes([_, _], Acc) -> lists:reverse(Acc);
sum_sliding_threes([H1, H2, H3 | T], Acc) ->
  sum_sliding_threes([H2, H3 | T], [H1 + H2 + H3 | Acc]).
