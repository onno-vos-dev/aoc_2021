-module(util).

-export([ read_file/3
        , binary_to_decimal/1
        , split_to_chunks/2
        , time_avg/2
        ]).

read_file(File, Split, CastFun) ->
  {ok, Bin} = file:read_file(code:priv_dir(aoc) ++ "/inputs/" ++ File),
  [ CastFun(S) || S <- binary:split(Bin, Split, [trim, global]) ].

time_avg(Fun, X) ->
  AvgTimeMicro = lists:sum(
                   lists:map(fun(_) ->
                                 {Avg, _} = timer:tc(fun() -> Fun() end),
                                 Avg
                             end, lists:seq(1, X))) / X,
  io:format("Time: ~p us ~.5f ms ~n~n", [AvgTimeMicro, AvgTimeMicro / 1000]).

binary_to_decimal(Binary) ->
  binary_to_integer(Binary, 2).

split_to_chunks(L, N) when N > 0 ->
  Len = length(L),
  case Len =< N of
    true -> [L];
    false ->
      split_to_chunks(L, Len, N, [])
  end.

split_to_chunks(L, Len, N, Acc) when Len =< N -> lists:reverse([L | Acc]);
split_to_chunks(L, _, N, Acc) ->
  {L1, L2} = lists:split(N, L),
  split_to_chunks(L2, length(L2), N, [L1 | Acc]).


%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
