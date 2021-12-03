-module(util).

-export([ read_file/3
        , binary_to_decimal/1
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

binary_to_decimal(<<"">>) ->
  0;
binary_to_decimal(Binary) when is_binary(Binary) ->
  binary_to_decimal(binary_to_list(Binary), 0).

binary_to_decimal([], Sum) ->
  Sum;
binary_to_decimal([48 | Tail], Sum) -> %48 is ascii code for 0
  binary_to_decimal(Tail, Sum * 2);
binary_to_decimal([49 | Tail], Sum) -> %49 is ascii code for 1
  binary_to_decimal(Tail, Sum * 2 + 1).

%%%_* Emacs ====================================================================
%%% Local Variables:
%%% allout-layout: t
%%% erlang-indent-level: 2
%%% End:
