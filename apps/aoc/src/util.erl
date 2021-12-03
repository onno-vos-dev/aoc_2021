-module(util).

-export([ read_file/2
        , read_file/3
        , time_avg/2
        , time_avg/4
        , binary_to_decimal/1
        , split_to_chunks/2
        ]).

%% API ========================================================================
-spec read_file(string(), binary()) -> [any()].
read_file(File, Split) ->
  {ok, Bin} = read(File),
  [ S || S <- binary:split(Bin, Split, [trim, global]) ].

-spec read_file(string(), binary(), fun((any()) -> any())) -> [any()].
read_file(File, Split, CastFun) ->
  {ok, Bin} = read(File),
  [ CastFun(S) || S <- binary:split(Bin, Split, [trim, global]) ].

  -spec time_avg(fun(() -> any()), non_neg_integer()) -> ok.
time_avg(Fun, N) when is_function(Fun, 0) andalso is_integer(N) ->
  do_time_avg(Fun, N).

-spec time_avg(fun(() -> any()), non_neg_integer(), [any()], fun((any()) -> ok)) -> ok.
time_avg(Fun, N, Configs, ConfigFun) ->
  lists:foreach(fun(Config) ->
                  io:format("Timing config: ~p~n", [Config]),
                  ConfigFun(Config),
                  do_time_avg(Fun, N)
                end, Configs).

-spec binary_to_decimal(binary()) -> integer().
binary_to_decimal(Binary) ->
  binary_to_integer(Binary, 2).

-spec split_to_chunks([any()], pos_integer()) -> [[any()], ...].
split_to_chunks(L, N) when is_integer(N), N > 0 ->
  split_to_chunks(N, 0, L, []).

%%%_* Internal ================================================================
-spec read(string()) -> {error, atom()} | {ok, binary()}.
read(File) ->
  file:read_file(code:priv_dir(aoc) ++ "/inputs/" ++ File).

-spec do_time_avg(fun(() -> any()), non_neg_integer()) -> ok.
do_time_avg(Fun, X) ->
  AvgTimeMicro = lists:sum(
                    lists:map(fun(_) ->
                                  {Avg, _} = timer:tc(fun() -> Fun() end),
                                  Avg
                              end, lists:seq(1, X))) / X,
  io:format("Time: ~p us ~.5f ms ~n~n", [AvgTimeMicro, AvgTimeMicro / 1000]).

-spec split_to_chunks(pos_integer(), non_neg_integer(), [any()], [any()]) -> [[any()], ...].
split_to_chunks(_, _, [], Acc) ->
  [Acc];
split_to_chunks(N, N, L, Acc) ->
  [Acc | split_to_chunks(N, 0, L, [])];
split_to_chunks(N, X, [H|T], Acc) ->
  split_to_chunks(N, X + 1, T, [H|Acc]).
