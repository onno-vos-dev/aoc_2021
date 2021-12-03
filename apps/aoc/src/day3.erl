-module(day3).

-export([ solve/0 ]).

solve() ->
  Input = util:read_file("day3.txt", <<"\n">>, fun(A) -> A end),
  Size = byte_size(hd(Input)),
  Commonalities = commonalities(Input, Size),
  {3923414, 5852595} = {part1(Size, Commonalities), part2(Input, Size, Commonalities)}.

%% Part1 ======================================================================
part1(Size, Commonalities) ->
  Gamma = calc_rate(Size, Commonalities, fun(X, Y) -> X > Y end),
  Epsilon = calc_rate(Size, Commonalities, fun(X, Y) -> X < Y end),
  binary_to_integer(Gamma, 2) * binary_to_integer(Epsilon, 2).

calc_rate(Size, {ZeroC, OneC}, Fun) ->
  lists:foldl(fun({_Pos, {X, Y}}, Acc) ->
              case Fun(X, Y) of
                true -> <<Acc/binary, "0">>;
                false -> <<Acc/binary, "1">>
              end
            end, <<"">>, merge(Size, ZeroC, OneC)).

merge(Size, ZeroC, OneC) ->
  Zero = get_all(ZeroC, Size),
  One = get_all(OneC, Size),
  lists:reverse(lists:zipwith(fun({Pos, X}, {Pos, Y}) -> {Pos, {X, Y}} end, Zero, One)).

get_all(Counter, Size) ->
  lists:foldl(fun(I, Acc) -> [{I, counters:get(Counter, I)} | Acc] end, [], lists:seq(1, Size)).

%% Part2 ======================================================================
part2(Input, Size, Commonalities) ->
  OxygenFilter = fun(One, Zero) -> One >= Zero end,
  ScrubberFilter = fun(One, Zero) -> One < Zero end,
  Self = self(),
  _Pid1 = spawn_link(fun() ->
                       Self ! {oxygen, filter(Input, OxygenFilter, Size, {1, Commonalities})}
                     end),
  _Pid2 = spawn_link(fun() ->
                       Self ! {scrubber, filter(Input, ScrubberFilter, Size, {1, Commonalities})}
                     end),
  Oxygen = receive_(oxygen),
  Scrubber = receive_(scrubber),
  binary_to_integer(Oxygen, 2) * binary_to_integer(Scrubber, 2).

filter([Input], _, _, _) -> Input;
filter(Input, Filter, Size, {Pos, {ZeroC, OneC}}) ->
  NewInput = lists:filter(fun(B) ->
                            {Zero, One} = {counters:get(ZeroC, Pos), counters:get(OneC, Pos)},
                            case Filter(One, Zero) of
                              false ->
                                binary:at(B, Pos - 1) =:= $0;
                              true ->
                                binary:at(B, Pos - 1) =:= $1
                            end
                          end,
                          Input),
  filter(NewInput, Filter, Size, {Pos + 1, commonalities(NewInput, Size)}).

%% Common =====================================================================
commonalities(L, Size) ->
  ZeroAcc = counters:new(Size, []),
  OneAcc = counters:new(Size, []),
  Self = self(),
  Chunks = util:split_to_chunks(L, 100),
  Pids = lists:map(
           fun(Chunk) ->
             spawn(
               fun() ->
                 Res = lists:foreach(fun(X) -> map_bits(X, {1, {ZeroAcc, OneAcc}}) end, Chunk),
                 Self ! {{done, self()}, Res}
               end)
           end, Chunks),
  lists:foreach(fun(Pid) -> receive_({done, Pid}) end, Pids),
  {ZeroAcc, OneAcc}.

map_bits(<<>>, {_Pos, _}) -> ok;
map_bits(<<H, Rest/binary>>, {Pos, {ZeroAcc, OneAcc}}) ->
  case H of
    $0 ->
      counters:add(ZeroAcc, Pos, 1);
    $1 ->
      counters:add(OneAcc, Pos, 1)
  end,
  map_bits(Rest, {Pos + 1, {ZeroAcc, OneAcc}}).

receive_(Type) ->
  receive
    {Type, Value} -> Value
  end.
