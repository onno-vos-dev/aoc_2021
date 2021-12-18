-module(day16).

-export([ solve/0 ]).

%% API ========================================================================
solve() ->
  Input = parse(input()),
  {953, 246225449979} = {sum_version_numbers(Input), evaluate(Input)}.

%% Logic ======================================================================
sum_version_numbers({literal, Version, _}) ->
  Version;
sum_version_numbers({packet, Version, _Type, Packets}) ->
  Version + lists:sum(lists:map(fun sum_version_numbers/1, Packets)).

evaluate({literal, _Version, L}) ->
  L;
evaluate({packet, _, 0, Packets}) ->
  lists:sum(lists:map(fun evaluate/1, Packets));
evaluate({packet, _, 1, Packets}) ->
  lists:foldl(fun(V, A) -> V * A end, 1, lists:map(fun evaluate/1, Packets));
evaluate({packet, _, 2, Packets}) ->
  lists:min(lists:map(fun evaluate/1, Packets));
evaluate({packet, _, 3, Packets}) ->
  lists:max(lists:map(fun evaluate/1, Packets));
evaluate({packet, _, 5, Packets}) ->
  [First, Second] = lists:map(fun evaluate/1, Packets),
  case First > Second of
    true -> 1;
    false -> 0
  end;
evaluate({packet, _, 6, Packets}) ->
  [First, Second] = lists:map(fun evaluate/1, Packets),
  case First < Second of
    true -> 1;
    false -> 0
  end;
evaluate({packet, _, 7, Packets}) ->
  [First, Second] = lists:map(fun evaluate/1, Packets),
  case First =:= Second of
    true -> 1;
    false -> 0
  end.

%% Parsing ====================================================================
input() ->
  hd(util:read_file("day16.txt", <<"\n">>)).

parse(Bin) ->
  {Parsed, _} = do_parse(<< <<(binary_to_integer(B, 16))>> || <<B:2/binary>> <= Bin >>),
  Parsed.

do_parse(<<V:3, 4:3, Rest/bitstring>>) ->
    {L, R} = parse_literal(Rest),
    {{literal, V, L}, R};
do_parse(<<V:3, T:3, 0:1, Size:15, Rest/bitstring>>) ->
    <<SubPackets:Size/bitstring, R/bitstring>> = Rest,
    Packets = parse_subpackets(SubPackets),
    {{packet, V, T, Packets}, R};
do_parse(<<V:3, T:3, 1:1, Count:11, Rest/bitstring>>) ->
    {Packets, R} = parse_packets(Count, Rest),
    {{packet, V, T, Packets}, R};
do_parse(_) ->
  <<>>.

parse_literal(<<0:1, N:4, Rest/bitstring>>) ->
  {N, Rest};
parse_literal(Extended) ->
  do_parse_literal(Extended, 0).

do_parse_literal(<<1:1, N:4, Rest/bitstring>>, Acc) ->
  do_parse_literal(Rest, (Acc bsl 4) + N);
do_parse_literal(<<0:1, N:4, Rest/bitstring>>, Acc) ->
  {(Acc bsl 4) + N, Rest}.

parse_subpackets(Data) ->
  do_parse_subpackets(Data, []).

do_parse_subpackets(Data, Acc) ->
  case do_parse(Data) of
    <<>> -> lists:reverse(Acc);
    {Packet, Rest} ->
      do_parse_subpackets(Rest, [Packet | Acc])
  end.

parse_packets(N, Data) ->
  do_parse_packets(0, N, Data, []).

do_parse_packets(N, N, Data, Acc) -> {lists:reverse(Acc), Data};
do_parse_packets(I, N, Data, Acc) ->
  {Packet, Rest} = do_parse(Data),
  do_parse_packets(I + 1, N, Rest, [Packet | Acc]).
