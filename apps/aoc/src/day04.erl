-module(day04).

-export([ solve/0 ]).

%% Records ====================================================================
-record(bingo_number, { key :: {pos_integer(), pos_integer(), pos_integer(), pos_integer()} | '_'
                       , x :: pos_integer() | '_'
                       , y :: pos_integer() | '_'
                       , number :: pos_integer() | '_'
                       , card_number :: pos_integer() | '_'
                       }).

-record(bingo_row, { key :: {pos_integer(), pos_integer()} | {'_', '_'} | '_'
                   , numbers :: [pos_integer()] | '_'
                   }).

-record(bingo_column, { key :: {pos_integer(), pos_integer()} | {'_', '_'} | '_'
                      , numbers :: [pos_integer()] | '_'
                      }).

%% API ========================================================================
solve() ->
  Input =
    util:read_file("day4.txt",
                   <<"\n">>),
  Tid = ets:new(?MODULE, [set, private]),
  put(tid, Tid),
  Numbers = parse_input(Input),
  ok = run_game(Numbers),
  Part1 = get(part1),
  Part2 = get(part2),
  erase(),
  {63424, 23541} = {Part1, Part2}.

%% Parsing ====================================================================
parse_input(Input) ->
  Numbers = lists:map(fun binary_to_integer/1, binary:split(hd(Input), <<",">>, [trim, global])),
  BingoCards0 = tl(Input),
  BingoCards1 = lists:map(fun(A) ->
                            lists:filtermap(fun(<<",">>) -> false;
                                               (<<>>) -> false;
                                               (<<" ">>) -> false;
                                               (B) -> {true, erlang:binary_to_integer(B)}
                                            end,
                                            re:split(A, <<" ">>))
                          end, BingoCards0),
  build_bingocards(tl(BingoCards1)), %% Pop off first empty row
  Numbers.

build_bingocards(BingoCards) ->
  build_numbers(BingoCards, {1, 1}),
  build_bingo_rows(),
  build_bingo_columns().

build_numbers([], {_, _}) -> ok;
build_numbers([[] | T], {_, CardNumber}) ->
  build_numbers(T, {1, CardNumber + 1});
build_numbers([Numbers | T], {Y, CardNumber}) ->
  Len = length(Numbers),
  Coordinates = lists:zip(lists:zip(lists:duplicate(Len, Y), lists:seq(1, Len)), Numbers),
  lists:foreach(fun(Coordinate) -> add_to_ets(CardNumber, Coordinate) end, Coordinates),
  build_numbers(T, {Y + 1, CardNumber}).

add_to_ets(CardNumber, {{Y, X}, Number}) ->
  Key = {X, Y, CardNumber, Number},
  Row = #bingo_number{key = Key, x = X, y = Y, number = Number, card_number = CardNumber},
  ets:insert(get(tid), {Key, Row}).

build_bingo_rows() ->
  MS = bingo_numbers_ms(),
  Rows = lists:foldl(fun({{_X, Y, CardNumber, Number}, _}, Acc) ->
                        update_acc(Y, Number, CardNumber, Acc)
                      end, #{}, ets:select(get(tid), MS)),
  lists:foreach(fun({{RowNum, CardNum}, Numbers}) ->
                  Key = {row, RowNum, CardNum},
                  BingoRow = #bingo_row{key = {RowNum, CardNum}, numbers = Numbers},
                  ets:insert(get(tid), {Key, BingoRow})
                end, maps:to_list(Rows)).

build_bingo_columns() ->
  MS = bingo_numbers_ms(),
  Columns = lists:foldl(fun({{X, _Y, CardNumber, Number}, _}, Acc) ->
                          update_acc(X, Number, CardNumber, Acc)
                        end, #{}, ets:select(get(tid), MS)),
  lists:foreach(fun({{ColumnNum, CardNum}, Numbers}) ->
                  Key = {column, ColumnNum, CardNum},
                  BingoColumn = #bingo_column{key = {ColumnNum, CardNum}, numbers = Numbers},
                  ets:insert(get(tid), {Key, BingoColumn})
                end, maps:to_list(Columns)).

bingo_numbers_ms() ->
  [{{'$1', #bingo_number{key = '_', x = '_', y = '_', number = '_', card_number = '_'}},
   [],
   ['$_']}
  ].

update_acc(X, Number, CardNumber, Acc) ->
  maps:update_with({X, CardNumber},
                   fun(Numbers) -> [Number | Numbers] end,
                   [Number],
                   Acc
                   ).

%% Bingo logic ================================================================
run_game([]) -> ok;
run_game([Nr | T]) ->
  case get(part2) of
    undefined ->
      Numbers = find_numbers(Nr),
      update_columns(Numbers),
      update_rows(Numbers),
      case has_bingo() of
        false -> run_game(T);
        {true, {Rows, Columns}} ->
          RowCardNrs = [ CardNr || #bingo_row{key = {_, CardNr}} <- Rows],
          ColumnCardNrs = [ CardNr || #bingo_column{key = {_, CardNr}} <- Columns],
          [ maybe_store_part1(CardNr, Nr) || CardNr <- RowCardNrs ++ ColumnCardNrs],
          [ maybe_store_part2(CardNr, Nr) || CardNr <- RowCardNrs ++ ColumnCardNrs],
          [ remove_card_from_game(CardNr) || CardNr <- RowCardNrs ++ ColumnCardNrs],
          run_game(T)
      end;
    _ ->
      ok
  end.

find_numbers(Number) ->
  MS = [{{'_', #bingo_number{number = Number, _ = '_'}},
        [],
        [{element, 2, '$_'}]}
       ],
  ets:select(get(tid), MS).

update_columns([]) -> ok;
update_columns([#bingo_number{x = X, card_number = CardNum, number = Number} | T]) ->
  case find_column(X, CardNum) of
    [] ->
      update_columns(T);
    [#bingo_column{numbers = Numbers} = Column] ->
      Key = {column, X, CardNum},
      ets:insert(get(tid), {Key, Column#bingo_column{ numbers = Numbers -- [Number]}}),
      update_columns(T)
  end.

find_column(ColumnNum, CardNum) ->
  MS = [{{'_', #bingo_column{key = {ColumnNum, CardNum}, _ = '_'}},
        [],
        [{element, 2, '$_'}]}
       ],
  ets:select(get(tid), MS).

update_rows([]) -> ok;
update_rows([#bingo_number{y = Y, card_number = CardNum, number = Number} | T]) ->
  case find_row(Y, CardNum) of
    [] ->
      update_rows(T);
    [#bingo_row{numbers = Numbers} = Row] ->
      Key = {row, Y, CardNum},
      ets:insert(get(tid), {Key, Row#bingo_row{ numbers = Numbers -- [Number]}}),
      update_rows(T)
  end.

find_row(RowNum, CardNum) ->
  MS = [{{'_', #bingo_row{key = {RowNum, CardNum}, _ = '_'}},
        [],
        [{element, 2, '$_'}]}
       ],
  ets:select(get(tid), MS).

has_bingo() ->
  case {find_bingo_row(), find_bingo_column()} of
    {[], []} -> false;
    {Rows, Columns} -> {true, {Rows, Columns}}
  end.

find_bingo_row() ->
  MS = [{{'_', #bingo_row{key = '_', numbers = []}},
        [],
        [{element, 2, '$_'}]}
        ],
  ets:select(get(tid), MS).

find_bingo_column() ->
  MS = [{{'_', #bingo_column{key = '_', numbers = []}},
        [],
        [{element, 2, '$_'}]}
        ],
  ets:select(get(tid), MS).

maybe_store_part1(CardNumber, Nr) ->
  case get(part1) of
    undefined ->
      put(part1, calculate_final_score(CardNumber, Nr));
    _ ->
      ok
  end.

maybe_store_part2(CardNumber, Nr) ->
  case num_cards_remaining() of
    1 ->
      put(part2, calculate_final_score(CardNumber, Nr));
    _ ->
      ok
  end.

num_cards_remaining() ->
  MS = [{{{row, '_', '$1'}, '_'}, [], ['$1']}],
  length(lists:usort(ets:select(get(tid), MS))).

calculate_final_score(CardNumber, Nr) ->
  MS = [{{'_', #bingo_row{key = {'_', CardNumber}, numbers = '_'}},
          [],
          [{element, 2, '$_'}]},
        {{'_', #bingo_column{key = {'_', CardNumber}, numbers = '_'}},
          [],
          [{element, 2, '$_'}]}
        ],
  lists:sum(lists:usort(lists:flatmap(fun(#bingo_column{numbers = Numbers}) -> Numbers;
                                          (#bingo_row{numbers = Numbers}) -> Numbers
                                      end, ets:select(get(tid), MS)))) * Nr.

remove_card_from_game(CardNumber) ->
  MS =
    [{ {'_', #bingo_row{key = {'_', CardNumber}, numbers = '_'}},
       [],
       [true]
     },
     { {'_', #bingo_column{key = {'_', CardNumber}, numbers = '_'}},
       [],
       [true]
     },
     { {'_', #bingo_number{key = '_', x = '_', y = '_', number = '_', card_number = CardNumber}},
       [],
       [true]
     }
    ],
  ets:select_delete(get(tid), MS).
