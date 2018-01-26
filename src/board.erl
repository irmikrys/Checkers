%%%-------------------------------------------------------------------
%%% @author Sebastian Skoczeń & Irmina Krysiak
%%% Created : 27. Dec 2017 12:13
%%%-------------------------------------------------------------------
-module(board).
-export([showBoard/1, emptyBoard/0, addToBoard/3, getPosFigure/2]).

-include("constants.hrl").

%% uruchamianie shella erlanga: erl +pc unicode
%% przykładowe wypisanie znaku: board:show({white, disc}).

%%----------------------------- board --------------------------------

%% Board to mapa, Key to {row, col}, Value to pionek

emptyBoard() ->
  maps:new().

getKeys(Board) ->
  maps:keys(Board).


%%maps:get(Key, Map, Default) - jakie value ma zwrocic jak nie znajdzie klucza


%%--------------------------------------------------------------------

addToBoard(Pos, Draught, Board) ->
  IsOccupied = isPosOccupied(Board, Pos),
  Add = if
          IsOccupied == false -> maps:put(Pos, Draught, Board);
          true -> throw(cannot_add_to_board)
        end,
  Add.

isPosOccupied(Board, Position) ->
  maps:is_key(Position, Board).

%%------------------------- showing board ----------------------------

showBoard(Board) ->
  "~n" ++
    upDownLabel() ++
    getAllStringRows(Board) ++
    upDownLabel() ++
    "~n".

getAllStringRows(Board) ->
  lists:concat([packRow(Board, Row) ++ "\n" || Row <- lists:seq(1, 8)]).

packRow(Board, Row) ->
  leftSideLabelPack(lists:nth(Row, ?NUMS)) ++
    getStringRow(Board, Row) ++
    rightSideLabelPack(lists:nth(Row, ?NUMS)).

getStringRow(Board, Row) ->
  lists:concat([showField(getFieldType(Board, {Row, Col})) ++ " " || Col <- lists:seq(1, 8)]).

showField(FieldType) ->
  Field = if
            FieldType == ?WHITE_DISC -> [9786];
            FieldType == ?BLACK_DISC -> [9787];
            FieldType == ?WHITE_KING -> [9812];
            FieldType == ?BLACK_KING -> [9818];
            FieldType == ?WHITE_FIELD -> [9633];
            FieldType == ?BLACK_FIELD -> [9632];
            true -> "*"
          end,
  Field.

getFieldType(Board, Position) ->
  maps:get(Position, Board, {getFieldColor(Position), field}).

getFieldColor({X, Y}) when (X + Y) rem 2 == 1 -> black;
getFieldColor({X, Y}) when (X + Y) rem 2 == 0 -> white;
getFieldColor(_) -> throw(exception_get_field_color).

leftSideLabelPack(Row)
  when Row > 0, Row < 9, is_integer(Row) ->
  integer_to_list(Row) ++ " ";
leftSideLabelPack(_) ->
  ".".

rightSideLabelPack(Row)
  when Row > 0, Row < 9, is_integer(Row) ->
  integer_to_list(Row);
rightSideLabelPack(_) ->
  ".".

upDownLabel() ->
  "  A B C D E F G H~n".
