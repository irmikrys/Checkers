%%%-------------------------------------------------------------------
%%% @author Sebastian Skoczeń & Irmina Krysiak
%%% Created : 27. Dec 2017 12:13
%%%-------------------------------------------------------------------
-module(board).
-export([showBoard/1]).
-import(logic, [addToBoard/3, makeMove/4, getFieldType/2]).

-include("constants.hrl").

%%------------------------- showing board ----------------------------

showBoard(Board) ->
  "~n" ++
    upDownLabel() ++
    getAllStringRows(Board) ++
    upDownLabel() ++
    "~n".

upDownLabel() ->
  "  A B C D E F G H~n".

getAllStringRows(Board) ->
  lists:concat([packRow(Board, Row) ++ "\n" || Row <- lists:seq(1, 8)]).

packRow(Board, Row) ->
  leftSideLabelPack(lists:nth(Row, ?NUMS)) ++
    getStringRow(Board, Row) ++
    rightSideLabelPack(lists:nth(Row, ?NUMS)).

leftSideLabelPack(Row) when Row > 0, Row < 9, is_integer(Row) ->
  integer_to_list(Row) ++ " ";
leftSideLabelPack(_) -> throw(exception_left_label).

rightSideLabelPack(Row) when Row > 0, Row < 9, is_integer(Row) ->
  integer_to_list(Row);
rightSideLabelPack(_) -> throw(exception_right_label).

getStringRow(Board, Row) ->
  lists:concat([showField(getFieldType(Board, {Row, Col})) ++ " " || Col <- lists:seq(1, 8)]).

showField(FieldType) ->
  Field = if
            FieldType == ?WHITE_DISC -> "w";%%[9786];
            FieldType == ?BLACK_DISC -> "b";%%[9787];
            FieldType == ?WHITE_KING -> "W";%%[9812];
            FieldType == ?BLACK_KING -> "B";%%[9818];
            FieldType == ?WHITE_FIELD -> " ";%%[9633];
            FieldType == ?BLACK_FIELD -> "+";%%[9632];
            true -> "*"
          end,
  Field.
