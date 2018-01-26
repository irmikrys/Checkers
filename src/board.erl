%%%-------------------------------------------------------------------
%%% @author Sebastian Skoczeń & Irmina Krysiak
%%% Created : 27. Dec 2017 12:13
%%%-------------------------------------------------------------------
-module(board).
-export([showBoard/1, emptyBoard/0, getFieldColor/1]).

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

%%------------------------- showing board ----------------------------

showBoard(Board) ->
  "~n" ++
    upDownLabel() ++
    getAllStringRows(Board) ++
    upDownLabel() ++
    "~n".

getAllStringRows(Board) ->
  lists:concat([packRow(Board, Row) ++ "\n" || Row <- lists:seq(1,8)]).

getStringRow(Board, Row) ->
  lists:concat([showField(getFieldType(Board, {Row, Col})) ++ " " || Col <- lists:seq(1, 8)]).

packRow(Board, Row) ->
  leftSideLabelPack(lists:nth(Row, ?NUMS)) ++
    getStringRow(Board, Row) ++
    rightSideLabelPack(lists:nth(Row, ?NUMS)).

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

getFieldType(Board, {X, Y}) ->
  FindResult = maps:is_key({X, Y}, Board),
  Type = if
           FindResult == true -> maps:get({X, Y}, Board);
           true -> {getFieldColor({X, Y}), field}
         end,
  Type.

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