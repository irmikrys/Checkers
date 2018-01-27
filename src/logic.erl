%%%-------------------------------------------------------------------
%%% @author irmi
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. sty 2018 11:41
%%%-------------------------------------------------------------------

-module(logic).
-author("irmi").

%% API
-export([addToBoard/3, makeMove/3, getFieldType/2]).

%-------------------------- game logic -------------------------------

addToBoard(Board, Pos, Draught) ->
  maps:put(Pos, Draught, Board).

deleteFromBoard(Board, Pos) ->
  maps:remove(Pos, Board).

%-- returns board, position From is deleted
%-- and its draught with To position added, To must be black
%-- kills enemy if necessary
makeMove(Board, From, To) ->
  IsToOccupied = checkIfOccupied(Board, To),
  IsBlack = checkIfMoveFieldBlack(To),
  if
    (IsToOccupied == false) and (IsBlack == true) ->
      {Figure, Color} = getDraught(Board, From),
      BoardWithDeleted = deleteFromBoard(Board, From),
      BoardWithAdded = addToBoard(BoardWithDeleted, To, {Figure, Color}),
      BoardWithAdded; % zwraca board z przesunietym pionkiem
      %BoardEnemyKilled = killEnemyIfNecessary(BoardWithAdded, From, To, Color),
      %BoardEnemyKilled;
    true -> throw(cannot_make_move_occupied)
  end.

%-- returns same board if no enemy between positions
%-- returns board without mid draught if enemy
killEnemyIfNecessary(Board, {Xfrom, Yfrom}, {Xto, Yto}, CurrentColor) ->
  EnemyPosition = {(Xfrom + Xto) / 2, (Yfrom + Yto) / 2},
  IsEnemy = checkIfEnemy(Board, EnemyPosition, CurrentColor),
  if
    IsEnemy == true -> deleteFromBoard(Board, EnemyPosition);
    true -> Board
  end.

turnToKing(Board, Position, Color) ->
  maps:update(Position, {Color, king}, Board).

%------------------------------ checkers -----------------------------

checkIfOccupied(Board, Position) ->
  maps:is_key(Position, Board).

checkIfEnemy(Board, EnemyPosition, CurrentColor) ->
  {Figure, Color} = getDraught(Board, EnemyPosition),
  IsDisc = Figure == disc,
  HasOppositeColor = Color /= CurrentColor,
  IsDisc and HasOppositeColor.

checkIfMoveFieldBlack(Position) ->
  Field = getFieldColor(Position),
  Field == black.

checkIfTurnsToKing({X, _Y}, Color) ->
  ((X == 1) and (Color == black)) or
    ((X == 8) and (Color == white)).

%------------------------------ getters ------------------------------

getFieldType(Board, Position) ->
  maps:get(Position, Board, {getFieldColor(Position), field}).

getFieldColor({X, Y}) when (X + Y) rem 2 == 1 -> black;
getFieldColor({X, Y}) when (X + Y) rem 2 == 0 -> white;
getFieldColor(_) -> throw(exception_get_field_color).

getDraught(Board, Position) ->
  maps:get(Position, Board).
