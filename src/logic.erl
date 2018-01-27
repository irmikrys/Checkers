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

getFieldType(Board, Position) ->
  maps:get(Position, Board, {getFieldColor(Position), field}).

getFieldColor({X, Y}) when (X + Y) rem 2 == 1 -> black;
getFieldColor({X, Y}) when (X + Y) rem 2 == 0 -> white;
getFieldColor(_) -> throw(exception_get_field_color).

addToBoard(Board, Pos, Draught) ->
  maps:put(Pos, Draught, Board).

deleteFromBoard(Board, Pos) ->
  maps:remove(Pos, Board).

getDraught(Board, Position) ->
  maps:get(Position, Board).

checkIfOccupied(Board, Position) ->
  maps:is_key(Position, Board).


%-- returns board, position From is deleted
%-- and its draught with To position added
%-- kills enemy if necessary
makeMove(Board, From, To) ->
  IsToOccupied = checkIfOccupied(Board, To),
  if
    IsToOccupied == false ->
      {Figure, Color} = getDraught(Board, From),
      BoardWithDeleted = deleteFromBoard(Board,From),
      BoardWithAdded = addToBoard(BoardWithDeleted, To, {Figure, Color}),
      BoardEnemyKilled = killEnemyIfNecessary(BoardWithAdded, From, To, Color);
    true -> throw(cannot_make_move_occupied)
  end.

%-- returns same board if no enemy between positions
%-- returns board without mid draught if enemy
killEnemyIfNecessary(Board, {Xfrom, Yfrom}, {Xto, Yto}, CurrentColor) ->
  EnemyPosition = {(Xfrom + Xto)/2, (Yfrom + Yto)/2},
  IsEnemy = checkIfEnemy(Board, EnemyPosition, CurrentColor),
  if
    IsEnemy == true -> deleteFromBoard(Board, EnemyPosition);
    true -> Board
  end.

checkIfEnemy(Board, EnemyPosition, CurrentColor) ->
  {Figure, Color} = getDraught(Board, EnemyPosition),
  IsDisc = Figure == disc,
  HasOppositeColor = Color /= CurrentColor,
  IsDisc and HasOppositeColor.
