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

-include("constants.hrl").

%% API
-export([addToBoard/3, makeMove/3, getFieldType/2, getPossibleMoves/3, getPossibleMoves/2]).

%-------------------------- game logic -------------------------------

%-- returns board, position From is deleted
%-- and its draught with To position added, To must be black
%-- kills enemy if necessary
makeMove(Board, From, To) ->
  IsToAvailable = checkIfPosAvailable(Board, To),
  IsBlack = checkIfMoveFieldBlack(To),
  if
    (IsToAvailable == true) and (IsBlack == true) ->
      Direction = getDirection(From, To),
      {Color, Figure} = getDraught(Board, From),
      BoardWithDeleted = deleteFromBoard(Board, From),
      BoardWithAdded = addToBoard(BoardWithDeleted, To, {Color, Figure}),
      IsJumpOver = checkIfJump(Board, From, To, oppositeColor(Color)),
      erlang:display(IsJumpOver),
      BoardJumpOver = jumpIfOver(BoardWithAdded, From, To, Direction, IsJumpOver),
      turnToKing(BoardJumpOver, To, Color);
    true -> throw(cannot_make_move_occupied)
  end.

addToBoard(Board, Pos, Draught) ->
  maps:put(Pos, Draught, Board).

deleteFromBoard(Board, Pos) ->
  maps:remove(Pos, Board).

jumpIfOver(Board, From, To, Direction, IsOver) ->
  if
    IsOver == true ->
      EnemyPosition = getEnemyPosition(Board, From, To, Direction),
      deleteFromBoard(Board, EnemyPosition);
    true -> Board
  end.

turnToKing(Board, Position, Color) ->
  TurnToKing = checkIfTurnsToKing(Position, Color),
  if
    TurnToKing == true -> maps:update(Position, {Color, king}, Board);
    true -> Board
  end.

%------------------------------- moves -------------------------------

getPossibleMoves(Board, Color) ->
  Filtered = maps:filter(fun(_, V) -> {ColorPiece, _} = V, ColorPiece == Color end, Board),
  JumpMap = maps:fold(
    fun(From, Piece, Acc) ->
      {Moves, HasJump} = getPossibleMoves(Board, From, Piece),
      if HasJump == true ->
        maps:put(From, Moves, Acc);
        true -> Acc
      end
    end, maps:new(), Filtered),
  Size = maps:size(JumpMap),
  if Size == 0 -> maps:fold(
    fun(From, Piece, Acc) ->
      {Moves, _} = getPossibleMoves(Board, From, Piece),
      maps:put(From, Moves, Acc)
    end, maps:new(), Filtered);
    true -> JumpMap
  end.

%% discs can move one field forward (whites diagonally down,
%% blacks diagonally up), kings as many fields as possible
%% if there is a kill (jump) possible, then steps not generated
%-- returns possible moves for FigureType from From position
%% {Moves,HasJumps}
getPossibleMoves(Board, From, FigureType) ->
  Jumps = getJumps(Board, From, FigureType),
%%  erlang:display(Jumps),
  NoKills = (Jumps == []),
  if
    NoKills == false ->
      {Jumps, true};
    NoKills == true ->
      {getSteps(Board, From, FigureType), false}
  end.

getJumps(Board, {X, Y}, {Color, disc}) ->
  [{X2, Y2} ||
    X2 <- [X - 2, X + 2], Y2 <- [Y - 2, Y + 2],
    checkIfPosAvailable(Board, {X2, Y2}),
    checkIfJump(Board, {X, Y}, {X2, Y2}, oppositeColor(Color))];

getJumps(Board, Position, {Color, king}) ->
  lists:flatten([
    skipFieldsBeforeJump(Board, Position, Dir, Color) || Dir <- [?NE, ?NW, ?SE, ?SW]
  ]).

%-- skip all fields before potential enemy to kill
%-- if the Position not available then (maybe) get a king jump
skipFieldsBeforeJump(Board, Position, Direction, KingColor) ->
  Available = checkInDirection(Board, Position, Direction),
  MaybeAvailablePos = getPosInDirection(Position, Direction),
  if
    Available == true ->
      skipFieldsBeforeJump(Board, MaybeAvailablePos, Direction, KingColor);
    true ->
      getKingJump(Board, MaybeAvailablePos, Direction, KingColor)
  end.

%-- check if anything stands in the Position,
%-- if there is an enemy then check if field behind available
%-- if available then add to jumps
%-- if more behind available then add also (like steps)
getKingJump(Board, EnemyPosition, Direction, KingColor) ->
  Occupied = checkIfOccupied(Board, EnemyPosition),
  if
    Occupied == true ->
      {DraughtColor, _} = getDraught(Board, EnemyPosition),
%%      erlang:display(DraughtColor),
      IsEnemy = checkIfEnemy(KingColor, DraughtColor),
%%      erlang:display(IsEnemy),
      if
        IsEnemy == true ->
          getKingSteps(Board, EnemyPosition, Direction);
        true ->
          []
      end;
    true -> []
  end.

getSteps(Board, {X, Y}, {white, disc}) ->
  [{X1, Y1} ||
    X1 <- [X + 1], Y1 <- [Y - 1, Y + 1],
    checkIfPosAvailable(Board, {X1, Y1})];

getSteps(Board, {X, Y}, {black, disc}) ->
  [{X1, Y1} ||
    X1 <- [X - 1], Y1 <- [Y - 1, Y + 1],
    checkIfPosAvailable(Board, {X1, Y1})];

%-- get all steps until new position not available
getSteps(Board, Position, {_, king}) ->
  lists:flatten([
    getKingSteps(Board, Position, Dir) || Dir <- [?NE, ?NW, ?SE, ?SW]
  ]).

getKingSteps(Board, Position, Direction) ->
  Available = checkInDirection(Board, Position, Direction),
  if
    Available == true ->
      AvailablePosition = getPosInDirection(Position, Direction),
      [AvailablePosition] ++ getKingSteps(Board, AvailablePosition, Direction);
    true -> []
  end.

%------------------------------ checkers -----------------------------

%-- checks if position in specified direction is available
checkInDirection(Board, Position, Direction) ->
  PosInDirection = getPosInDirection(Position, Direction),
  checkIfPosAvailable(Board, PosInDirection).

checkIfEnemy(CurrentColor, EnemyColor) ->
  Color = oppositeColor(CurrentColor),
  (Color == EnemyColor).

checkIfPosAvailable(Board, Position = {X, Y}) ->
  Occupied = checkIfOccupied(Board, Position),
  if
    Occupied == false ->
      (X >= 1) and (X =< 8) and (Y =< 8) and (Y >= 1);
    true -> false
  end.

checkIfOccupied(Board, Position) ->
  maps:is_key(Position, Board).

checkIfMoveFieldBlack(Position) ->
  Field = getFieldColor(Position),
  Field == black.

%-- returns true if there is enemy between positions
checkIfJump(Board, From = {Xfrom, Yfrom}, To = {Xto, Yto}, EnemyColor) ->
  Direction = getDirection(From, To),
  erlang:display(Direction), %ok
  Xdist = erlang:abs(Xto - Xfrom),
  Ydist = erlang:abs(Yto - Yfrom),
  PossibleJump = checkIfJumpPossible(Xdist, Ydist),
  erlang:display(PossibleJump), %ok
  if
    PossibleJump == true ->
      checkFieldsBetween(Board, From, To, Direction, EnemyColor);
    true -> false
  end.

checkIfJumpPossible(Xdist, Ydist) ->
  (Xdist > 1) and (Ydist > 1).

%-- check if between positions there is exactly one enemy
checkFieldsBetween(Board, From, To, Direction, EnemyColor) ->
  Fields = getFieldsBetween(Board, From, To, Direction),
  erlang:display(Fields),
  Discs = lists:filter(fun({_, {_,Type}}) -> Type /= field end, Fields),
  erlang:display(Discs),
  DiscsNum = length(Discs),
  EnemiesNum = length(lists:filter(fun({_,{Col, _}}) -> Col == EnemyColor end, Discs)),
  if
    (DiscsNum == 1) and (EnemiesNum == 1) -> true;
    true -> false
  end.

checkIfTurnsToKing({X, _Y}, white) -> X == 8;
checkIfTurnsToKing({X, _Y}, black) -> X == 1.

%------------------------------ getters ------------------------------

getDirection({Xfrom, Yfrom}, {Xto, Yto}) ->
  Xdir = round((Xto - Xfrom) / erlang:abs(Xto - Xfrom)),
  Ydir = round((Yto - Yfrom) / erlang:abs(Yto - Yfrom)),
  {Xdir, Ydir}.

getFieldsBetween(Board, From, To, Direction) ->
  Pos = getPosInDirection(From, Direction),
  if
    Pos == To ->
      [];
    true ->
      FieldType = getFieldType(Board, Pos),
      [{Pos, FieldType}] ++ getFieldsBetween(Board, Pos, To, Direction)
  end.

getEnemyPosition(Board, From, To, Direction) ->
  Fields = getFieldsBetween(Board, From, To, Direction),
  [{EnemyPosition, _}] = lists:filter(fun({_, {_, Type}}) -> Type /= field end, Fields),
  erlang:display(EnemyPosition),
  EnemyPosition.

getPosInDirection({X, Y}, {Xdir, Ydir}) ->
  XInDirection = X + Xdir,
  YInDirection = Y + Ydir,
  {XInDirection, YInDirection}.

getFieldType(Board, Position) ->
  maps:get(Position, Board, {getFieldColor(Position), field}).

getFieldColor({X, Y}) when (X + Y) rem 2 == 1 -> black;
getFieldColor({X, Y}) when (X + Y) rem 2 == 0 -> white;
getFieldColor(_) -> throw(exception_get_field_color).

getDraught(Board, Position) ->
  maps:get(Position, Board).

%------------------------------ helpful ------------------------------

oppositeColor(white) -> black;
oppositeColor(black) -> white.