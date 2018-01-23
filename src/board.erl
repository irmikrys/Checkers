%%%-------------------------------------------------------------------
%%% @author Sebastian Skoczeń & Irmina Krysiak
%%% Created : 27. Dec 2017 12:13
%%%-------------------------------------------------------------------
-module(board).
-export([start/0]).
-compile(export_all).

-include("constants.hrl").

%%------------------------- showing board ----------------------------
%% uruchamianie shella erlanga: erl +pc unicode
%% przykładowe wypisanie znaku: board:show({white, disc}).

show(?WHITE_DISC) ->
  [9786];
show(?BLACK_DISC) ->
  [9787];
show(?WHITE_KING) ->
  [9812];
show(?BLACK_KING) ->
  [9818].


up_down_label() ->
  "  A B C D E F G H".

%%--------------------------------------------------------------------

-record(piece,{type :: piece(),color :: colour(),history :: [move()]}).
-record(player,{color :: colour(),history :: [move()]}).
-record(game,{board :: dict:dict(field(),piece()),pieceCount :: {8,8}, players :: {player(),player()},history :: [move()]}).

-type player() :: white_player | black_player. % player names
-type move() :: {field(), field(), piece()}. % {from, to, piece}
-type colour() :: black | white.
-type piece() :: white_men | white_king | black_men | black_king.
-type field() :: a8 | b8 | c8 | d8 | e8 | f8 | g8 | h8 |
                 a7 | b7 | c7 | d7 | e7 | f7 | g7 | h7 |
                 a6 | b6 | c6 | d6 | e6 | f6 | g6 | h6 |
                 a5 | b5 | c5 | d5 | e5 | f5 | g5 | h5 |
                 a4 | b4 | c4 | d4 | e4 | f4 | g4 | h4 |
                 a3 | b3 | c3 | d3 | e3 | f3 | g3 | h3 |
                 a2 | b2 | c2 | d2 | e2 | f2 | g2 | h2 |
                 a1 | b1 | c1 | d1 | e1 | f1 | g1 | h1.

start() ->
  newGame().

newGame() ->
  Game = #game{board = initiateBoard(),players = [white_player,black_player],history = []},
  StartingPlayer = lists:nth(1,Game#game.players),
  {FinishedGame,Winner} = gameLoop(Game,StartingPlayer,null),
  io:fwrite("The winner is: ~s!~n",[Winner]).

gameLoop(Game,Player,Winner) ->
  if
    Winner /= null ->
      {Game,Winner};
    true ->
      % the rest of the loop
      %printBoard(Game#game.board),
      %makeMove(),
      io:fwrite("~s~n",[Player]),
      PostTurnGame = Game,
      % add checking if one of the players have won the game
      case Player == lists:nth(1,Game#game.players) of
        true -> NewPlayer = lists:nth(2,Game#game.players);
        false -> NewPlayer = lists:nth(1,Game#game.players)
      end,
    gameLoop(PostTurnGame,NewPlayer,Winner)
  end.



initiateBoard() ->
  Init_Pieces = [{b1,white_men},{d1,white_men},{f1,white_men},{h1,white_men},{a2,white_men},{c2,white_men},{e2,white_men},{g2,white_men},{b7,black_men},{d7,black_men},{f7,black_men},{h7,black_men},{a8,black_men},{c8,black_men},{e8,black_men},{g8,black_men}],
  Board = dict:from_list(Init_Pieces),
  Board.

printBoard(Board) ->
  List = dict:to_list(Board),
  printList(List).

printList([]) -> [];
printList([H|T]) ->
  displayTuple(H),
  printList(T).


displayTuple(Tuple) ->
  Key = element(1,Tuple),
  Value = element(2,Tuple),
  io:fwrite("~s => ~s~n",[Key,Value]).