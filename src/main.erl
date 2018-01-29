%%%-------------------------------------------------------------------
%%% @author Sebastian Skoczeń & Irmina Krysiak
%%% Created : 26. Dec 2017 17:59

%% uruchamianie shella erlanga: erl +pc unicode
%% cover:compile_directory().
%% przykładowe wypisanie znaku: board:show({white, disc}).

%%%-------------------------------------------------------------------
-module(main).
-export([start/0, initBoard/0, test/0]).
-import(ai, [nextPlayer/1]).

start() ->
  io:fwrite("~n============= Draughts =============~n~n"),
  CurrentPlayer = white,
  play(initBoard(),CurrentPlayer).

play(Board,CurrentPlayer) ->
  NewPlayer = nextPlayer(CurrentPlayer),
  io:fwrite(lists:concat(["Player ",CurrentPlayer," move!~n"])),
  io:fwrite(board:showBoard(Board)),
  [From,To] = input:getInput(),
  NewBoard = logic:makeMove(Board,From,To),
  play(NewBoard,NewPlayer).

initBoard() ->
  ListWhites = [{{X, Y}, {white, disc}} ||
    X <- lists:seq(1, 3), Y <- lists:seq(1, 8), (X + Y) rem 2 == 1],
  ListBlacks = [{{X, Y}, {black, disc}} ||
    X <- lists:seq(6, 8), Y <- lists:seq(1, 8), (X + Y) rem 2 == 1],
  List = lists:append([ListWhites, ListBlacks]),
  maps:from_list(List).

%% ------ test generating tree ------

test() ->
  io:fwrite("~n============= Testing =============~n~n"),
  Board = initBoard(),
  %io:fwrite(Board),
  playAI(Board,white).
  %NewBoard = ai:computerMove(initBoard(),white),
  %io:fwrite(board:showBoard(NewBoard)).

playAI(Board,CurrentPlayer) ->
  NewPlayer = nextPlayer(CurrentPlayer),
  io:fwrite(lists:concat(["Player ",CurrentPlayer," move!~n"])),
  io:fwrite(board:showBoard(Board)),
  NewBoard = ai:computerMove(Board,CurrentPlayer),
  {ok,[_]} = io:fread("Nextmove: ","~s"),
  playAI(NewBoard,NewPlayer).