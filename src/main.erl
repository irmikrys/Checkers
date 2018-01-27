%%%-------------------------------------------------------------------
%%% @author Sebastian Skoczeń & Irmina Krysiak
%%% Created : 26. Dec 2017 17:59

%% uruchamianie shella erlanga: erl +pc unicode
%% przykładowe wypisanie znaku: board:show({white, disc}).

%%%-------------------------------------------------------------------
-module(main).
-export([start/0, initBoard/0]).

start() ->
  io:fwrite("~n============= Draughts =============~n~n"),
  io:fwrite(board:showBoard(initBoard())).

initBoard() ->
  ListWhites = [{{X, Y}, {white, disc}} ||
    X <- lists:seq(1, 3), Y <- lists:seq(1, 8), (X + Y) rem 2 == 1],
  ListBlacks = [{{X, Y}, {black, disc}} ||
    X <- lists:seq(6, 8), Y <- lists:seq(1, 8), (X + Y) rem 2 == 1],
  List = lists:append([ListWhites, ListBlacks]),
  maps:from_list(List).
