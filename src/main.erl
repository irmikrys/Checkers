%%%-------------------------------------------------------------------
%%% @author Sebastian Skoczeń & Irmina Krysiak
%%% Created : 26. Dec 2017 17:59
%%%-------------------------------------------------------------------
-module(main).   % module name must be the same as the file name
-export([start/0]).
-compile(export_all).

start() ->
  io:fwrite("~n============= Draughts =============~n~n"),
  io:fwrite(board:showBoard(board:emptyBoard())).
