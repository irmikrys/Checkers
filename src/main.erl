%%%-------------------------------------------------------------------
%%% @author Sebastian Skoczeń & Irmina Krysiak
%%% Created : 26. Dec 2017 17:59
%%%-------------------------------------------------------------------
-module(main).   % module name must be the same as the file name
-import(io,[fwrite/1]). % imports fwrite function from io module
-compile(export_all).

start () ->
  fwrite("Draughts").
