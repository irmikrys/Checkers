%%%-------------------------------------------------------------------
%%% @author Sebastian Skoczeń & Irmina Krysiak
%%% Created : 26. Dec 2017 17:59
%%%-------------------------------------------------------------------
-module(main).   % module name must be the same as the file name
-import(io,[fwrite/2]). % imports fwrite function from io module
-compile(export_all).

start () ->
  for(2,fun hello/1),
  fwrite("~w",[reverse([1,2,3,4,a,b,c])]).

for(1,Fun) -> Fun(1);
for(N,Fun) -> for(N-1,Fun), Fun(N).


reverse(L) -> reverse(L,[]).
reverse([],Acc) -> Acc;
reverse([H|T],Acc) -> reverse(T,[H|Acc]).

hello (N) -> fwrite("Hello World!~w~n",[N]).

