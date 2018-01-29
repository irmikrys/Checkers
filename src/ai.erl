%%%-------------------------------------------------------------------
%%% @author 4figh
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jan 2018 12:49
%%%-------------------------------------------------------------------
-module(ai).
-author("4figh").

%% API
-import(lists, [foldr/3, map/2, nth/2]).
-export([computerMove/2, nextPlayer/1]).
-define(TREE_DEPTH, 1).

computerMove(Board, Color) ->
  PossibleMoves = logic:getPossibleMoves(Board, Color),
  PossibleMovesSize = maps:size(PossibleMoves),
  if (PossibleMovesSize > 0) -> Tree = concurrentTreeGeneration(Board, Color, ?TREE_DEPTH),
    {NewBoard, _, _, _} = minMax(1, Tree),
    NewBoard;
    true -> Board
  end.

concurrentTreeGeneration(Board, Color, Depth) ->
  register(threadpool, spawn(fun() -> threadPool(erlang:system_info(schedulers_online) - 1) end)),
  Pid = self(),
  spawn(fun() -> concurrentTreeGeneration(Board, Color, Depth, Pid) end),
  Result = receive X -> X end,
  threadpool ! finish,
  Result.

concurrentTreeGeneration(Board, Color, Depth, Parent) ->
  Pid = self(),
  ChildFunc = fun(PossibleMove) -> UseNewThread = if Depth > 1 -> anyThreadAvailable();
                                                    true -> reject
                                                  end,
    case UseNewThread of
      reject ->
        {sync, PossibleMove};
      ok ->
        From = nth(1, PossibleMove),
        To = nth(2, PossibleMove),
        NewBoard = logic:makeMove(Board, From, To),
        spawn(fun() -> concurrentTreeGeneration(NewBoard, Color, Depth - 1, Pid) end),
        async
    end
              end,
  PossibleMoves = logic:getPossibleMoves(Board, Color),
  AllPossibleMoves = maps:fold(fun(K, V, AccIn) ->
    if V /= [] ->
      AccIn ++ lists:foldl(fun(Element, Acc) ->
        Acc ++ [[K, Element]]
                           end,
        [], V);
      true -> AccIn
    end
                               end, [], PossibleMoves),
  Children = map(ChildFunc, AllPossibleMoves),
  ChildrenRes = map(fun(Result) -> case Result of
                                     {sync, PossibleMove} ->
                                       From = nth(1, PossibleMove),
                                       To = nth(2, PossibleMove),
                                       NewBoard = logic:makeMove(Board, From, To),
                                       generateTree(NewBoard, Color, Depth - 1);
                                     async ->
                                       receive X -> X end
                                   end end,
    Children),
  threadpool ! freeThread,
  Parent ! {Board, Color, rateBoard(Board, Color), ChildrenRes}.

generateTree(Board, Color, 0) ->
  {Board, Color, rateBoard(Board, Color), []};
generateTree(Board, Color, Depth) ->
  NewColor = nextPlayer(Color),
  PossibleMoves = logic:getPossibleMoves(Board, Color),
  AllPossibleMoves = maps:fold(fun(K, V, AccIn) ->
    if V /= [] ->
      AccIn ++ lists:foldl(fun(Element, Acc) ->
        Acc ++ [[K, Element]]
                           end,
        [], V);
      true -> AccIn
    end
                               end, [], PossibleMoves),
  Children = map(fun(PositionPossibleMoves) ->
    From = nth(1, PositionPossibleMoves),
    To = nth(2, PositionPossibleMoves),
    generateTree(logic:makeMove(Board, From, To), NewColor, Depth - 1)
                 end,
    AllPossibleMoves),
  {Board, Color, rateBoard(Board, Color), Children}.

minMax(_, {Board, Color, Value, []}) ->
  {Board, Color, Value, []};
minMax(N, {Board, Color, Value, Children}) ->
  ChildrenMinMax = map(fun(Child) -> minMax(N + 1, Child) end, Children),
  Func = fun(X, Acc) -> minMaxCondition(N, X, Acc) end,
  Max = foldr(Func, nth(1, ChildrenMinMax), ChildrenMinMax),
  if
    N > 1 -> copyMoveValueForTree({Board, Color, Value, Children}, Max);
    true -> Max
  end.

minMaxCondition(N, X, Acc) ->
  IsOdd = odd(N),
  XValue = valueForTree(X),
  AccValue = valueForTree(Acc),
  if
    IsOdd and (XValue > AccValue) -> X;
    IsOdd -> Acc;
    XValue < AccValue -> X;
    true -> Acc
  end.

valueForTree({_, _, Value, _}) -> Value.

copyMoveValueForTree({Board, Color, _, _}, {_, _, Value2, Children2}) ->
  {Board, Color, Value2, Children2}.

even(X) -> X band 1 == 0.
odd(X) -> not even(X).

rateBoard(Board, Color) ->
  Filtered = maps:filter(fun(_, V) -> {ColorPiece, _} = V, ColorPiece == Color end, Board),
  maps:fold(fun(_, V, AccIn) ->
    if V == {Color, disc} -> AccIn + 1;
      V == {Color, king} -> AccIn + 3
    end
            end, 0, Filtered).

nextPlayer(white) -> black;
nextPlayer(black) -> white.

threadPool(AvailableCores) ->
  receive
    finish -> ok;
    freeThread -> threadPool(AvailableCores + 1);
    {canGetNew, Sender} ->
      if
        AvailableCores > 0 -> Sender ! ok,
          threadPool(AvailableCores - 1);
        true -> Sender ! reject,
          threadPool(0)
      end
  end.

anyThreadAvailable() -> threadpool ! {canGetNew, self()},
  receive X -> X end.
