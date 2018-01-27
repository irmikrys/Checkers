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
-export([computerMove/2]).
-define(TREE_DEPTH, 1).

computerMove(Board, Color) ->
  PossibleMoves = logic:getPossibleMoves(Board,Color),
  if (length(PossibleMoves)==0) -> Board;
     (length(PossibleMoves)>0) -> Tree = generateTree(Board,Color,?TREE_DEPTH),
          {NewBoard, _, _, _} = minMax(1,Tree),
           NewBoard
  end.

generateTree(Board, Color, 0)     ->
  {Board, Color, rateBoard(Board,Color), []};
generateTree(Board, Color, Depth) ->
  NewColor = board:nextPlayer(Color),
  AllPossibleMoves = maps:to_list(logic:getPossibleMoves(Board, Color)),
  Children = map(fun(PositionPossibleMoves) ->
                    {From,ToList} = PositionPossibleMoves,
                    map(fun(To) ->
                          generateTree(logic:makeMove(Board, From, To), NewColor, Depth-1)
                        end,
                        ToList)
                 end,
                 AllPossibleMoves),
  {Board, Color, rateBoard(Board,Color), Children}.

minMax(_, {Board, Color, Value, []}) ->
  {Board, Color, Value, []};
minMax(N, {Board, Color, Value, Children}) ->
  ChildrenMinMax = map(fun(Child) -> minMax(N+1, Child) end, Children),
  Func = fun(X, Acc) -> minMaxCondition(N, X, Acc) end,
  Max = foldr(Func, nth(1, ChildrenMinMax), ChildrenMinMax),
  if
    N > 1 -> copyMoveValueForTree({Board, Color, Value, Children}, Max);
    true  -> Max
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

rateBoard(Board,Color) ->
  Filtered = maps:filter(fun(_,V) -> V == {Color,_} end,Board),
  maps:fold(fun(_,V,AccIn) ->
                if V=={Color,disc} -> AccIn + 1;
                   V=={Color,king} -> AccIn + 3
                end
            end,0,Filtered).