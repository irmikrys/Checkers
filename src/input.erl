%%%-------------------------------------------------------------------
%%% @author Sebastian Skoczeń & Irmina Krysiak
%%% Created : 27. sty 2018 14:50
%%%-------------------------------------------------------------------

-module(input).
-author("irmi").

%% API
-export([getInput/0]).

-include("constants.hrl").

%------------------------ parse player input--------------------------

getInput() ->
  io:fwrite("Enter positions properly formatted e.g. A1:~n"),
  try
    From = getFromPosition(),
    To = getToPosition(),
    [From, To]
  catch
    _:_ ->
      io:fwrite("Wrong input! Try again:~n"),
      getInput()
  end.

getFromPosition() ->
  {ok, [From]} = io:fread("From: ", "~s"),
  parseToPosition([From]).

getToPosition() ->
  {ok, [To]} = io:fread("To: ", "~s"),
  parseToPosition([To]).

parseToPosition(String) ->
  Length = string:length(String),
  if
    Length == 2 ->
      Column = parseColumn(string:slice(String, 0, 1)),
      Row = parseRow(string:slice(String, 1, 1)),
      {Row, Column};
    true -> throw(cannot_parse_wrong_input_length)
  end.

parseColumn(String) ->
  Map = mapCharsToNums(),
  maps:get(String, Map).

parseRow(String) ->
  Row = list_to_integer(String),
  if
    (Row < 9) and (Row > 0) -> Row;
    true -> throw(cannot_parse_row)
  end.

mapCharsToNums() ->
  #{"A" => 1, "B" => 2, "C" => 3, "D" => 4, "E" => 5, "F" => 6, "G" => 7, "H" => 8}.
