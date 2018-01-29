%%%-------------------------------------------------------------------
%%% @author Sebastian Skoczeń & Irmina Krysiak
%%% Created : 22. sty 2018 16:09
%%%-------------------------------------------------------------------
-author("irmi").

-define(WHITE_DISC, {white, disc}).
-define(BLACK_DISC, {black, disc}).
-define(WHITE_KING, {white, king}).
-define(BLACK_KING, {black, king}).
-define(WHITE_FIELD, {white, field}).
-define(BLACK_FIELD, {black, field}).

-define(NUMS, lists:seq(1, 8)).
-define(CHARS, ["A", "B", "C", "D", "E", "F", "G", "H"]).

-define(TREE_DEPTH, 5).

%-- directions
-define(SW, {+ 1, -1}).
-define(NW, {-1, -1}).
-define(NE, {-1, + 1}).
-define(SE, {+ 1, + 1}).

