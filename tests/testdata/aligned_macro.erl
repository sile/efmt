-module(aligned_macro).

-define(a,   1).
-define(bb,  2).
-define(ccc, 3).

-define(dddd,  11).
-define(eeeee, 123).

-define(a, 1).
-define(bb(B), 2).
-define(ccc,  3).
-define(dddd, 4).

-define(a, 1).
-define(bb,  % foo
        2).
-define(ccc,  3).
-define(dddd, 4).

-define(a,  1).
-define(bb, 2).
%% bar
-define(ccc,  3).
-define(dddd, 4).

-define('meow-1', "Meow").
-define('meow-2', "Meow Meow").
-define('meow-3', "Meow Meow Meow").
