-module('weird-macro-before').

-define(FOO, /).
-define(BAR, :format().

-export([hello ?FOO 0]).

hello() ->
    io ?BAR "World!\n").
