-module('weird-macro-before').

-define(FOO, /).
-define(BAR, :format().
-define(baz(A), A).

-export([?baz(hello) ?FOO 0]).

hello() ->
    io ?BAR "World!\n").
