-module('weird-macro-before').

-define(FOO, /).
-define(BAR, :format().
-define(baz(A), A).
-define(qux, [1, 2, 3], [).
-define(quux, a], [b,).

-export([?baz(echo) / 1, ?baz(hello) ?FOO 0]).

echo(A) -> io ?BAR "World! ~p\n", [A]).
hello() -> ?qux a, b, c], [1, ?quux 2].
