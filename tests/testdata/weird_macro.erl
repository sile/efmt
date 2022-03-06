-module(weird_macro).

-define(FOO, /).
-define(BAR, :format().
-define(baz(A), A).
-define(qux, -> [1, 2, 3], [).
-define(quux, )], [2,).
-define(a(A, B), A).

-export([?baz(?baz(main))?FOO 0]).


hello(A) ->
    io?BAR "hello ~p\n", [A]).


main() ?a(?qux a, b)
    , c],
    [1, hello(world?quux
    3].
