-module('weird-empty-macro-before').

-define(EMPTY,).

?EMPTY.

main() ->
    hello.
