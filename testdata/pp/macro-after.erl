-define(FOO, foo).
-define(bar(A, B), [A, B, foo]).

main() ->
    {foo, [C, "10", foo]}.
