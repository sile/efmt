-define(FOO, foo).
-define(bar(A, B), [A, B, ?FOO]).

main() ->
    {?FOO, ?bar(C, "10")}.
