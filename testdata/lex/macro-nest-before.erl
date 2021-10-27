-define(FOO, foo).
-define(BAR(X), [X]).
-define(BAZ(A, B), [A, ?BAR(B)]).

main() ->
    ?BAZ(C, ?FOO).
