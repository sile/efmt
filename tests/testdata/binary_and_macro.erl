%%---10--|----20---|----30---|----40---|----50---|
-module(binary_and_macro).

-define(FOO, <<"hello world">>).

x0([], X) ->
    X;
x0([#x1{x2 = X3} | X4], X5) ->
    x0(X4, <<?FOO/binary, X3:16, X5/binary>>).
