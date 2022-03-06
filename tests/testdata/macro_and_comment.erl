%%---10--|----20---|----30---|----40---|----50---|
-module(macro_and_comment).

-define(FOO, <<"hello world">>).


hello() ->
    ?FOO.  % bar
