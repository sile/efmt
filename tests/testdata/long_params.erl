%%---10--|----20---|----30---|----40---|----50---|
-module(long_params).

-export([foo/7, bar/6, baz/5]).


foo(AAAAA,
    BBBBB,
    CCCCC,
    DDDDD,
    EEEEE,
    FFFFF,
    GGGGG) ->
    [AAAAA, BBBBB, CCCCC, DDDDD, EEEEE, FFFFF,
     GGGGG].


bar(AAAAA, BBBBB, CCCCC, DDDDD, EEEEE, FFFFF) ->
    [AAAAA, BBBBB, CCCCC, DDDDD, EEEEE, FFFFF].


baz(AAAAA, BBBBB, CCCCC, DDDDD, EEEEE) ->
    [AAAAA, BBBBB, CCCCC, DDDDD, EEEEE, AAAAA,
     BBBBB, CCCCC, DDDDD, EEEEE].
