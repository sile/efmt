%%---10--|----20---|----30---|----40---|----50---|
-module(long_spec).

-spec foo(#bar{}, #baz{}) ->
          {ok, #quz{}} | {error, term()}.