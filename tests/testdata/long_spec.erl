%%---10--|----20---|----30---|----40---|----50---|
-module(long_spec).


-spec foo(#bar{}, #baz{}) ->
          {ok, #quz{}} | {error, term()}.

-spec bar(#bar{}, #baz{}) ->
          {ok, #quz{}} |
          {error, term()} |
          undefined.

-spec baz() ->
          {ok, term()} | {error, Reason} | timeout
              when Reason :: term().
