%%---10--|----20---|----30---|----40---|----50---|
-module(comments2).

foo(A)
  when A == ?FOO orelse
       %% comment1
       A == ?BAR ->
    hello;
foo(_) ->
    world.
