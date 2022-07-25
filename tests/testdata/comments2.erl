%%---10--|----20---|----30---|----40---|----50---|
-module(comments2).


foo(A)
  when A == ?FOO orelse
       %% comment1
       A == ?BAR ->
    hello;
foo(B) ->
    case B of
        1 ->
            ok
            %% comment2
    end,
    world.
