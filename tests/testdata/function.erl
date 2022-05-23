-module(function).


foo() ->
    fun() ->
            hello,
            ok
    end,
    Function = fun() ->
                       hello,
                       ok
               end.


bar() ->
    fun() ->
            fun() ->
                    hello,
                    world
            end
    end.
