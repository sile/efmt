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


-spec bar() -> term().
bar() ->
    fun() ->
            fun() ->
                    hello,
                    world
            end
    end.  % bar


baz() ->
    ok.


-type qux() :: any().
%% This is qux.


-spec qux() -> ok.
qux() ->
    ok.


%%----


quux() ->
    ok.
