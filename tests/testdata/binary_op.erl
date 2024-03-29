%%---10--|----20---|----30---|----40---|----50---|
-module(binary_op).


foo(Bar, Baz)
  when (Bar =:= ?AAA_AAA_AAA orelse
        Bar =:= ?CCC_CCC_CCC_CCC_CCC) andalso
       is_map_key(Bar, Baz) ->
    Qux = (1 + 2 + 3 * 4 + 5 - 6 / 8) +
        (1 + 2 + 3 * 4 + 5 - 6 / 8),
    ok.


bar() ->
    A = [aaa, bbb, ccc] ++
        [[111, 222, 333], [444, 555, 666]] --
        [#{a => b, c => d}, [e, f]],
    A.


baz() ->
    Baz = 1000 * 1000 * 1000 * 1000 * 1000 *
        1000 * 1000 * 1000 * 1000 * 1000 *
        1000 andalso
        10 * 1000 * 10 * 1000 * 10 * 1000 * 10 *
        1000 * 10 * 1000 * 10 * 1000 * 10 * 1000 *
        10 * 1000 * 10 * 1000 * 10,
    Baz.
