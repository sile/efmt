%%---10--|----20---|----30---|----40---|----50---|
-module(binary_op).


foo(Bar, Baz)
  when (Bar =:= ?AAA_AAA_AAA orelse
        Bar =:= ?CCC_CCC_CCC_CCC_CCC) andalso
       is_map_key(Bar, Baz) ->
    Qux = (1 + 2 + 3 * 4 + 5 - 6 / 8) +
        (1 + 2 + 3 * 4 + 5 - 6 / 8),
    ok.
