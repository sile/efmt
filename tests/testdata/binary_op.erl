%%---10--|----20---|----30---|----40---|----50---|
-module(binary_op).


foo(Bar, Baz)
  when (Bar =:= ?AAA_AAA_AAA orelse
        Bar =:= ?CCC_CCC_CCC_CCC_CCC) andalso
       is_map_key(Bar, Baz) ->
    ok.
