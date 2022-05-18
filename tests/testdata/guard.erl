%%---10--|----20---|----30---|----40---|----50---|
-module(guard).


foo()
  when aaa;
       bbb,
       ccc;
       ddd;
       eee,
       fff;
       ggg;
       hhh,
       iii ->
    ok.


bar()
  when aaa andalso
       bbb orelse
       ccc andalso
       ddd orelse
       eee orelse
       (fff orelse
        ggg andalso
        hhh orelse
        iii andalso
        jjj) orelse
       kkk orelse
       lll ->
    ok.


baz()
  when aaa andalso bbb orelse ccc;
       ddd orelse eee,
       (fff orelse
        ggg andalso
        hhh orelse
        iii andalso
        jjj) orelse
       kkk ->
    ok.
