-module(records_and_maps).

-record(foo, {
          a :: term(),
          b :: term(),
          c :: term()
         }).

-type foo() :: #foo{
                 a :: integer(),
                 b :: integer(),
                 c :: integer()
                }.

-type bar() :: #{
                 a => integer(),
                 b => integer(),
                 c => integer()
                }.


foo() ->
    #foo{
      a = 1,
      b = 2,
      c = 3
     },
    Foo#foo{
      a = 1,
      b = 2,
      c = 3
     },
    #{
      a => 1,
      b => 2,
      c => 3
     },
    Foo#{
      a => 1,
      b => 2,
      c => 3
     }.
