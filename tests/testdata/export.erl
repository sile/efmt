%%---10--|----20---|----30---|----40---|----50---|
-module(export).

-export([foo/0,
         bar/0,
         baz/0]).

-export_type([foo/0, foo/1, foo/2]).

-export([foo/0,

         foo/1,
         bar/0, bar/1,
         baz/0, baz/1, baz/2, baz/3, baz/4, baz/5,
         baz/6,
         qux/0]).
