-module(include).

-include_lib("foo/foo.hrl").
-include_lib("bar/bar.hrl").

-include("baz.hrl").
-include("qux.hrl").
