How Macros and Directives are Handled
=====================================

TODO: https://github.com/sile/efmt/issues/4

References:
- [Erlang Reference Manual - 9. Preprocessor](https://www.erlang.org/doc/reference_manual/macros.html)

`-include` and `-include_lib` Directives
----------------------------------------

TODO: About `--include-search-dir` (`-I`) option

TODO: To handle `-include_lib`, `efmt` invokes `erl` command

The macro definitions collected during processing a `-include` or `-include_lib` directive is cached as a JSON file under `$PWD/.efmt/cache/v0/` directory (`v0` is the current cache format version).
The cache file is used when processing the same include target file next time to reduce the overhead of parsing the whole file from scratch.

Note that `efmt` provides some options to control how to handle those directives as follows:
- `--disable-include`
- `--disable-include-cache`
- `--include-cache-dir`

Please run `$ efmt --help` to see the descriptions of those options.


Flow Control Directives
-----------------------

`efmt` doesn't recognize the following directives relating to flow control:
- `-undef(Macro)`
- `-ifdef(Macro)`
- `-ifndef(Macro)`
- `-else`
- `-endif`
- `-if(Condition)`
- `-elif(Condigion)`

Those are treated as ordinal attributes.
That is, both the "then" and "else" branch forms are always processed.
In many cases, this behavior doesn't cause a problem.
However, if either of the branches contains corrupted code, `efmt` would fail to format the file (see the example code below).

```erlang
-define(FOO, foo).

-ifdef(FOO).

%% As the `FOO` macro is defined in the same file, the Erlang compiler always evaluates this branch.
foo() -> ok.

-else.

%% On the other hand, this branch is always removed by the preprocessor.
%% But, `efmt` tries to parse this branch. Then it fails as the following function declaration is invalid.
foo()

-endif.
```
