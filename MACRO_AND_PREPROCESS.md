How Macros and Directives are Handled
=====================================

TODO: https://github.com/sile/efmt/issues/4

If you want to know the detail of Erlang macros and directives, please refer to
[Erlang Reference Manual - 9. Preprocessor](https://www.erlang.org/doc/reference_manual/macros.html).


`-include` and `-include_lib` Directives
----------------------------------------

To collect macro definitions needed to parse an input file, `efmt` try to process `-include` and `-include_lib` directives in the file as much as possible as the Erlang preprocessor does.
If `efmt` fails to find the include target file, it just ignores the directive.
Note that when `efmt` encounters an unknown macro, that might be defined in the ignored file, during the parse phase, the macro is replaced with the `'EFMT_DUMMY'` atom token.

Resolving the file path specified by `-include_lib` is more complicated than `-include` as the first path component can be the name of an Erlang application rather than a plain directory name.
So, `efmt` invokes `erl -noshell -eval 'io:format("~s", [code:lib_dir($APP_NAME)]), halt().'` command to try to resolve the application directory path.

### Include cache

The macro definitions collected during processing a `-include` or `-include_lib` directive is cached as a JSON file under `$PWD/.efmt/cache/v0/` directory (`v0` is the current cache format version).
The cache file is used when processing the same include target file next time to reduce the overhead of parsing the whole file from scratch.

### `efmt` options

Note that `efmt` provides some options to control how to handle those directives as follows:
- `--include-search-dir` (`-I` in short)
- `--include-cache-dir`
- `--disable-include`
- `--disable-include-cache`

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
