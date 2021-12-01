How Macros and Directives are Handled
=====================================

TODO: https://github.com/sile/efmt/issues/4

References:
- [Erlang Reference Manual - 9. Preprocessor](https://www.erlang.org/doc/reference_manual/macros.html)

`-include` and `-include_lib` directives
-----------------------------------------


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
