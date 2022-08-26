How Macros and Directives are Handled
=====================================

Note that this document doesn't explain Erlang macros and directives themselves.
So if you want to know the detail of them, please refer to
[Erlang Reference Manual - 9. Preprocessor](https://www.erlang.org/doc/reference_manual/macros.html).


Macros
------

A unique point of `efmt` among various Erlang formatters is that it can handle macros correctly.

For example, `efmt` can format the following code containing unusual macros without errors.

### Original code

```erlang
-module(weird_macro).

-define(FOO, /).
-define(BAR, :format().
-define(baz(A), A).
-define(qux, -> [1, 2, 3], [).
-define(quux, )], [2,).
-define(a(A, B), A).

-export([?baz(?baz(main))?FOO 0]).

hello(A)->io?BAR "hello ~p\n",[A]).
main()?a(?qux a, b), c],[1, hello(world?quux 3].
```

### Formatted code

`$ efmt weird_macro.erl`

```erlang
-module(weird_macro).

-define(FOO, /).
-define(BAR, :format().
-define(baz(A), A).
-define(qux, -> [1, 2, 3], [).
-define(quux, )], [2,).
-define(a(A, B), A).

-export([?baz(?baz(main))?FOO 0]).

hello(A) ->
    io?BAR "hello ~p\n", [A]).
main() ?a(?qux a, b)
    , c],
    [1, hello(world?quux
    3].
```

To make it possible, during the parse phase, `efmt` collects macro definitions (i.e., `-define` directives) and expands macro calls (i.e., `?MACRO_NAME`) to build an abstract syntax tree-like structure from the input text.
Then, during the format phase, `efmt` traverses the tree and emits the formatted text representing each tree node.
When it visits tree nodes expanded from a macro, the formatted text of the original macro call is emitted instead.


`-include` and `-include_lib` Directives
----------------------------------------

`efmt` doesn't process `-include` and `-include_lib` directives. The macros defined in those include files are expanded to an atom.


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
