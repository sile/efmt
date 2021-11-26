efmt
====

[![efmt](https://img.shields.io/crates/v/efmt.svg)](https://crates.io/crates/efmt)
[![Documentation](https://docs.rs/efmt/badge.svg)](https://docs.rs/efmt)
[![Actions Status](https://github.com/sile/efmt/workflows/CI/badge.svg)](https://github.com/sile/efmt/actions)
![License](https://img.shields.io/crates/l/efmt)

Erlang code formatter.

**NOTICE** This crate is still in the development phase. So the formatting style can be radically changed in the future.

Examples
--------

```console
$ echo 'foo() -> bar.' | efmt
foo() ->
    bar.
```

```console
$ echo 'foo(x) -> y; foo(A) -> [{A, A}, A].' | efmt --max-columns 10
foo(x) ->
    y;
foo(A) ->
    [{A,
      A},
     A].
```

Limitations
-----------

TODO (e.g., broken forms in an unreachable `-if.` branch, parse-transform, UTF-8 files only)
