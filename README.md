efmt
====

[![efmt](https://img.shields.io/crates/v/efmt.svg)](https://crates.io/crates/efmt)
[![hex.pm version](https://img.shields.io/hexpm/v/rebar3_efmt.svg)](https://hex.pm/packages/rebar3_efmt)
[![Documentation](https://docs.rs/efmt/badge.svg)](https://docs.rs/efmt)
[![Actions Status](https://github.com/sile/efmt/workflows/CI/badge.svg)](https://github.com/sile/efmt/actions)
![License](https://img.shields.io/crates/l/efmt)

An Erlang code formatter.

Features
--------

- Opinionated: only maximum line length is configurable by users
- [Emacs Erlang Mode](https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html) friendly indentation
- Preserves non-whitespace tokens of the original text as-is
  - Ensures the code after formatting keeps the same semantic meaning
- Provides a rebar3 plugin: [rebar3_efmt](https://hex.pm/packages/rebar3_efmt)
- Thorough macro support ([MACRO_AND_PREPROCESS.md](MACRO_AND_PREPROCESS.md))

An Formatting Example
---------------------

### Before

```erlang
-module(example).
-export(
  [fac/1]
).

fac(1) -> 1; fac(N) -> N*fac(N-1).
```

### After

```erlang
-module(example).
-export([fac/1]).

fac(1) ->
    1;
fac(N) ->
    N * fac(N - 1).
```

Please refer to [FORMAT_RULES.md](FORMAT_RULES.md) about the formatting style.

Installation
------------

Usage
-----

Editor Integrations
-------------------

TODO

Comparison with other formatters
---------------------------------

### erlfmt

- formatting style
- error handling
- macro handling
- formatting speed
- development phase

Limitations
-----------

There are some limitations that are not planned to be addressed in the future:
- Only supports UTF-8 files
- Doesn't process parse transforms
  - That is, if a parse transform has introduced custom syntaxes in your Erlang code, `efmt` could fail
