efmt
====

[![efmt](https://img.shields.io/crates/v/efmt.svg)](https://crates.io/crates/efmt)
[![hex.pm version](https://img.shields.io/hexpm/v/rebar3_efmt.svg)](https://hex.pm/packages/rebar3_efmt)
[![Documentation](https://docs.rs/efmt/badge.svg)](https://docs.rs/efmt)
[![Actions Status](https://github.com/sile/efmt/workflows/CI/badge.svg)](https://github.com/sile/efmt/actions)
![License](https://img.shields.io/crates/l/efmt)

An opinionated Erlang code formatter.

Features
--------

- Opinionated: only maximum line length is configurable by users
- [Emacs Erlang Mode](https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html) friendly indentation
- Preserves non-whitespace tokens of the original text as-is
  - Ensures the code after formatting keeps the same semantic meaning
- Provides a rebar3 plugin: [rebar3_efmt](https://hex.pm/packages/rebar3_efmt)
- Almost thoroughly macro support

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

Limitations
-----------

- UTF-8 files only
-
TODO (e.g., broken forms in an unreachable `-if.` branch, parse-transform)
