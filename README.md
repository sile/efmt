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

### With [Rebar3](https://github.com/erlang/rebar3)

Just add the following line to your `rebar.config`.

```erlang
{plugins, [rebar3_efmt]}.
```

Then, you can run the `$ rebar3 efmt` command.

If you want to provide the default options via `rebar.config`,
please specify an entry that has `efmt` as the key and `efmt`'s options as the value.
```erlang
{efmt, [{print_width, 100}]}.  % Sets the maximum line length hint to 100.
```

Note that `rebar3_efmt` tries to automatically download a pre-built binary (see the next section) for your environment.
However, if there is not a suitable one, you need to build the `efmt` binary on your own.

### Pre-built binaries

Pre-built binaries for Linux and MacOS are available in [the releases page](https://github.com/sile/efmt/releases).

```console
// An example to download the binary for Linux.
$ curl -L https://github.com/sile/efmt/releases/download/${VERSION}/efmt-${VERSION}.x86_64-unknown-linux-musl -o efmt
$ chmod +x efmt
$ ./efmt
```

### With [Cargo](https://doc.rust-lang.org/cargo/)

If you have installed `cargo` (the package manager for Rust), you can install `efmt` with the following command:
```console
$ cargo install efmt
$ efmt
```

Usage
-----

Formats an Erlang file (assuming `example.erl` in the above example is located in the current directory):
```console
$ efmt example.erl  # or `rebar3 efmt example.erl`

// You can specify multiple files.
$ efmt example.erl rebar.config ...
```

Checks diff between the original text and the formatted one:
```console
$ efmt -c example.erl  # or `rebar3 efmt -c example.erl`
...
    1   1    | -module(example).
    2        |--export(
    3        |-  [fac/1]
    4        |-).
        2    |+-export([fac/1]).
    5   3    |
    6        |-fac(1) -> 1; fac(N) -> N*fac(N-1).
        4    |+fac(1) ->
        5    |+    1;
        6    |+fac(N) ->
        7    |+    N * fac(N - 1).
...

// If you omit the filename, all the Erlang-like files (i.e., `*.{erl, hrl, app.src}` and `rebar.config`)
// are included in the target (if you're in a git repository the files specified by `.gitignore` are excluded).
$ efmt -c
```

Overwrites the original file with the formatted one:
```console
$ efmt -w example.erl  # or `rebar3 efmt -w example.erl`

// As with `-c` option, you can omit the filename arg.
$ emf -w
```

For the other command-line options, please see the help document:
```console
// Short doc.
$ efmt -h  # or `rebar3 efmt -h`

// Long doc.
$ efmt --help  # or `rebar3 efmt --help`
```

Editor Integrations
-------------------

TODO (contribution welcome)

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
