efmt
====

[![efmt](https://img.shields.io/crates/v/efmt.svg)](https://crates.io/crates/efmt)
[![hex.pm version](https://img.shields.io/hexpm/v/rebar3_efmt.svg)](https://hex.pm/packages/rebar3_efmt)
[![vscode version](https://img.shields.io/vscode-marketplace/v/sile.efmt.svg?label=vscode)](https://marketplace.visualstudio.com/items?itemName=sile.efmt)
[![Documentation](https://docs.rs/efmt/badge.svg)](https://docs.rs/efmt)
[![Actions Status](https://github.com/sile/efmt/workflows/CI/badge.svg)](https://github.com/sile/efmt/actions)
![License](https://img.shields.io/crates/l/efmt)

An Erlang code formatter.

[Online demo](https://sile.github.io/efmt/examples/efmt.html).

Features
--------

- An opinionated formatter
  - No configuration options
  - If items (e.g., `case` blocks, lists, records) contain newlines in the original code, those are processed in multi-line mode
- [Emacs Erlang Mode](https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html) friendly indentation with some exceptions
- Preserves non-whitespace tokens of the original text as-is
  - Ensures the code after formatting keeps the same semantic meaning
- Provides a rebar3 plugin: [rebar3_efmt](https://hex.pm/packages/rebar3_efmt)
- Thorough macro support ([MACRO_AND_DIRECTIVE.md](MACRO_AND_DIRECTIVE.md))

An Formatting Example
---------------------

### Before

```erlang
-module(example).
-export(
  [fac/1]
).

fac(1)->
1;fac(N   )
-> N*fac(
N-1).
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

Installation
------------

### With [Rebar3](https://github.com/erlang/rebar3)

Just add the following line to your `rebar.config`.

```erlang
{project_plugins, [rebar3_efmt]}.
```

Then, you can run the `$ rebar3 efmt` command.

If you want to provide the default options via `rebar.config`,
please specify an entry that has `efmt` as the key and `efmt`'s options as the value.
```erlang
{efmt, [{exclude_file, "rebar.config"}]}.
```

Note that `rebar3_efmt` tries to automatically download a pre-built binary (see the next section) for your environment.
However, if there is not a suitable one, you need to build the `efmt` binary on your own.

### Pre-built binaries

Pre-built binaries for Linux and MacOS are available in [the releases page](https://github.com/sile/efmt/releases).

```console
// An example to download the binary for Linux.
$ VERSION=0.18.0
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
--- a/example.erl
+++ b/example.erl
@@ -1,9 +1,8 @@
 -module(example).
--export(
-  [fac/1]
-).
+-export([fac/1]).

-fac(1)->
-1;fac(N   )
--> N*fac(
-N-1).
+
+fac(1) ->
+    1;
+fac(N) ->
+    N * fac(N - 1).


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

### How to keep some areas from being formatted

If you want to keep the style of some areas in your input text,
please use `@efmt:off` and `@efmt:on` comments as follows:

```erlang
foo() ->
    %% @efmt:off
    LargeList =
      [1,2,3,...,
       998,999,1000],
    %% @efmt:on

    bar(LargeList).
```

Editor Integrations
-------------------

- Emacs: [emacs-format-all-the-code](https://github.com/lassik/emacs-format-all-the-code)
- VSCode: [extension](https://marketplace.visualstudio.com/items?itemName=sile.efmt)
- Sublime Text: [Formatter](https://packagecontrol.io/packages/Formatter)

Differences with other Erlang formatters
-----------------------------------------

Since I'm not familiar with other Erlang formatters, and [the README.md of `erlfmt`](https://github.com/WhatsApp/erlfmt/blob/main/README.md) already provides a good comparison table among various formatters, I only describe the differences between `efmt` and `erlfmt` here.

Note that in the following examples, I used `efmt-v0.11.0` and `erlfmt-v1.0.0`.

### Formatting style

I think the formatting style of `efmt` is much different from `erlfmt`.
IMO, this is a major point when you decide which one you should choose.
If you like the `erlfmt` style. It's okay. I recommend using `erlfmt`.
But, if you like the `efmt` style. It's welcomed. Please use `efmt`.

It's hard work to pick up all difference points here.
So I just give you some formatted code examples and hope they give you a sense.

#### Original code

```erlang
-module(foo).

-spec hello(term(), integer()) ->
 {ok, integer()} | {error, Reason :: term()} |
          timeout.
hello({_, _, A, _,
 [B, _, C]}, D) -> {ok,
A + B +
C + D};
hello(Error, X) when not is_integer(X);
                     is_atom(X) ->
    {error, Error};
hello(#record{foo=[_,_],
bar=#{qux := 10}}, World) ->
    World.
```

Let's see how `erlfmt` and `efmt` format the above code.

#### `erlfmt` formatted code

`$ erlfmt foo.erl`
```erlang
-module(foo).

-spec hello(term(), integer()) ->
    {ok, integer()}
    | {error, Reason :: term()}
    | timeout.
hello({_, _, A, _, [B, _, C]}, D) ->
    {ok,
        A + B +
            C + D};
hello(Error, X) when
    not is_integer(X);
    is_atom(X)
->
    {error, Error};
hello(
    #record{
        foo = [_, _],
        bar = #{qux := 10}
    },
    World
) ->
    World.
```

#### `efmt` formatted code

`$ efmt foo.erl`
```erlang
-module(foo).


-spec hello(term(), integer()) ->
          {ok, integer()} |
          {error, Reason :: term()} |
          timeout.
hello({_,
       _,
       A,
       _,
       [B, _, C]},
      D) ->
    {ok, A + B +
         C + D};
hello(Error, X)
  when not is_integer(X);
       is_atom(X) ->
    {error, Error};
hello(#record{
        foo = [_, _],
        bar = #{qux := 10}
       },
      World) ->
    World.
```

### No line width limit

Unlike `erlfmt`, `efmt` doesn't provide a feature to ensure each line of the formatted code is within a specified line width (columns).

### Error handling

`erlfmt` seems to try formatting the remaining part of code even if it detected a syntax error.
In contrast, `efmt` aborts once it detects an error.

For instance, let's format the following code.
```erlang
-module(bar).

invalid_fun() ->
    : foo,
ok.

valid_fun
()->
ok.
```

Using `erlfmt`:
```console
$ erlfmt bar.erl
-module(bar).

invalid_fun() ->
    : foo,
ok.

valid_fun() ->
    ok.
bar.erl:4:5: syntax error before: ':'
// `valid_fun/0` was formatted and the program exited with 0 (success)
```

Using `efmt`:
```console
$ efmt bar.erl
[2021-11-28T11:30:06Z ERROR efmt] Failed to format "bar.erl"
    Parse failed:
    --> bar.erl:4:5
    4 |     : foo,
      |     ^ unexpected token

Error: Failed to format the following files:
- bar.erl
// The program exited with 1 (error)
```

### Macro handling

`efmt`, as much as possible, processes macros as the Erlang preprocessor does.

Thus, it can cover a wide range of tricky cases.
Let's format the following code which is based on a macro usage in [sile/jsone/src/jsone.erl](https://github.com/sile/jsone/blob/master/src/jsone.erl):
```erlang
-module(baz).

-ifdef('OTP_RELEASE').
%% The 'OTP_RELEASE' macro introduced at OTP-21,
%% so we can use it for detecting whether the Erlang compiler supports new try/catch syntax or not.
-define(CAPTURE_STACKTRACE, :__StackTrace).
-define(GET_STACKTRACE, __StackTrace).
-else.
-define(CAPTURE_STACKTRACE,).
-define(GET_STACKTRACE, erlang:get_stacktrace()).
-endif.

decode(Json, Options) ->
try
{ok, Value, Remainings} = try_decode(Json, Options),
check_decode_remainings(Remainings),
Value
catch
error:{badmatch, {error, {Reason, [StackItem]}}} ?CAPTURE_STACKTRACE ->
erlang:raise(error, Reason, [StackItem])
end.
```

Using `efmt`:
```console
$ efmt baz.erl
-module(baz).

-ifdef('OTP_RELEASE').
%% The 'OTP_RELEASE' macro introduced at OTP-21,
%% so we can use it for detecting whether the Erlang compiler supports new try/catch syntax or not.
-define(CAPTURE_STACKTRACE, :__StackTrace).
-define(GET_STACKTRACE, __StackTrace).
-else.
-define(CAPTURE_STACKTRACE, ).
-define(GET_STACKTRACE, erlang:get_stacktrace()).
-endif.

decode(Json, Options) ->
    try
        {ok, Value, Remainings} = try_decode(Json, Options),
        check_decode_remainings(Remainings),
        Value
    catch
        error:{badmatch, {error, {Reason, [StackItem]}}} ?CAPTURE_STACKTRACE->
            erlang:raise(error, Reason, [StackItem])
    end.
```

Using `erlfmt`:
```console
$ erlfmt baz.erl
baz.erl:6:29: syntax error before: ':'
-module(baz).

-ifdef('OTP_RELEASE').
%% The 'OTP_RELEASE' macro introduced at OTP-21,
%% so we can use it for detecting whether the Erlang compiler supports new try/catch syntax or not.
-define(CAPTURE_STACKTRACE, :__StackTrace).
-define(GET_STACKTRACE, __StackTrace).
-else.
-define(CAPTURE_STACKTRACE,).
-define(GET_STACKTRACE, erlang:get_stacktrace()).
-endif.

decode(Json, Options) ->
try
{ok, Value, Remainings} = try_decode(Json, Options),
check_decode_remainings(Remainings),
Value
catch
error:{badmatch, {error, {Reason, [StackItem]}}} ?CAPTURE_STACKTRACE ->
erlang:raise(error, Reason, [StackItem])
end.
baz.erl:19:50: syntax error before: '?'
```

### Formatting speed

The following benchmark compares the time to format all "*.erl" files contained in the OTP-24 source distribution.
```console
// OS and CPU spec.
$ uname -a
Linux TABLET-GC0A6KVD 5.10.16.3-microsoft-standard-WSL2 #1 SMP Fri Apr 2 22:23:49 UTC 2021 x86_64 x86_64 x86_64 GNU/Linux
$ cat /proc/cpuinfo | grep 'model name' | head -1
model name      : 11th Gen Intel(R) Core(TM) i7-1185G7 @ 3.00GHz

// Downloads OTP source code. There are 3,737 "*.erl" files.
$ wget https://erlang.org/download/otp_src_24.1.tar.gz
$ tar zxvf otp_src_24.1.tar.gz
$ cd otp_src_24.1/
$ find . -name '*.erl' | wc -l
3737

// Erlang version: Erlang/OTP 24 [erts-12.1] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit]

// erlfmt: 17.30s
$ time erlfmt (find . -name '*.erl') > /dev/null 2> /dev/null
________________________________________________________
Executed in   17.30 secs
   usr time   97.73 secs
   sys time   10.20 secs

// efmt: 5.84s
$ time efmt --parallel $(find . -name '*.erl') > /dev/null 2> /dev/null
________________________________________________________
Executed in    5.84 secs
   usr time   43.88 secs
   sys time    1.28 secs
```

### Development phase

`erlfmt` has released the stable version (v1), but `efmt` hasn't.
Perhaps some parts of the `efmt` style will change in future releases until it releases v1.

Limitations
-----------

There are some limitations that are not planned to be addressed in the future:
- Only supports UTF-8 files
- Doesn't process parse transforms
  - That is, if a parse transform has introduced custom syntaxes in your Erlang code, `efmt` could fail
- Doesn't process `-include().` and `-include_lib().` directives
  - Macros defined in those include files are expanded to (dummy) atoms.
