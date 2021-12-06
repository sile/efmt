Formatting Rules
================


TODO: https://github.com/sile/efmt/issues/3

This document describes the rules for how `efmt` formats Erlang texts.

Table of Contents
-----------------

- General Rules
  - [R001] Removes or inserts only whitespace tokens
  - [R002] Removes redundant whitespace tokens
  - [R003] Inserts a newline at an insertable point if the maximum line length is exceeded
  - [R004] Emacs Erlang mode friendly indentation
- Comments
  - [R005] Directive comments to enable or disable formatting
  - [R006] Inserts two spaces before a trailing comment
- Attributes, Declarations and Directives
- Expressions
- Types
- Macros

General Rules
-------------

### <a id="R001">[R001] Removes or inserts only whitespace tokens</a>
[R001]: #R001

`efmt` doesn't modify the text or order of visible tokens.
So it's guaranteed that the semantic meaning of the input file is preserved after formatting.

If you call the following `check/2` function for the before/after files, the result should always be `true`.
```erlang
-module(check).

-export([check/2]).

check(OriginalFile, FormattedFile) ->
    OriginalTokens = tokenize_file(OriginalFile),
    FormattedTokens = tokenize_file(FormattedFile),
    length(OriginalTokens) == length(FormattedTokens) andalso
        lists:all(
          fun ({{V, _}, {V, _}}) ->
                  %% Symbol or keyword.
                  true;
              ({{comment, _, V0}, {comment, _, V1}}) ->
                  %% Comment.
                  string:trim(V0, trailing) == V1;
              ({{T, _, V}, {T, _, V}}) ->
                  %% Other token.
                  true;
              (_) ->
                  false
          end,
          lists:zip(OriginalTokens, FormattedTokens)).

tokenize_file(File) ->
    {ok, Text} = file:read_file(File),
    {ok, Tokens, _} = erl_scan:string(binary_to_list(Text), 0, [return_comments]),
    Tokens.
```

### <a id="R002">[R002] Removes redundant whitespace tokens</a>
[R002]: #R002

- Trailing spaces are removed
- Two or more consecutive spaces are replaced with one space
- Three or more consecutive newlines are replaced with two newlines

#### Examples

```erlang
[aaa,   bbb].  % before
[aaa, bbb].  % after

%% before
aaa.


bbb.

%% after
aaa.

bbb.
```

Note that this rule only affects whitespace tokens.
That is, if redundant space or newline characters are a part of a string or character token, those are never modified (see also [R001]).

### <a id="R003">[R003] Inserts a newline at an insertable point if the maximum line length is exceeded</a>
[R003]: #R003

This rule exists not to exceed the maximum line length specified by `--print-width` option (defaults to `120`) as much as possible.

Where is "insertable point" depends on the item (e.g., list, function, ...) being formatted and will be described in the separate rules for each item.

Note that there are some exceptions to this rule:
- Comments are ignored when counting line length
- Doesn't insert a newline if it's meaningless in terms of line length

Please also see the following example (`--print-width 20` is assumed):
```erlang
%%---10---%%---20---
[1, 2, 3, 4, 5, 6, 7, 8].  % NG: Should insert a newline.
[1, 2, 3, 4, 5, 6].  % This is OK.

%% Even if a newline is inserted after `=` with 4 spaces indentation,
%% the line length isn't shortened (i.e., it's meaningless).
A = "a too long string".
```

### <a id="R004">[R004] Emacs Erlang mode friendly indentation</a>
[R004]: #R004

`efmt` follows the indentation style of [the Official Emacs Erlang mode](https://www.erlang.org/doc/apps/tools/erlang_mode_chapter.html).

Comments
--------

### <a id="R005">[R005] Directive comments to enable or disable formatting</a>
[R005]: #R005

`efmt` skips formatting a region that starts with a `@efmt:off` comment and ends with a `@efmt:on` comment.

#### Examples

```erlang
foo() ->
    %% @efmt:off
    LargeList =
      [1,2,3,...,
       998,999,1000],
    %% @efmt:on

    bar(LargeList).
```

### <a id="R006">[R006] Inserts two spaces before a trailing comment</a>
[R006]: #R006

#### Examples

```erlang
%% Before
foo.% bar
baz.    % qux

%% After
foo.  % bar
baz.  % qux
```

Attributes, Declarations and Directives
---------------------------------------

- `-define`
- `-record`
- `-type` or `-opaque`
- `-spec` or `-callback`
- Function declaration
- Other attributes

Expressions
-----------

- string concatenation

Types
-----

Macros
------

