Formatting Rules
================


TODO: https://github.com/sile/efmt/issues/3

This document describes the rules for how `efmt` formats Erlang texts.



General Rules
-------------

### <a id="R001">[R001] Removes or inserts only whitespace tokens</a>
[R001]: #R001

`efmt` doesn't modify the text or order of visible tokens to not change the semantic meaning of the input file.

That is, if you call the following `check/2` function for the before/after files, the result should always be `true`.
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

TODO
