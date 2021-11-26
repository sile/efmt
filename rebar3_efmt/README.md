rebar3_efmt
===========

Erlang Code Formatter

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar3_efmt, {git_subdir, "https://host/user/efmt.git", "master", "."}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 efmt
    ===> Fetching efmt
    ===> Compiling efmt
    <Plugin Output>
